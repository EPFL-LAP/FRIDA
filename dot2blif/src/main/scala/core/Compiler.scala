package core

import arch._
import readers._
import crkt._
import handshake._
import core._
import printers._
import analysis._
import archs._
import frontend.GlobalParamsInst
import packerv2._
import dummy._
import util.Util
import frontend.GlobalParams

import java.io.FileWriter
import java.io.FileReader
import java.io.File
import scala.io.Source
import io.AnsiColor._
import collection.mutable.{Set => MSet}
import collection.mutable.{Map => MMap}
import math.ceil
import sys.process._

case class CompilerState[T <: AbsTile](
  tiles: Map[String, T],
  crkt: ElasticGraph,
  archMasks: List[Arch],
  mols: List[ImplementedMolecule]
) {
  def placeArch: Arch = {
    archMasks
      .filter(_.place)
      .headOption
      .fold {
        scala.sys.error("Expected to have the place architecture.")
      }(
        pArch => pArch
      )
  }
}

object CompilerState {
  def getArchs(params: GlobalParamsInst): List[Arch] = {
    params.scArch.archs
  }

  def getTiles(params: GlobalParamsInst, archs: List[Arch]): Map[String, Tile] = {
    params.scArch.tiles.map {
      tileGen =>
        {
          val t = tileGen()
          LogicalTilePrinter.apply(t, params)

          (t.name, t)
        }
    }.toMap
  }

  def genCircuit(params: GlobalParamsInst): Unit = {
    println(params.mlirDir)

    ("mkdir -p " + params.mlirDir).!!

    val frontendScript = Seq(
      GlobalParams.frontendScript,
      params.benchSrcDir,
      params.mlirDir,
      params.bench,
      params.preBuf
    )

    println(frontendScript.mkString(" "))

    val canonScript = Seq(
      GlobalParams.canonScript,
      params.benchSrcDir,
      params.mlirDir,
      params.bench,
      params.preBuf,
      params.benchUnbufferedLoc
    )

    frontendScript.!
    canonScript.!
  }

  def readCrkt(params: GlobalParamsInst): ElasticGraph = {
    if (params.fromDot) {
      val genDot = DotSimpleGenericParser(Source.fromFile(params.dotPath))
      val g = DotToCrkt(genDot)

      val rewriters = List (
        RewriteCGRAMEConstants,
        RewriteInsertForks()
      )

      val canon = PatternRewriterPass(rewriters, g)

      DotPrinter(params.buildDir + "/lowering/parsed.dot")(AnnotateBasicBlocks(canon), Set())

      canon
    } else {
      genCircuit(params)
      println("mlir: " + params.benchUnbufferedLoc)

      val mlir = MLIRParser(params.benchUnbufferedLoc)
      val g = MlirToCrktConverter(params, mlir)

      DotPrinter(params.buildDir + "/lowering/parsed.dot")(AnnotateBasicBlocks(g), Set())

      g
    }
  }

  def readTiles(params: GlobalParamsInst): Map[String, Tile] = {
    val rawTiles = getTiles(params, getArchs(params))
    rawTiles.foreach(
      (_, t) => SanityCheckArchNames(t)
    )

    rawTiles.view
      .mapValues(
        (t: Tile) => SimplifyAbsArch(t)
      )
      .toMap
  }

  def frontEnd(params: GlobalParamsInst): CompilerState[Tile] = {
    val archs = getArchs(params)
    val g = readCrkt(params)
    val simpleTiles = readTiles(params)

    CompilerState(simpleTiles, g, archs, Nil)
  }

  def genArch(params: GlobalParamsInst): CompilerState[RootPb] = {
    // Ingore timing info
    val paramsNoTimings = params.withoutTimings

    // Build Tiles
    val tiles = buildTiles(params, readTiles(params))

    // println(tiles.map((tName, t) => tName + ":" + AreaExtractor.getIoInfo(t)).mkString("\n\n"))

    // Build Molecule Patterns
    ???
  }

  def buildTiles(params: GlobalParamsInst, rawTiles: Map[String, Tile]): Map[String, RootPb] = {
    println(CYAN + "Building tiles..." + RESET)
    val builtTiles = rawTiles.view
      .mapValues(
        (t: Tile) => PbTypeBuilder(params, t, false)
      )
      .toMap

    println(CYAN + "Removing useless modes..." + RESET)
    val simpleTiles = builtTiles.map(
      (tName, t) => (tName, SimplifyRedundantModes(t))
    )

    println(CYAN + "Make multiplexers explicit..." + RESET)
    val muxTiles = simpleTiles.map(
      (tName, t) => (tName, MuxExpliciter(t))
    )

    println(CYAN + "Canonicalize architecture..." + RESET)
    val canonTiles = muxTiles.map(
      (tName, t) => (tName, CanonicalizeArch(t))
    )

    println(CYAN + "Wireize architecture..." + RESET)
    val tiles = canonTiles.map(
      (tName, t) => (tName, WireizePb(params, t))
    )

    tiles.toMap
  }

  def legalizeCircuit(crkt: ElasticGraph, archs: List[Arch])(implicit params: GlobalParamsInst): ElasticGraph = {
    // TODO maybe add a rewrite patterns to merge some components: for example, consecutive forks

    val rewriters = List(
      RewriteDetectDontTouch,
      RewriteUnsupportedToIO(0, 0),
      RewriteIotoIo,
      RewriteDangling,
      RewriteCanonOEHBs,
      RewriteExstsiTrunc,
      RewriteWidth(archs),
      RewriteInconsistentWidth,
      RewriteIdentities,
      RewriteDangling
    )

    val canon = PatternRewriterPass(rewriters, crkt)
    DotPrinter(params.buildDir + "/lowering/legal.dot")(canon, Set())

    canon
  }

  def decoupleCircuit(crkt: ElasticGraph)(implicit params: GlobalParamsInst): ElasticGraph = {
    val hsCrkt = LowerToExplicitHandshakeCrkt(crkt)

    DotPrinter(params.buildDir + "/lowering/hs.dot")(hsCrkt, Set())

    val rewrites = List(
      WireizePass,
      RewriteWithoutZeroData,
      RewriteRemoveDuplicatedIO,
      RewriteFixupExit,
      RewriteDangling
    )

    val wireCrkt = PatternRewriterPass(rewrites, hsCrkt)
    DotPrinter(params.buildDir + "/lowering/wired.dot")(wireCrkt, Set())

    wireCrkt
  }

  def lowerCircuit(crkt: ElasticGraph, archs: List[Arch], loweredCircuit: Boolean)(implicit
      params: GlobalParamsInst
  ): ElasticGraph = {
    if (loweredCircuit) {
      crkt
    } else {
      val legal = legalizeCircuit(crkt, archs)
      decoupleCircuit(legal)
    }
  }

  def lowerVldRdy(cs: CompilerState[RootPb]): CompilerState[RootPb] = {
    val nTiles = cs.tiles.map(
      (tName, t) => (tName, LowerExplicitValidReady(t))
    )
    val nPackedNetlist = cs.mols.map(ImplementedMoleculeToExplicitVldRdy(_, cs.crkt))
    val nCrkt = LowerToExplicitVldRdy(cs.crkt)

    CompilerState(nTiles, nCrkt, cs.archMasks, nPackedNetlist)
  }

  def insertDummies(params: GlobalParamsInst, cs: CompilerState[RootPb]): CompilerState[RootPb] = {
    println(CYAN + "Insert dummies..." + RESET)

    val dummyTiles = cs.tiles.map(
      (tName, t) => (tName, InsertDummiesArch(params, t).asInstanceOf[RootPb])
    )
    val dummyCrkt = InsertDummiesCrkt(params, cs.crkt, cs.mols)
    val dummyMappings = cs.mols.map(InsertDummiesImplementedMolecule(params, _, dummyCrkt))

    DotPrinter(params.buildDir + "/lowering/dummy.dot")(dummyCrkt, Set())

    CompilerState(dummyTiles, dummyCrkt, cs.archMasks, dummyMappings)
  }

  def insertMissingArchDelays(params: GlobalParamsInst, cs: CompilerState[RootPb]): CompilerState[RootPb] = {
    val delayTiles = cs.tiles.map {
      (tName, t) =>
        {
          (tName, AddConnectionBlockDelay(AddInterconnectDelay(t, params), cs.archMasks, params))
        }
    }

    CompilerState(delayTiles, cs.crkt, cs.archMasks, cs.mols)
  }

  // TODO rename to apply
  def defaultPipeline(
      params: GlobalParamsInst
  ): Option[CompilerState[RootPb]] = {
    implicit val p = params

    // parse input files
    val parsed = frontEnd(params)

    // Lower Circuit
    val crkt = lowerCircuit(parsed.crkt, parsed.archMasks, false)
    DotPrinter(params.buildDir + "/lowering/lowered.dot")(crkt, Set())

    // Build Tiles
    val tiles = buildTiles(params, parsed.tiles)

    // Construct Compiler State Object
    val csHs = CompilerState(tiles, crkt, parsed.archMasks, Nil)

    // Pack
    println(CYAN + "Packing..." + RESET)
    val csPackedOpt = Packer(params, csHs, parsed.tiles)
    csPackedOpt.map {
      csPacked =>
        {
          // save packing utilization
          println(CYAN + "Saving primitive utilization..." + RESET)
          UtilizationReport.savePrimitiveUtilization(params, csPacked.mols)

          // Print without dummy blocks
          println(CYAN + "Printing final circuit..." + RESET)
          val plainCrkt = Legalizer.extractCrkt(csPacked.mols.map(_.mm.get.m))
          val fullCrkt = AnnotateMolecules(plainCrkt, csPacked.mols.map(_.mm.get))
          val feedbackEdges = csPacked.mols.map(_.mm.get).map(_.feedbackEdges).flatten.toSet

          // println("feedbacks: " + feedbackEdges.mkString("\n"))
          DotPrinter(params.buildDir + "/full.dot")(fullCrkt, feedbackEdges)

          println(CYAN + "Lower to explicit valid and ready..." + RESET)
          val loweredCs = lowerVldRdy(csPacked)

          val dummyCs = if (params.timings) {
            insertDummies(params, loweredCs)
          } else {
            loweredCs
          }

          insertMissingArchDelays(params, dummyCs)
        }
    }
  }
}
