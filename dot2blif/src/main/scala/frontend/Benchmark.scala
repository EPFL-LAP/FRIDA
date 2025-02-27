package frontend

import core._
import arch._
import printers._
import rrggen._
import routegen._
import packerv2._
import analysis.GridInfoExtractor
import analysis.GlobalTimingReports
import analysis.LocalReports
import archs.ArchDef
import analysis.UtilizationReport
import verif.Simulate

import sys.process._
import io.AnsiColor._
import java.io.File
import java.io.FileOutputStream
import scala.concurrent.*
import ExecutionContext.Implicits.global
import scala.util.Failure
import scala.util.Success
import scala.concurrent.duration.Duration

object Benchmark extends Tool {
  import VprUtils._

  val name = "bench"

  // Floating Point Kernels:
  //   - get_tanh
  //   - syr2k_float
  //   - gemver_float
  //   - histogram
  //   - atax
  //   - kernel_2mm_float
  //   - bicg float
  //   - kernel_3mm_float
  //   - gesummv_float
  //   - symm_float
  //   - correlation_float
  //   - gsumif
  //   - covariance_float
  //   - cordic
  //   - float_basic
  //   - atax_float
  //   - gemm_float
  //   - if_loop_add
  //   - if_loop_mul
  //   - matching_2
  //   - gsum
  //   - matching

  // Cannot converge to consistent bitwidths:
  //   - Sobel

  // Control merge of size 2
  //   - Insertion Sort
  //   - Kmp

  // Div in:
  //   - Covariance
  //   - if_loop_3
  //   - ComplexDiv
  //   - trisolv

  // Not enough routing resources:
  //   - cnn with overused RRs: 30?
  //   - gemver with overused RRs: 2

  // Dynamatic Errors:
  //    - Sharing -> Seems something specific for resource sharing, not the same structure as other benchmarks
  //    - Memory -> Seems to be a bunch of tests specifically related to memory.
  //    - ExternalIntegration -> unit test for something else
  //    - Spmv -> out of bound load during profiling?
  //    - path_profiling -> failed to legalize to handshake
  //    - Complex div -> Do not support division? In any case fail to lower to handshake
  //    - gemm -> Failed to compile to handshake, sitofp

  // Too simple:
  //    - simple_example_1: is just a fork to constants...

  val discarded = Set(
    "sharing",
    "memory",
    "insertion_sort",
    "dct",
    "sobel",
    "get_tanh",
    "syr2k_float",
    "gemver_float",
    "histogram",
    "atax",
    "kernel_2mm_float",
    "bicg float",
    "kernel_3mm_float",
    "gesummv_float",
    "symm_float",
    "correlation_float",
    "gsumif",
    "covariance_float",
    "cordic",
    "float_basic",
    "atax_float",
    "gemm_float",
    "if_loop_add",
    "if_loop_mul",
    "matching_2",
    "gsum",
    "spmv",
    "kmp",
    "external_integration",
    "matching",
    "simple_example",
    "path_profiling",
    "complexdiv",
    "gemm",
    // Too simple,
    "simple_example_1",
    // Have division
    "covariance",
    "trisolv",
    "if_loop_3",
    // Unsufficient RRs
    "cnn",
    "triangular" // , "gemver",
    // working but do not want to run twice...
    // "bicg", "binary_search","fir", "gaussian", "gcd", "if_loop_1", "if_loop_2", "iir", "image_resize",
    // "jacobi_1d_imper", "kernel_2mm", "kernel_3mm", "loop_array", "matrix", "matrix_power", "matvec", "mul_example",
    // "pivot", "polyn_mult", "stencil_2d", "sumi3_mem", "threshold", "triangular", "vector_rescale", "video_filter",
    // "while_loop_1", "while_loop_2", "while_loop_3"

    // Needing more extensive sweeps
    // "threshold", "gemver", "binary_search"
  )

  val benchList = {
    new File(GlobalParams.integrationTestPath).listFiles
      .map(_.toString())
      .map(_.split("/").last)
      .filter(!_.contains("float"))
      .filter(!_.contains("."))
      .toList
      .filter(!discarded.contains(_))
      .sorted
  }

  val cgrameDiscarded = Set[String]()

  val cgrameBenchList = {
    new File(GlobalParams.dotPath).listFiles
      .map(_.toString().replace(".dot", ""))
      .map(_.split("/").last)
      .toList
      .filter(!cgrameDiscarded.contains(_))
      .sorted
  }

  val bufDefaultObj = 2

  def split(params: GlobalParamsInst, cs: CompilerState[RootPb]): Unit = {
    println(CYAN + "Splitting placement according to arch masks..." + RESET)
    PlaceSplitter(params, cs.archMasks, cs.mols)

    println(CYAN + "Keeping same architectural layout..." + RESET)
    ToFixedLayout(params, cs.archMasks)
  }

  // def baseRun(params: GlobalParamsInst): (CompilerState[Tile], CompilerState[RootPb]) = {
  //   val parsedCs = CompilerState.frontEnd(params, false, false)
  //   val cs = CompilerState.defaultPipeline(params)

  //   (parsedCs, cs)
  // }

  def fullRun(params: GlobalParamsInst): Unit = {
    val parsedCs = CompilerState.frontEnd(params)
    val csOpt = CompilerState.defaultPipeline(params)

    csOpt.map {
      cs =>
        {
          ArchPrinter(params, cs.tiles, cs.archMasks, None)
          BlifPrinter(params, cs.crkt, cs.archMasks, cs.mols, cs.tiles.map(_._2).toList)
          PackPrinter(params, cs.mols, cs.archMasks, cs.crkt, cs.tiles.map(_._2).toList)

          println(CYAN + "Place without timings.." + RESET)

          // Runs vpr once without timings to get the RRG of each architecture
          val placeArch = cs.placeArch
          place(params.withoutTimings, placeArch, VPRPlace)

          split(params, cs)

          println(CYAN + "route no timing.." + RESET)
          route(params.withoutTimings, cs.archMasks)

          if (params.timings) {
            // Extract architecture properties, maybe only want to keep the SbInfo???
            val gridInfo = GridInfoExtractor(params, cs.archMasks, cs.tiles)

            ArchPrinter(params, cs.tiles, cs.archMasks, Some(gridInfo))

            println(CYAN + "place with timing.." + RESET)
            place(params, placeArch, VPRPlace)

            split(params, cs)

            println(CYAN + "route with timing.." + RESET)
            route(params, cs.archMasks)

            RRGUnifier(params, cs.archMasks, cs.tiles, Some(gridInfo))

            println(CYAN + "route unified with timing.." + RESET)
            routeUnified(params, cs.archMasks)
          }

          cs
        }
    }

  }

  def cacheRRG(params: GlobalParamsInst): Unit = {
    val dotName = params.bench // "fir"

    val mlir = GlobalParams.root + "/build/temp/mlir/" + dotName + ".mlir"

    val rrgParams = GlobalParamsInst(
      params.scArch,
      dotName,
      GlobalParams.root + "/build/temp/",
      GlobalParams.root + "/build/temp/mlir/",
      GlobalParams.root + "/build/temp/",
      true,
      false,
      true,
      1,
      10 :: Nil, // Buffer placement quality is irrelevent here...
      VPRParams.default,
      true,
      true,
      params.parallel,
      false,
      params.fromDot,
      All,
      () => ""
    )

    fullRun(rrgParams)

    Seq("mkdir", "-p", GlobalParams.rrgLib).!!

    Seq(
      "cp",
      GlobalParams.root + "/build/temp/vpr/rrg_all.xml",
      GlobalParams.rrgLib + "/" + params.scArch.toString() + "all.xml"
    ).!

    params.scArch.archs.filter(!_.place).map {
      arch =>
        {
          Seq(
            "cp",
            GlobalParams.root + "/build/temp/vpr/rrg_" + arch.name + ".xml",
            GlobalParams.rrgLib + "/" + params.scArch.toString() + arch.name + ".xml"
          ).!
        }
    }
  }

  def cachedRRG(params: GlobalParamsInst): Boolean = {
    new java.io.File(GlobalParams.rrgLib + "/" + params.scArch.toString() + "all.xml").exists
  }

  def moveRRG(params: GlobalParamsInst): Unit = {
    ("mkdir -p " + params.buildDir + "/vpr/").!

    Seq(
      "cp",
      GlobalParams.rrgLib + "/" + params.scArch.toString() + "all.xml",
      params.buildDir + "/vpr/rrg_all.xml"
    ).!

    params.scArch.archs.filter(!_.place).map {
      arch =>
        {
          Seq(
            "cp",
            GlobalParams.rrgLib + "/" + params.scArch.toString() + arch.name + ".xml",
            params.buildDir + "/vpr/rrg_" + arch.name + ".xml"
          ).!
        }
    }
  }

  def runRec(
      params: GlobalParamsInst,
      csHs: CompilerState[RootPb],
      rrgs: Seq[RRG],
      bufferedMatchesOpt: Option[Seq[MappedMolecule[Molecule]]]
  ): Unit = {
    bufferedMatchesOpt.map {
      bufferedMatches =>
        {
          val brokenForkMatches = Packer.legalizeForks(params, rrgs, bufferedMatches)
          val csPacked = Packer.implementPacking(params, csHs, brokenForkMatches)

          // save packing utilization
          println(CYAN + "Saving primitive utilization..." + RESET)
          UtilizationReport.savePrimitiveUtilization(params, csPacked.mols)

          // Print without dummy blocks
          println(CYAN + "Printing final circuit..." + RESET)
          val plainCrkt = Legalizer.extractCrkt(csPacked.mols.map(_.mm.get.m))
          val fullCrkt = AnnotateMolecules(plainCrkt, csPacked.mols.map(_.mm.get))
          val feedbackEdges = csPacked.mols.map(_.mm.get).map(_.feedbackEdges).flatten.toSet

          if(params.mode != frontend.Pack) {
            // println(CYAN + "Simulating the circuit..." + RESET)
            // Simulate(params, fullCrkt, csPacked.archMasks)

            println("feedbacks: " + feedbackEdges.mkString("\n"))
            DotPrinter(params.buildDir + "/full.dot")(fullCrkt, feedbackEdges)

            if (params.mode != frontend.Sim) {
              println(CYAN + "Lower to explicit valid and ready..." + RESET)
              val loweredCs = CompilerState.lowerVldRdy(csPacked)

              val dummyCs = if (params.timings) {
                CompilerState.insertDummies(params, loweredCs)
              } else {
                loweredCs
              }

              val cs = CompilerState.insertMissingArchDelays(params, dummyCs)

              // ArchPrinter(params, cs.tiles, cs.archMasks, None)
              BlifPrinter(params, cs.crkt, cs.archMasks, cs.mols, cs.tiles.map(_._2).toList)
              PackPrinter(params, cs.mols, cs.archMasks, cs.crkt, cs.tiles.map(_._2).toList)

              moveRRG(params)
              val gridInfo = GridInfoExtractor(params, cs.archMasks, cs.tiles)

              ArchPrinter(params, cs.tiles, cs.archMasks, Some(gridInfo))
              ArchitectureLegalizer(cs.archMasks, cs.tiles, params, Some(gridInfo))

              val placeArch = cs.placeArch

              val placeStr = if(params.timings) {
                "place with timings..."
              } else {
                "place without timings"
              }

              println(CYAN + placeStr + RESET)
              place(params, placeArch, VPRPlaceFull)

              val routeStr = if(params.timings) {
                "route unified with timings..."
              } else {
                "route unified without timings"
              }

              println(CYAN + routeStr + RESET)
              routeUnified(params, cs.archMasks)

              if(params.timings) {
                LocalReports(cs, params, gridInfo)
              }
            }
          }
        }
    }
  }

  def expandParams(params: GlobalParamsInst): List[GlobalParamsInst] = {
    params.cp.map {
      cp =>
      {
        val cpStr = f"_cp$cp%1.2f"
        val seedStr = "seed_" + params.seed

        val nBuild = params.buildDir.replace("base", "") + seedStr + cpStr + "/"
        val nAnalysis = nBuild + "/csv/"

        GlobalParamsInst (
          params.scArch,
          params.bench,
          nBuild,
          params.mlirDir,
          nAnalysis,
          params.withBuffers,
          params.enableDisplay,
          params.timings,
          params.seed,
          cp :: Nil,
          params.vprSettings,
          params.forcePack,
          params.forceRun,
          params.parallel,
          params.fpga,
          params.fromDot,
          params.mode,
          params.config
        )
      }
    }
  }

  // TODO allow only packing, only placement, and all
  // TODO we do not rely on defaultPipeline for speed, but maybe there is a better way to organize everything...
  def run(params: GlobalParamsInst): Unit = {
    println(
      YELLOW + " -> " + RESET
        + "mapping " + GREEN + params.bench + RESET
        + " onto " + CYAN + params.scArch.name + RESET
        + " with seed " + MAGENTA + params.seed + RESET + "."
    )

    if (params.mode == Vis) {
      expandParams(params).map {
        nParams => {
          // TODO make all paths available in GlobalParams directly...
          val simFName = (nParams.buildMlirDir + "/" + nParams.bench + "_sim.mlir").replace("//", "/")
          val dotFName = (nParams.buildMlirDir + "/" + nParams.bench + "_sim.dot").replace("//", "/")
          val wlfFName = (nParams.buildSimDir + "/HLS_VERIFY/vsim.wlf").replace("//", "/")

          val visualizeScript = Seq (
            GlobalParams.visualizeScript,
            simFName,
            dotFName,
            wlfFName,
            nParams.buildDir + "/visual/",
            nParams.bench
          )

          visualizeScript.!
        }
      }
    } else {
      // TODO it would be nice to have one option in the frontend to generate the dots and the timing
      // TODO representation
      DynamaticTimingPrinter(params)

      if (!cachedRRG(params) && !(params.mode == PackNoBuffer)) {
        cacheRRG(params)
      }

      val parsed = CompilerState.frontEnd(params)

      // Lower Circuit
      val crkt = CompilerState.lowerCircuit(parsed.crkt, parsed.archMasks, false)(params)
      DotPrinter(params.buildDir + "/lowering/lowered.dot")(crkt, Set())

      // Build Tiles
      val tiles = CompilerState.buildTiles(params, parsed.tiles)

      // Construct Compiler State Object
      val csHs = CompilerState(tiles, crkt, parsed.archMasks, Nil)

      // Pack
      println(CYAN + "Packing..." + RESET)
      val rrgs = csHs.tiles
        .map(_._2)
        .map(
          pb => RRG.gen(pb, parsed.tiles(pb.name), params)
        )
        .toSeq
      rrgs.foreach(RRGPrinter(params, _))

      val plainMatches = Packer.plainPacking(params, csHs, rrgs)

      if (params.mode != PackNoBuffer) {
        val allParams = expandParams(params)

        if (params.parallel && (allParams.size > 1)) {
          val backendRuns = Future.sequence(allParams.map {
            nParams =>
              {
                val f = Future {
                  val logF = new File("/dev/null")
                  val log = new FileOutputStream(logF)

                  val bufferedMatchesOpt = Packer.bufferedPacking(nParams, csHs, rrgs, plainMatches)

                  Console.withOut(log) {
                    

                    runRec(nParams, csHs, rrgs, bufferedMatchesOpt)
                  }

                  log.close()
                }

                f.onComplete {
                  case Failure(exception) => exception
                  case Success(cs)        => cs
                }

                f
              }
          })

          // TODO this should be an onComplete and the rest is passed as a callback...
          Await.result(backendRuns, Duration.Inf)
        } else {
          allParams.map {
            nParams => {
              val bufferedMatchesOpt = Packer.bufferedPacking(nParams, csHs, rrgs, plainMatches)

              runRec(nParams, csHs, rrgs, bufferedMatchesOpt)
            }
          }
        }
      }

    }
  }

  // TODO allow some sweeps on VPR Parameters
  def parseArguments(args: Seq[String]): List[GlobalParams] = {
    val archName = getArgVal(args, "arch=")
    val dotName = getArgValOpt(args, "dot=")
    val cp = getCompositeArgValOpt(args, "cp=").fold(2.0 :: Nil)(_.map(_.toDouble))

    val forceRun = getArgValOpt(args, "plot").fold(true)(
      _ => false
    )

    val forcePack = getArgValOpt(args, "forcePack").fold(false)(
      _ => true
    )

    val disp = getArgValOpt(args, "disp").fold(false)(
      _ => true
    )

    val parallel = getArgValOpt(args, "parallel").fold(false)(
      _ => true
    )

    val fromDot = getArgValOpt(args, "fromDot").fold(false)(
      _ => true
    )

    val v2 = getArgValOpt(args, "packerv2").fold(false)(
      _ => true
    )

    val sim = getArgValOpt(args, "simulate").fold(false)(
      _ => true
    )

    val timings = getArgValOpt(args, "timings").fold(false)(
      _ => true
    )

    val placeRlimEscape = getArgValOpt(args, "placeRlimEscape=").fold(VPRParams.defRlim)(_.toDouble)
    val placeInnerNum = getArgValOpt(args, "placeInnerNum=").fold(VPRParams.defPlaceInnerNum)(_.toInt)
    val routerIterations = getArgValOpt(args, "routerIterations=").fold(VPRParams.defRouterIterations)(_.toInt)

    val placePinCost = getArgValOpt(args, "placePinCost=").fold(VPRParams.defPlacePinCost) {
      case "true"  => true
      case "false" => false
    }

    val placeModel = getArgValOpt(args, "placeModel=").fold(VPRParams.defPlaceDelayModel) {
      case DeltaMatrix.str      => DeltaMatrix
      case DeltaSidesMatrix.str => DeltaSidesMatrix
    }

    val mode = getArgValOpt(args, "mode=").fold(All) {
      case PackNoBuffer.str => PackNoBuffer
      case Pack.str         => Pack
      case Sim.str          => Sim
      case Vis.str          => Vis
      case Place.str        => Place
      case Route.str        => Route
      case All.str          => All
    }

    val vprParams = VPRParams(mode, placeModel, routerIterations, placePinCost, placeInnerNum, placeRlimEscape, v2)

    val baseDir = GlobalParams.defaultBaseDir(archName)

    if (dotName.isEmpty) {
      val benchs = if (fromDot) {
        cgrameBenchList
      } else {
        benchList
      }

      println(benchs.mkString(", "))

      GlobalParams(
        archName,
        benchs,
        baseDir,
        cp,
        enableDisplay = disp,
        forceRun = forceRun,
        forcePack = forcePack,
        vprSettings = vprParams :: Nil,
        parallel = parallel,
        fromDot = fromDot,
        mode = mode,
        timings = timings
      ) :: Nil
    } else {
      GlobalParams(
        archName,
        dotName.get :: Nil,
        baseDir,
        cp,
        enableDisplay = disp,
        forceRun = forceRun,
        forcePack = forcePack,
        vprSettings = vprParams :: Nil,
        parallel = parallel,
        fromDot = fromDot,
        mode = mode,
        timings = timings
      ) :: Nil
    }
  }

  def aggregate(params: Seq[GlobalParamsInst]): Unit = {
    // TOOD Only keep GlobalTimingReports for now...
    // TODO probably only report basic stuff such as CP and area, per benchmark
    // TODO probably only report the seed so far...

    // LocalTimingReports(params)
    // GlobalTimingReports(params)
  }
}
