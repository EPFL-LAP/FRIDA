package verif

import crkt._
import arch.Arch
import frontend.GlobalParamsInst
import printers.MLIRPrinter
import printers.DotPrinter
import core.PatternRewriterPass
import readers.MLIRParser
import arch.PTOutput
import arch.PMCond
import arch.PTInput
import archs.TEHB
import archs.OEHB
import archs.Fork
import archs.ForkParams
import archs.Mux
import archs.Constant
import mlir._
import frontend.GlobalParams

import sys.process._
import scala.xml.transform.RewriteRule
import archs.Branch

// Assume JSON with Chisel generator calls has already been generated

object Simulate {
  def crktWithMemories(params: GlobalParamsInst, archs: List[Arch]): ElasticGraph = {
    val parsed = MlirToCrktConverter(params, MLIRParser(params.benchUnbufferedLoc))

    // Similar frontend as usual, just keep the memories / end primitives

    val rewriters = List (
      RewriteCanonOEHBs,
      RewriteExstsiTrunc,
      RewriteWidth(archs),
      RewriteInconsistentWidth,
      RewriteIdentities,
      RewriteDangling
    )

    PatternRewriterPass(rewriters, parsed)
  }

  // Only follow buffer / identity primitives
  // TODO also insert back attached constants, etc...
  def followPath(from: Option[Port], to: Option[Port], g: ElasticGraph): List[Port] = {
    def rec(loc: Port, acc: List[Port]): List[Port] = {
      val reachedTo = from.isDefined && to.fold(g.nodes(loc.thisNode).nType.isIo)(_ == loc)
      val reachedFrom = to.isDefined && from.fold(g.nodes(loc.thisNode).nType.isIo)(_ == loc)

      if(reachedTo) {
        to.fold(acc.reverse)(toP => (toP :: acc).reverse)
      } else if(reachedFrom) {
        from.fold(acc)(fromP => (fromP :: acc))
      } else {
        val n = g.nodes(loc.thisNode)
        val flippedP = n.ports(loc.id.flipped()).head

        rec(flippedP.distPorts.head._2, flippedP :: loc :: acc)
      }
    }

    if(from.isDefined) {
      rec(from.get.distPorts.head._2, from.get :: Nil)
    } else {
      rec(to.get.distPorts.head._2, to.get :: Nil)
    }
  }

  def getCorrespondingPort(p: Port, g: ElasticGraph): Option[Port] = {
    if(g.nodes.contains(p.thisNode)) {
      g.nodes(p.thisNode).ports(p.id).filter(_.loc == p.loc).headOption
    } else {
      None
    }
  }

  def findPath(from: Port, to: Port, g: ElasticGraph): List[Port] = {
    val srcPort = getCorrespondingPort(from, g)
    val dstPort = getCorrespondingPort(to, g)

    if(srcPort.isEmpty && dstPort.isEmpty) {
      Nil
    } else {
      followPath(srcPort, dstPort, g)
    }
  }

  def findIdentityConstants(n: TNode, g: ElasticGraph): List[Node] = {
    if(n.name.contains("identity")) {
      val idCsts = n.ports.map(_._2).flatten.filter(_.id.pmw.pm.isInstanceOf[PMCond]).map {
        p => {
          val cstOut = p.distPorts.head._2
          val cst = g(cstOut.thisNode)

          val srcOut =  cst.ports.filter(_._1.pt == PTInput).head._2.head.distPorts.head._2
          val src = g(srcOut.thisNode)

          cst :: src :: Nil
        }
      }

      n :: idCsts.flatten.toList
    } else {
      n :: Nil
    }
  }

  def fixupBrokenForks(raised: ElasticGraph, withMemories: ElasticGraph): ElasticGraph = {
    def rec(nextPs: List[Port], acc: List[TNode]): List[TNode] = {
      val p = nextPs.head

      if(withMemories.nodes.contains(p.thisNode)) {
        val memN = withMemories(p.thisNode)
        val memP = memN.ports(p.id).filter(_.loc == p.loc).head

        memP.distPorts = p.distPorts

        if(nextPs.tail.isEmpty) {
          acc
        } else {
          rec(nextPs.tail, acc)
        }
      } else {
        val n = raised(p.thisNode)
        val recPs = n.ports(p.id.flipped()).map(_.distPorts.map(_._2)).flatten ++ nextPs.tail

        rec(recPs, n :: acc)
      }
    }

    val nNodes = withMemories.nodes.map(_._2).map {
      n => {
        n.nType match {
          case Fork(p) => {
            val nR = raised(n.name)

            if(p.num != nR.nType.p.asInstanceOf[ForkParams].num) {
              val nOutPorts = nR.ports.filter(_._1.pt == PTOutput)
              val nInPorts = n.ports.filter(_._1.pt == PTInput)

              val nPorts = nOutPorts ++ nInPorts
              val nNode = Node(n.name, nR.nType, nR.mlirAttr, nR.attr, nR.annos, nPorts)

              val nNodes = nOutPorts.map(_._2).flatten.map {
                p => {
                  rec(p.distPorts.map(_._2).toList, nNode :: Nil)
                }
              }.flatten

              nNodes
            } else {
              n :: Nil
            }
          }

          case others => n :: Nil
        }
      }
    }.flatten.toList

    val nNodesWithIdCsts = nNodes.map(findIdentityConstants(_, raised)).flatten.toList

    ElasticGraph(nNodesWithIdCsts)
  }

  def extendCrkt(raised: ElasticGraph, withMemories: ElasticGraph): ElasticGraph =  {
    val nNodes = withMemories.nodes.map(_._2).map(_.ports.map(_._2).flatten.filter(_.id.pt == PTOutput)).flatten.map {
      p => {
        val dps = p.distPorts
        assert(dps.size == 1)

        val dp = dps.head._2
        val e = ElasticEdge(p, dp)

        if(!raised.contains(e)) {
          val path = findPath(p, dp, raised)

          if(path.nonEmpty) {
            val startPath = if(path.head.thisNode == p.thisNode) 1 else 0
            val endPath = if(path.last.thisNode == dp.thisNode) 1 else 0

            if(path.drop(startPath).dropRight(endPath).nonEmpty) {
              val innerPath = path.drop(startPath).dropRight(endPath)

              val nNodes = innerPath.map(_.thisNode).toSet.map(raised(_)).map(findIdentityConstants(_, raised)).flatten
                
              val nDP = innerPath.head
              assert(p != nDP, path)

              p.distPorts = Map((nDP.nodeID() -> nDP))
              nDP.distPorts = Map((p.nodeID() -> p))

              val lDP = innerPath.last
              assert(dp != lDP, path)

              dp.distPorts = Map((lDP.nodeID() -> lDP))
              lDP.distPorts = Map((dp.nodeID() -> dp))

              nNodes.toSeq
            } else {
              Nil
            }
          } else {
            Nil
          }
        } else {
          Nil
        }
      }
    }.flatten

    val allNodes = (withMemories.nodes.map(_._2) ++ nNodes).map(n => (n.name, n)).toMap

    ElasticGraph(allNodes)
  }

  val requiredMlirAttrs = Set("handshake.name")

  def requiredMlirAttrsFor(n: TNode): Set[String] = {
    n.nType match {
      case TEHB(p) => requiredMlirAttrs + "hw.parameters"
      case OEHB(p) => requiredMlirAttrs + "hw.parameters"
      case Constant(p) => requiredMlirAttrs + "value"
      case other => requiredMlirAttrs
    }
  }

  def fixupMlirAttrs(g: ElasticGraph): ElasticGraph = {
    val nNodes = g.nodes.map(_._2).map {
      n => {
        if(requiredMlirAttrsFor(n).forall(n.mlirAttr.contains(_))) {
          n
        } else {
          val nMlirAttrs = requiredMlirAttrsFor(n).filter(!n.mlirAttr.contains(_)).map {
            case "handshake.name" => ("handshake.name" -> AttrString(n.name))
            case "value" => {
              assert(n.nType.isInstanceOf[Constant] && n.name.contains("identity"))

              val nextNode = g(n.ports.map(_._2).flatten.filter(_.id.pt == PTOutput).head.distPorts.head._2.thisNode)

              nextNode.nType match {
                case Branch(_) => ("value" -> AttrBoolean(true))
                case Mux(_) => ("value" -> AttrBoolean(false))
                case other => scala.sys.error("Unexpected node type.")
              }
            }

            case "hw.parameters" => {
              n.nType match {
                case TEHB(p) => ("hw.parameters" -> BufConfigAttr(p.depth, false, false, true))
                case OEHB(p) => ("hw.parameters" -> BufConfigAttr(p.depth, true, true, false))
                case other => scala.sys.error("Expected hw.parameters on: " + other)
              }
            }

            case other => scala.sys.error("Unsupported mlir attribute: " + other)
          }

          Node(n.name, n.nType, n.mlirAttr ++ nMlirAttrs, n.attr, n.annos, n.ports)
        }
      }
    }.toList

    ElasticGraph(nNodes)
  }

  def apply(params: GlobalParamsInst, g: ElasticGraph, archs: List[Arch]): Unit = {
    val rewriters = List (
      RaiseToImpl//,
      // RewriteIdentities
    )

    val raised = PatternRewriterPass(rewriters, g)
    DotPrinter(params.buildDir + "/lowering/raised.dot")(raised, Set())

    val withMemories = crktWithMemories(params, archs)
    DotPrinter(params.buildDir + "/lowering/memories.dot")(withMemories, Set())

    val bFork = fixupBrokenForks(raised, withMemories)
    DotPrinter(params.buildDir + "/lowering/bFork.dot")(bFork, Set())

    val noMol = bFork.nodes.map(_._2).filter(_.attr.contains("mol"))
    println(noMol.size)

    val sim = fixupMlirAttrs(extendCrkt(raised, bFork))
    DotPrinter(params.buildDir + "/lowering/sim.dot")(sim, Set())

    val finalRewrites = List (
      RewriteFixupIdentities
    )

    val simLegal = PatternRewriterPass(finalRewrites, sim)
    DotPrinter(params.buildDir + "/lowering/sim_legal.dot")(simLegal, Set())

    val mlir = CrktToMlirConverter(params, simLegal)

    val simFName = params.buildMlirDir + "/" + params.bench + "_sim.mlir"
    val hwFName = params.buildMlirDir + "/" + params.bench + "_hw.mlir"

    MLIRPrinter(simFName, mlir)

    val hdlScript = Seq(
      GlobalParams.writeHDLScript,
      params.buildHdlDir,
      params.bench,
      simFName,
      hwFName,
      "verilog"
    )

    hdlScript.!

    val simScript = ("bash -c \""
      + "cd " + GlobalParams.root + "; "
      + "source env.env ; "
      + "cd " + params.buildSimDir + " ; "
      + GlobalParams.simulateScript + " "
      + params.buildHdlDir + " "
      + params.buildSimDir + " "
      + params.benchSrcDir + " "
      + params.bench + " "
      + "\"")

    println(simScript)

    simScript.!
  }
}

