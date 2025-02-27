package printers

import frontend.GlobalParamsInst
import packerv2._
import arch.BlockPortID
import arch.PTInput
import util.Util
import arch.Impl
import arch.D
import arch.Hs
import arch.Vld
import arch.Rdy
import arch.Dummy
import arch.Regular
import arch.DagId
import core.ATileIo
import archs.Fork
import archs.ForkParams
import archs.EagerFork
import archs.LazyFork

import io.AnsiColor._
import collection.mutable.{Map => MMap}
import collection.mutable.{Set => MSet}
import arch.PTOutput

object RRGPrinter {
  def validName(name: String): String = {
    name.replace("/", "").replace(":", "")
  }

  def getColor(rr: RR): String = {
    rr match {
      case cmux: RRCMux  => "darkblue"
      case pin: RRPin    => "cornflowerblue"
      case src: RRSource => "seagreen"
      case sink: RRSink  => "firebrick3"
      case gi: RRGI      => "coral"
    }
  }

  def getIdColor(id: BlockPortID): String = {
    id.pmw.pb match {
      case D => {
        id.dummy match {
          case Dummy => "burlywood4"
          case Regular => {
            id.width match {
              case 0     => "gold3"
              case 1     => "chartreuse3"
              case 32    => "black"
              case 2     => "darkorchid2"
              case other => ???
            }
          }
        }
      }

      case Hs => {
        id.dummy match {
          case Dummy   => "aquamarine3"
          case Regular => "darkorchid2"
        }
      }

      case Impl => ???
      case Vld  => ???
      case Rdy  => ???
    }
  }

  def getRRName(rr: RR): String = {
    validName(rr.name.replace("[", "").replace("]", ""))
  }

  def printPrimitive(indent: String, rrgPrim: RRGPrim, primMask: Set[RRGPrim]): String = {
    val annosPrim = if (rrgPrim.annos.contains(ATileIo)) {
      rrgPrim.withDagId(None)
    } else {
      rrgPrim
    }

    val color = if (primMask.contains(annosPrim)) {
      "azure4"
    } else {
      rrgPrim.block.prim match {
        case Fork(p) => {
          p.variant match {
            case EagerFork => DotPrinter.nodeTypeToColor("EFork")
            case LazyFork  => DotPrinter.nodeTypeToColor("LFork")
            case other => scala.sys.error("Expected previse fork implementation.")
          }
        }

        case other => {
          DotPrinter.nodeTypeToColor(rrgPrim.block.prim.typeString)
        }
      }
    }

    val attrs = ("style" -> "filled") :: ("fillcolor" -> color) :: ("shape" -> "oval") :: Nil
    val attrsStr = attrs
      .map(
        (k, v) => k + "=" + v
      )
      .mkString(", ")

    indent + validName(rrgPrim.name) + " [" + attrsStr + "];"
  }

  def printRR(indent: String, rr: RR, rrMask: Set[RR]): String = {
    val (style, colorAttr, color) = if (rrMask.contains(rr)) {
      ("bold", "color", "azure4")
    } else {
      rr match {
        case gi: RRGI => ("filled", "fillcolor", getColor(rr))
        case other    => ("bold", "color", getColor(rr))
      }
    }

    val label = rr match {
      case gi: RRGI => gi.name
      case other    => rr.capacity
    }

    val attrs = ("style" -> style) :: (colorAttr -> color) :: ("shape" -> "circle") :: ("label", label) :: Nil
    val attrsStr = attrs
      .map(
        (k, v) => k + "=" + v
      )
      .mkString(", ")

    rr match {
      case RRCMux(pbName, cmuxName, capacity, rrType, _) => {
        // indent + getRRName(rr) + " [" + attrsStr +", label=" + name +"];"
        indent + getRRName(rr) + " [" + attrsStr + "];"
      }

      case other => {
        indent + getRRName(rr) + " [" + attrsStr + "];"
      }
    }
  }

  def printPrimEdge(
      indent: String,
      prim: RRGPrim,
      rr: RRSourceSink,
      rrMask: Set[RR],
      primMask: Set[RRGPrim]
  ): String = {
    val color = if (primMask.contains(prim) || rrMask.contains(rr)) {
      "azure4"
    } else {
      getIdColor(rr.rrType)
    }

    val attrs = ("style" -> "dotted") :: ("color" -> color) :: ("arrowType" -> "none") :: Nil
    val attrsStr = attrs
      .map(
        (k, v) => k + "=" + v
      )
      .mkString(", ")

    rr match {
      case src: RRSource => {
        indent + validName(prim.name) + " -> " + getRRName(rr) + "[" + attrsStr + "];"
      }

      case sink: RRSink => {
        indent + getRRName(rr) + " -> " + validName(prim.name) + "[" + attrsStr + "];"
      }
    }
  }

  def printEdge(indent: String, srcRR: RR, dstRR: RR, rrg: RRG, rrMask: Set[RR]): String = {
    val color = if (rrMask.contains(srcRR) || rrMask.contains(dstRR)) {
      "azure4"
    } else {
      if (
        rrMask.nonEmpty
        && srcRR.isInstanceOf[RRPin] && (srcRR.rrType.pt == PTInput) && !rrMask.contains(rrg.succs(srcRR).head)
        && dstRR.isInstanceOf[RRPin] && (dstRR.rrType.pt == PTOutput) && !rrMask.contains(rrg.preds(dstRR).head)
      ) {
        "azure4"
      } else {
        getIdColor(srcRR.rrType)
      }
    }

    val attrs = ("style" -> "solid") :: ("color" -> color) :: Nil
    val attrsStr = attrs
      .map(
        (k, v) => k + "=" + v
      )
      .mkString(", ")

    indent + getRRName(srcRR) + " -> " + getRRName(dstRR) + "[" + attrsStr + "];"
  }

  def printPrim(indent: String, rrg: RRG, prim: RRGPrim, primMask: Set[RRGPrim], rrMask: Set[RR]): String = {
    val primStr = printPrimitive(indent + "  ", prim, primMask)

    val srcSinks = prim.pinToSrcSink.map(_._2).toSet
    val pins = srcSinks.map {
      srcSink =>
        {
          val preds = rrg.preds.getOrElse(srcSink, Set())
          val succs = rrg.succs.getOrElse(srcSink, Set())

          preds ++ succs
        }
    }.flatten

    val rrStr = (srcSinks ++ pins)
      .map {
        rr =>
          {
            printRR(indent + "  ", rr, rrMask)
          }
      }
      .mkString("\n")

    val primEdges = srcSinks.map(printPrimEdge(indent + "  ", prim, _, rrMask, primMask)).mkString("\n")

    primStr + "\n"
      + rrStr + "\n"
      + primEdges
  }

  def printSym(
      indent: String,
      rrg: RRG,
      prims: Set[RRGPrim],
      primMask: Set[RRGPrim],
      rrMask: Set[RR]
  ): Seq[String] = {
    prims.toSeq.map {
      prim =>
        {
          val clustStart = indent + "subgraph cluster_" + validName(prim.name) + " {\n"
            + indent + "  label=\"" + validName(prim.name) + "\";"

          val primStr = printPrim(indent, rrg, prim, primMask, rrMask)

          clustStart + "\n"
            + primStr + "\n"
            + indent + "}"
        }
    }
  }

  def dagIdStr(dagId: Option[DagId]): String = {
    dagId.fold("Default") {
      id =>
        {
          id.str + "_" + id.loc
        }
    }
  }

  def classifyRR(rrg: RRG): Map[RR, Option[DagId]] = {
    def recPreds(rr: RR): Option[DagId] = {
      val recIds = rr match {
        case gi: RRGI      => None :: Nil
        case src: RRSource => rrg.srcSinkToPrim(src).dagIds :: Nil
        case other => {
          rrg.preds.getOrElse(rr, Set()).map(recPreds(_)).filter(_.nonEmpty)
        }
      }

      if ((recIds.size > 1) || (recIds.size == 0)) {
        None
      } else {
        recIds.head
      }
    }

    def recSuccs(rr: RR): Option[DagId] = {
      val recIds = rr match {
        case gi: RRGI     => None :: Nil
        case sink: RRSink => rrg.srcSinkToPrim(sink).dagIds :: Nil
        case other        => rrg.succs.getOrElse(rr, Nil).map(recSuccs(_)).filter(_.nonEmpty)
      }

      if ((recIds.size > 1) || (recIds.size == 0)) {
        None
      } else {
        recIds.head
      }
    }

    rrg.rrs.map {
      rr =>
        {
          val rrPred = rr match {
            case gi: RRGI => None
            case rr       => recPreds(rr)
          }

          val id = if (rrPred.isEmpty) {
            rr match {
              case gi: RRGI => None
              case rr       => recSuccs(rr)
            }
          } else {
            rrPred
          }

          (rr, id)
        }
    }.toMap
  }

  def printClusters(
      indent: String,
      rrg: RRG,
      primMask: Set[RRGPrim],
      rrMask: Set[RR]
  ): Seq[String] = {
    val rrIds = classifyRR(rrg)

    rrg.prims
      .map {
        prim =>
          {
            if (prim.annos.contains(ATileIo)) {
              val id = rrIds(prim.pinToSrcSink.head._2)
              prim.withDagId(id)
            } else {
              prim
            }
          }
      }
      .groupBy(_.dagIds)
      .map(_._2)
      .map {
        symPrims =>
          {
            val dId = symPrims.head.dagIds
            val idStr = dagIdStr(dId)
            val clustStart = indent + "subgraph cluster_" + idStr + " {\n"
              + indent + "  label=\"" + idStr + "\";"

            val clust = printSym(indent + "  ", rrg, symPrims, primMask, rrMask).mkString("\n" + indent)

            val cmuxs = rrg.rrs
              .collect {
                case cmux: RRCMux => cmux
              }
              .filter(rrIds(_) == dId)
              .map(printRR("  ", _, rrMask))
              .mkString("\n" + indent)

            val clustEnd = indent + "}"

            (clustStart :: clust :: cmuxs :: clustEnd :: Nil)
          }
      }
      .flatten
      .toSeq
  }

  def removeGIs(rrg: RRG): RRG = {
    val nAllPreds = rrg.preds.filter(!_._1.isInstanceOf[RRGI]).map {
      (rr, drrs) =>
        {
          (rr, drrs.filter(!_.isInstanceOf[RRGI]))
        }
    }

    val nAllSuccs = rrg.succs.filter(!_._1.isInstanceOf[RRGI]).map {
      (rr, drrs) =>
        {
          (rr, drrs.filter(!_.isInstanceOf[RRGI]))
        }
    }

    val nAllRRs = rrg.rrs.filter(!_.isInstanceOf[RRGI])

    RRG(rrg.molName, rrg.prims, nAllRRs, nAllPreds, nAllSuccs, rrg.srcSinkToPrim, rrg.t, rrg.pb, rrg.pinMap)
  }

  def apply(
      params: GlobalParamsInst,
      fullRRG: RRG,
      molName: Option[String] = None,
      rrMask: Set[RR] = Set(),
      primMask: Set[RRGPrim] = Set(),
      silent: Boolean = false
  ): Unit = {
    val rrg = removeGIs(fullRRG)

    val clusters = printClusters("  ", rrg, primMask, rrMask)
    val gi = rrg.rrs
      .collect {
        case gi: RRGI => gi
      }
      .map(printRR("  ", _, rrMask))

    val edges = rrg.succs.map {
      (srcRR, dstRRs) =>
        {
          dstRRs.map {
            dstRR =>
              {
                printEdge("  ", srcRR, dstRR, rrg, rrMask)
              }
          }
        }
    }.flatten

    val fName = if (molName.nonEmpty) {
      if (molName.get.contains("debug")) {
        params.buildDir + "/molecules/map/debug/" + validName(molName.get) + ".dot"
      } else if (molName.get.contains("enum")) {
        params.buildDir + "/molecules/map/matches/" + validName(molName.get).replace("enum_", "") + ".dot"
      } else {
        params.buildDir + "/molecules/map/" + validName(molName.get) + ".dot"
      }
    } else {
      params.buildDir + "/rrgs/" + validName(rrg.molName) + ".dot"
    }

    val f = Util.writeOpen(fName)
    if (!silent) {
      println(YELLOW + "Printing: " + fName + RESET)
    }

    f.write("Digraph G {\n")
    f.write(clusters.mkString("\n") + "\n")
    f.write(gi.mkString("\n") + "\n")
    f.write(edges.mkString("\n") + "\n")
    f.write("}\n")
    f.close()
  }
}
