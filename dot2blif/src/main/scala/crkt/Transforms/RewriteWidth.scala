package crkt

import core.PatternRewriter
import archs._
import arch.Arch
import arch.Impl
import arch.BlockPortID
import arch.PMCond
import crkt.WidthToArchPass.findClosestWidth
import mlir.MLIRInt
import mlir.AttrInteger
import mlir.MLIRUInt
import mlir.AttrBoolean

// TODO do not touch width of conditions so far...

object RewriteWidthUtils {
  def mapPorts(ports: List[Port], nWidth: Int): List[Port] = {
    ports.map {
      case p @ Port(BlockPortID(width, pt, pmw, dummy), name, attr, thisNode, distPorts, loc) => {
        assert(nWidth >= width, "" + width + " -> " + nWidth)

        if (pmw.pm.isInstanceOf[PMCond] || (nWidth == width)) {
          p
        } else {
          val nId = BlockPortID(nWidth, pt, pmw, dummy)
          val nP = Port(nId, name, attr, thisNode, distPorts, loc)

          Port.updateDistPorts(p, nP)

          nP
        }
      }
    }
  }

  def mapNode[Prim <: Primitive](n: TNode, updateNode: ((TNode, Int, (Int => Primitive)) => TNode)): TNode = {
    n.nType match {
      case Fork(p)       => updateNode(n, p.width, ((nWidth: Int) => Fork(ForkParams(nWidth, p.num, p.variant))))
      case Merge(p)      => updateNode(n, p.width, ((nWidth: Int) => Merge(MergeParams(nWidth, p.num))))
      case Mux(p)        => updateNode(n, p.width, ((nWidth: Int) => Mux(MuxParams(nWidth, p.num))))
      case Select(p)     => updateNode(n, p.width, ((nWidth: Int) => Select(SelectParams(nWidth, p.num))))
      case Branch(p)     => updateNode(n, p.width, ((nWidth: Int) => Branch(BranchParams(nWidth))))
      case Constant(p)   => updateNode(n, p.width, ((nWidth: Int) => Constant(ConstantParams(nWidth))))
      case TEHB(p)       => updateNode(n, p.width, ((nWidth: Int) => TEHB(TEHBParams(nWidth, p.depth))))
      case OEHB(p)       => updateNode(n, p.width, ((nWidth: Int) => OEHB(OEHBParams(nWidth, p.depth))))
      case CntrlMerge(p) => n
      case Sink(p)       => updateNode(n, p.width, ((nWidth: Int) => Sink(SinkParams(nWidth))))
      case Source(p)     => n
      case Extsi(p) => {
        val nIn = updateNode(n, p.inWidth, ((nWidth: Int) => Extsi(ExtsiParams(nWidth, p.outWidth))))
        val callback = ((nWidth: Int) => Extsi(ExtsiParams(nIn.nType.p.asInstanceOf[ExtsiParams].inWidth, nWidth)))
        updateNode(nIn, p.outWidth, callback)
      }
      case Extui(p) => {
        val nIn = updateNode(n, p.inWidth, ((nWidth: Int) => Extui(ExtuiParams(nWidth, p.outWidth))))
        val callback = ((nWidth: Int) => Extui(ExtuiParams(nIn.nType.p.asInstanceOf[ExtuiParams].inWidth, nWidth)))
        updateNode(nIn, p.outWidth, callback)
      }
      case Trunc(p) => {
        val nIn = updateNode(n, p.inWidth, ((nWidth: Int) => Trunc(TruncParams(nWidth, p.outWidth))))
        val callback = ((nWidth: Int) => Trunc(TruncParams(nIn.nType.p.asInstanceOf[TruncParams].inWidth, nWidth)))
        updateNode(nIn, p.outWidth, callback)
      }
      case Mult(p)       => updateNode(n, p.width, ((nWidth: Int) => Mult(MultParams(nWidth))))
      case Operator(p)   => updateNode(n, p.width, ((nWidth: Int) => Operator(OperatorParams(nWidth, p.op))))
      case Comparator(p) => updateNode(n, p.width, ((nWidth: Int) => Comparator(ComparatorParams(nWidth, p.predicate))))
      case Entry(p)      => updateNode(n, p.width, ((nWidth: Int) => Entry(EntryParams(nWidth, p.pbs, p.mlirOrigType))))
      case Exit(p)       => updateNode(n, p.width, ((nWidth: Int) => Exit(ExitParams(nWidth, p.pbs))))
    }
  }

  def updateAttrs(mlirAttr: Map[String, mlir.AttrValue], nWidth: Int): Map[String, mlir.AttrValue] = {
    mlirAttr.map {
      case ("value", attr) => {
        attr match {
          case AttrInteger(value, t) => {
            val nType = t match {
              case MLIRInt(_)  => MLIRInt(nWidth)
              case MLIRUInt(_)=> MLIRUInt(nWidth)
            }

            ("value" -> AttrInteger(value, nType))
          }

          case attr @ AttrBoolean(b) => {
            if(nWidth > 1) {
              if(b) {
                ("value" -> AttrInteger(1, MLIRInt(nWidth)))
              } else {
                ("value" -> AttrInteger(0, MLIRInt(nWidth)))
              }
            } else {
              ("value" -> attr)
            }
          }

          case other => scala.sys.error("Unexpected type for MLIR value.")
        }
      }
      case other => other
    }
  }
}

case class RewriteWidth(archs: List[Arch]) extends PatternRewriter {
  import RewriteWidthUtils._

  val allowedWidths = archs.map(_.width).toSet + 0

  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    assert(n.ports.map(_._2).flatten.forall(_.id.pmw.pb == Impl))

    n.ports
      .map(_._2)
      .flatten
      .exists(
        p => !allowedWidths.contains(p.id.width)
      )
  }

  def findClosestWidth(width: Int): Int = {
    allowedWidths.toList.sorted.filter(_ >= width).head
  }

  def findClosestWidth(p: Port): Int = {
    allowedWidths.toList.sorted.filter(_ >= p.width).head
  }

  def updateNode(n: TNode, width: Int, instPrim: (Int => Primitive)): TNode = {
    val nWidth = findClosestWidth(width)

    if (nWidth == width) {
      n
    } else {
      val nPorts = n.nType match {
        case Constant(p) => {
          val outPorts = mapPorts(n.outPortsList(), nWidth)
          val inPorts = n.inPortsList()

          (inPorts ++ outPorts).groupBy(_.id)
        }

        case other => mapPorts(n.ports.map(_._2).flatten.toList, nWidth).groupBy(_.id)
      }

      val nPrim = instPrim(nWidth)

      Node(n.name, nPrim, updateAttrs(n.mlirAttr, nWidth), n.attr, n.annos, nPorts)
    }
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    n.nType match {
      case BlackBox(p) => {
        // Hacky but will do for now
        val nIns = p.ins.map((w, n) => (findClosestWidth(w), n))
        val nOuts = p.outs.map((w, n) => (findClosestWidth(w), n))

        val nPrim = BlackBox(BlackBoxParams(nIns, nOuts, p.name, p.dontTouch, p.recursivelyExtendPrefix))

        val nPorts = n.ports.map(_._2).flatten.map {
          p => {
            val nId = BlockPortID(findClosestWidth(p.width), p.pt, p.pmw, p.dummy)
            val nP = Port(nId, p.name, p.attr, p.thisNode, p.distPorts, p.loc)

            Port.updateDistPorts(p, nP)

            nP
          }
        }.toList.groupBy(_.id)

        Node(n.name, nPrim, n.mlirAttr, n.attr, n.annos, nPorts) :: Nil
      }

      case others => mapNode(n, updateNode) :: Nil
    }
  }
}
