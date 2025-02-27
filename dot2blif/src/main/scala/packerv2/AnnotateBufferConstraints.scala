package packerv2

import frontend.GlobalParamsInst
import core.PatternRewriterPass
import crkt._
import mlir.{Block => MBlock, _}
import arch._
import archs.Branch
import archs.Fork
import archs.Sink
import archs.Mux
import printers.DotPrinter
import readers.MLIRParser
import printers.MLIRPrinter

import math.max
import math.min
import archs.BlackBox

object AnnotateBufferConstraints {
  def keepHsEdgesOnly(g: ElasticGraph): ElasticGraph = {
    val outPorts = g.nodes
      .map(_._2)
      .map(_.ports.map(_._2).flatten)
      .flatten
      .filter(_.id.pt == PTOutput)
      .filter(_.id.pmw.pb == Hs)
      .toList

    val nPorts = outPorts.map {
      p =>
        {
          val nId = BlockPortID(p.width, p.pt, PortMeaningWrapper(p.pmw.pm, Impl), p.dummy)
          val nP = Port(nId, "", Map(), p.thisNode, Map(), p.loc)

          val nDPs = p.distPorts
            .map(_._2)
            .map {
              dp =>
                {
                  val nDpId = BlockPortID(dp.width, dp.pt, PortMeaningWrapper(dp.pmw.pm, Impl), dp.dummy)
                  Port(nDpId, "", Map(), dp.thisNode, Map((nP.nodeID() -> nP)), dp.loc)
                }
            }
            .toList

          nP.distPorts = nDPs
            .map(
              nDP => (nDP.nodeID() -> nDP)
            )
            .toMap

          nP :: nDPs
        }
    }.flatten

    val nNodes = nPorts
      .groupBy(_.thisNode)
      .map {
        (nodeName, ports) =>
          {
            val node = g.nodes(nodeName)

            Node(node.name, node.nType, node.mlirAttr, node.attr, node.annos, ports.groupBy(_.id))
          }
      }
      .map(
        n => (n.name, n)
      )
      .toMap

    val nProperties = g.properties
      .filter(_._1.pId.pmw.pb == Hs)
      .map(
        (pId, valueConstr) => {
          val nId = BlockPortID(pId.pId.width, pId.pId.pt, PortMeaningWrapper(pId.pId.pmw.pm, Impl), pId.pId.dummy)
          (PortNodeID(pId.nodeName, nId, pId.loc) -> valueConstr)
        }
      )

    ElasticGraph(nNodes, nProperties)
  }

  def fixupProperties(g: ElasticGraph, props: Map[PortNodeID, ValueConstraint]): ElasticGraph = {
    val nProps = props.map {
      (value, valueConstr) =>
        {
          val candidateP = g.nodes(value.nodeName).ports.map(_._2).flatten.filter(_.nodeID() == value)
          if (candidateP.size == 1) {
            (value -> valueConstr)
          } else {
            val nWidth = g.nodes(value.nodeName).nType match {
              case Branch(p) => p.width
              case Fork(p)   => p.width
              case Sink(p)   => p.width
              case Mux(p)    => p.width
              case other     => scala.sys.error("Unexected primtivie.")
            }

            val nId = BlockPortID(nWidth, value.pId.pt, value.pId.pmw, value.pId.dummy)
            val nValue = PortNodeID(value.nodeName, nId, value.loc)

            (nValue -> valueConstr)
          }
        }
    }

    ElasticGraph(g.nodes, nProps)
  }

  def emptyBufConstr(argNum: Int): BufConstraint = {
    BufConstraint(argNum, BufSlotConstraint(0, 0, 0, 0), BufDelayConstraint(0, 0, 0))
  }

  def getBufConstraint(g: ElasticGraph, n: TNode, target: PortNodeID): BufConstraint = {
    val mlirArgNum = MlirIndexInformation(n.nType)(Pin(target.pId, target.loc))

    if (g.properties.contains(target)) {
      val constr = g.properties(target).asInstanceOf[BufferConstraint]
      BufConstraint(mlirArgNum, constr.sc, constr.dc)
    } else {
      emptyBufConstr(mlirArgNum)
    }
  }

  def getAttrs(g: ElasticGraph, n: TNode): Option[(String, BufConstrAttr)] = {
    val bufConstraints = n.ports.map(_._2).flatten.filter(_.id.pt == PTInput).map {
      p =>
        {
          getBufConstraint(g, n, p.nodeID())
        }
    }

    if (n.mlirAttr.contains("bufProps")) {
      assert(
        n.mlirAttr("bufProps").asInstanceOf[BufConstrAttr].value.forall {
          bc =>
            {
              (bc.minT == 0) && (bc.minO == 0) && (bc.maxT == 0) && (bc.maxT == 0)
            }
        }
      )
    }

    val bufProps = n.mlirAttr.get("bufProps")
    val constrainedNums = if (bufProps.nonEmpty) {
      bufProps.get.asInstanceOf[BufConstrAttr].value.map(_.argNum).toSet
    } else {
      Set()
    }

    val finalBufConstr = bufConstraints.filter(
      bc => !constrainedNums.contains(bc.argNum)
    )

    if (finalBufConstr.nonEmpty) {
      val finalBufAttr = BufConstrAttr(finalBufConstr.toList)

      Some(("handshake.bufProps" -> finalBufAttr))
    } else {
      None
    }
  }

  def getName(op: Operation): String = {
    op.genOp.attrs("handshake.name").str().replace("\"", "")
  }

  def genWrapper(hsBlock: MBlock, oldOp: Operation): GenericOp = {
    val hsRegion = Region(Nil, hsBlock :: Nil)

    val hsOpAttr = oldOp.genOp.attrs
    val hsOpType = MLIRFunc(Nil, Nil)
    val hsGenOp = GenericOp(PrefixedName("handshake", "func"), Nil, hsOpAttr, hsRegion :: Nil, Map(), hsOpType)
    val hsOp = Operation(Nil, hsGenOp)

    val moduleOpType = MLIRFunc(Nil, Nil)
    val moduleRegion = Region(hsOp :: Nil, Nil)

    GenericOp(PrefixedName("builtin", "module"), Nil, Map(), moduleRegion :: Nil, Map(), moduleOpType)
  }

  def merge(a: BufSlotConstraint, b: BufSlotConstraint): BufSlotConstraint = {
    BufSlotConstraint(max(a.minT, b.minT), min(a.maxT, b.maxT), max(a.minO, b.minO), min(a.maxO, b.maxO))
  }

  def merge(a: BufDelayConstraint, b: BufDelayConstraint): BufDelayConstraint = {
    assert(a.isEmpty || b.isEmpty)

    if(a.isEmpty) {
      b
    } else {
      a
    }
  }

  def resolveConflicts(op: GenericOp, bufAttr: BufConstrAttr): BufConstrAttr = {
    if(op.attrs.contains("handshake.bufProps")) {
      val opBufAttr = op.attrs("handshake.bufProps").asInstanceOf[BufConstrAttr]

      val nAttr = (opBufAttr.value ++ bufAttr.value).groupBy(_.argNum).map(_._2).map {
        bufConstrs => {
          if(bufConstrs.size == 1) {
            bufConstrs.head
          } else {
            assert(bufConstrs.size == 2)

            val a = bufConstrs.head
            val b = bufConstrs.tail.head

            BufConstraint(a.argNum, merge(a.slotConstr, b.slotConstr), merge(a.delayConstr, b.delayConstr))
          }
        }
      }

      BufConstrAttr(nAttr.toList)
    } else {
      bufAttr
    }
  }

  def annotateMLIR(mlir: GenericOp, g: ElasticGraph): GenericOp = {
    val crktBlock = MlirToCrktConverter.getCrkt(mlir)
    val hsOp = MlirToCrktConverter.getCrktOp(mlir)

    val nodeBufConstrs = g.nodes.map(
      (nName, n) => (nName, getAttrs(g, n))
    )

    val nOps = crktBlock.ops.map {
      op =>
        {
          val opName = getName(op)

          val nAttrs = if (nodeBufConstrs.contains(opName)) {
            val existingAttrs = op.genOp.attrs
            nodeBufConstrs(opName).fold(existingAttrs) {
              bufConstr => {
                (existingAttrs + ("handshake.bufProps" -> resolveConflicts(op.genOp, bufConstr._2)))
              }
            }
          } else {
            if (opName.contains("end")) {
              op.genOp.attrs
            } else {
              val bufAttr = op.toPrim match {
                case BlackBox(_) => Nil
                case others => {
                  (0 until op.genOp.args.size).map {
                    i => {
                      emptyBufConstr(i)
                    }
                  }
                }
              }

              if (bufAttr.isEmpty) {
                op.genOp.attrs
              } else {
                op.genOp.attrs + ("handshake.bufProps" -> resolveConflicts(op.genOp, BufConstrAttr(bufAttr.toList)))
              }

            }
          }

          val nGenOp = GenericOp(op.genOp.name, op.genOp.args, nAttrs, op.genOp.regions, op.genOp.config, op.genOp.t)
          Operation(op.values, nGenOp)
        }
    }

    val nBlock = MBlock(crktBlock.name, crktBlock.args, nOps)
    genWrapper(nBlock, hsOp)
  }

  def apply(params: GlobalParamsInst, g: ElasticGraph): GenericOp = {
    val implG = keepHsEdgesOnly(g)
    val widthG = PatternRewriterPass(RewriteInconsistentWidth :: Nil, implG)
    val mlirG = fixupProperties(widthG, implG.properties)

    DotPrinter(params.buildDir + "/lowering/mlir.dot")(mlirG, Set())

    val origMlir = MLIRParser(params.preBuf)
    annotateMLIR(origMlir, mlirG)
  }
}
