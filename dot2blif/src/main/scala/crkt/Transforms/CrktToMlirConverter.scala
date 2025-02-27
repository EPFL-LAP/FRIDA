package crkt

import frontend.GlobalParamsInst
import mlir.GenericOp
import arch.Hs
import arch.BlockPortID
import arch.PortMeaningWrapper
import arch.Impl
import core.PatternRewriterPass
import archs.Branch
import archs.Fork
import archs.Sink
import archs.Mux
import printers.DotPrinter
import mlir._
import arch.PTOutput
import arch.Pin
import arch.PTInput
import archs.Comparator
import archs.Entry
import archs.Exit
import readers.MLIRParser.functionType
import archs.CntrlMerge
import archs.EntryParams

// Should only be used in the D/Hs representation

object CrktToMlirConverter {
  // def keepHsEdgesOnly(g: ElasticGraph): ElasticGraph = {
  //   val nNodes = g.nodes.map(_._2).map {
  //     n => {
  //       val nPorts = n.ports.map(_._2).flatten.toList.filter(_.id.pmw.pb == Hs).map {
  //         p => {
  //           val nId = BlockPortID(p.width, p.pt, PortMeaningWrapper(p.pmw.pm, Impl), p.dummy)
  //           val nP = Port(nId, "", Map(), p.thisNode, p.distPorts, p.loc)

  //           Port.updateDistPorts(p, nP)

  //           nP
  //         }
  //       }.groupBy(_.id)

  //       Node(n.name, n.nType, n.mlirAttr, n.attr, nPorts)
  //     }
  //   }.map(n => (n.name, n)).toMap

  //   val nProperties = g.properties.filter(_._1.pId.pmw.pb == Hs).map(
  //     (pId, valueConstr) => {
  //       val nId = BlockPortID(pId.pId.width, pId.pId.pt, PortMeaningWrapper(pId.pId.pmw.pm, Impl), pId.pId.dummy)
  //       (PortNodeID(pId.nodeName, nId, pId.loc) -> valueConstr)
  //     }
  //   )

  //   ElasticGraph(nNodes, nProperties)
  // }

  // def fixupProperties(g: ElasticGraph, props: Map[PortNodeID, ValueConstraint]): ElasticGraph = {
  //   val nProps = props.map {
  //     (value, valueConstr) => {
  //       val candidateP = g.nodes(value.nodeName).ports.map(_._2).flatten.filter(_.nodeID() == value)
  //       if(candidateP.size == 1) {
  //         (value -> valueConstr)
  //       } else {
  //         val nWidth = g.nodes(value.nodeName).nType match {
  //           case Branch(p) => p.width
  //           case Fork(p) => p.width
  //           case Sink(p) => p.width
  //           case Mux(p) => p.width
  //           case other => scala.sys.error("Unexected primtivie.")
  //         }

  //         val nId = BlockPortID(nWidth, value.pId.pt, value.pId.pmw, value.pId.dummy)
  //         val nValue = PortNodeID(value.nodeName, nId, value.loc)

  //         (nValue -> valueConstr)
  //       }
  //     }
  //   }

  //   ElasticGraph(g.nodes, nProps)
  // }

  def getName(n: TNode): PrefixedName = {
    PrefixedName.fromPrim(n.nType)
  }

  def getArgs(g: ElasticGraph, n: TNode, nodeNames: Map[TNode, String]): List[Value] = {
    n.ports
      .map(_._2)
      .flatten
      .filter(_.id.pt == PTInput)
      .toList
      .map {
        p =>
          {
            val producer = p.distPorts.head._2
            val producerNode = g(producer.thisNode)

            val producerPin = Pin(producer.id, producer.loc)
            val prodNumOuts = producerNode.ports.map(_._2).flatten.filter(_.id.pt == PTOutput).size

            val prodLoc = if (prodNumOuts == 1) {
              None
            } else {
              Some(MlirIndexInformation(producerNode.nType)(producerPin))
            }

            val loc = MlirIndexInformation(n.nType)(Pin(p.id, p.loc))

            (Value(nodeNames(producerNode), prodLoc, true), loc)
          }
      }
      .sortBy(_._2)
      .map(_._1)
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

  def getAttrs(g: ElasticGraph, n: TNode): AttrMap = n.mlirAttr

  def getConfig(n: TNode): ConfigMap = {
    Map()
  }

  def getType(n: TNode, g: ElasticGraph): MLIRFunc = {
    def rec(ports: List[Port]): List[MLIRType] = {
      val types = ports.map {
        p =>
          {
            val pin = Pin(p.id, p.loc)
            val loc = MlirIndexInformation(n.nType)(pin)

            assert(p.distPorts.size == 1)
            val dp = p.distPorts.head._2

            g(dp.thisNode).nType match {
              case Entry(EntryParams(_, _, Some(memref @ MLIRMemref(shape, t)), _)) => {
                (memref, loc)
              }

              case others => {
                if (p.width == 0) {
                  (MLIRControl, loc)
                } else {
                  (MLIRChannel(MLIRInt(p.width)), loc)
                }
              }
            }
          }
      }

      types.sortBy(_._2).map(_._1)
    }

    val inTypes = rec(n.ports.map(_._2).flatten.filter(_.id.pt == PTInput).toList)
    val outTypes = rec(n.ports.map(_._2).flatten.filter(_.id.pt == PTOutput).toList)

    MLIRFunc(inTypes, outTypes)
  }

  def getResults(n: TNode, nodeNames: Map[TNode, String]): Option[Value] = {
    val numOuts = n.ports.map(_._2).flatten.filter(_.id.pt == PTOutput).size
    val num = if (numOuts == 1) None else Some(numOuts)

    if (numOuts == 0) {
      None
    } else {
      Some(Value(nodeNames(n), num, false))
    }
  }

  def nameNodes(g: ElasticGraph): Map[TNode, String] = {
    val namesNoEntries = g.nodes
      .filter(!_._2.nType.isInstanceOf[Entry])
      .map(_._2)
      .zipWithIndex
      .map {
        (n, i) =>
          {
            (n -> ("%" + i))
          }
      }
      .toMap

    val entriesName = g.ios().filter(_.nType.isInstanceOf[Entry]).toList.sortBy(_.name).zipWithIndex.map {
      (n, i) =>
        {
          (n -> ("%arg" + i))
        }
    }

    namesNoEntries ++ entriesName
  }

  def mapNode(g: ElasticGraph, n: TNode, nodeNames: Map[TNode, String]): Operation = {
    val opName = getName(n)
    val opArgs = getArgs(g, n, nodeNames)
    val opAttrs = getAttrs(g, n)
    val opRegions = Nil
    val opConfig = getConfig(n)
    val opT = getType(n, g)

    val genOp = GenericOp(opName, opArgs, opAttrs, opRegions, opConfig, opT)

    val opResults = getResults(n, nodeNames)

    Operation(
      opResults.fold(Nil)(
        v => v :: Nil
      ),
      genOp
    )
  }

  def mapNodes(g: ElasticGraph): List[Operation] = {
    val nodeNames = nameNodes(g)

    val endNode = g.nodes.filter(_._2.name.contains("end"))
    assert(endNode.size == 1, "No end found.")

    val endOp = mapNode(g, endNode.head._2, nodeNames)

    val otherOps = g.nodes
      .map(_._2)
      .filter(!_.nType.isIo)
      .filter(!_._2.name.contains("end"))
      .toList
      .map {
        n =>
          {
            mapNode(g, n, nodeNames)
          }
      }

    // Put endOp at the end
    otherOps ++ List(endOp)
  }

  def getBlockArgs(g: ElasticGraph): List[Argument] = {
    g.ios().filter(_.nType.isInstanceOf[Entry]).toList.sortBy(_.name).zipWithIndex.map {
      (n, i) =>
        {
          assert(n.ports.forall(_._1.pmw.pb == Impl))
          assert(n.ports.size == 1)
          assert(n.ports.head._2.size == 1)

          val width = n.ports.head._1.width

          n.nType.asInstanceOf[Entry].p.mlirOrigType match {
            case Some(memref @ MLIRMemref(shape, t)) => {
              Argument(Value("%arg" + i, None, false), memref)
            }

            case other => {
              if (width == 0) {
                Argument(Value("%arg" + i, None, false), MLIRControl)
              } else {
                Argument(Value("%arg" + i, None, false), MLIRChannel(MLIRInt(width)))
              }
            }
          }
        }
    }
  }

  // We assume argnames and resNames are consistent with the original naming...
  def getOpAttrs(params: GlobalParamsInst, g: ElasticGraph): AttrMap = {
    val ins = g.ios().filter(_.nType.isInstanceOf[Entry]).toList.sortBy(_.name)
    val endNode = g.nodes.filter(_._2.name.contains("end"))
    assert(endNode.size == 1)

    val outs = endNode.head._2.ports.map(_._2).flatten.toList.sortBy(_.loc)

    val argNames = ("argNames" -> ArrayAttr(
      ins
        .map(_.name)
        .map(
          s => AttrString(s)
        )
    ))

    val resNames = ("resNames" -> endNode.head._2.mlirAttr("resNames"))

    def getIOType(n: TNode): MLIRType = {
      assert(n.ports.forall(_._1.pmw.pb == Impl))
      assert(n.ports.size == 1)
      assert(n.ports.head._2.size == 1)

      val width = n.ports.head._1.width

      n.nType.asInstanceOf[Entry].p.mlirOrigType match {
        case Some(memref @ MLIRMemref(shape, t)) => {
          memref
        }

        case other => {
          if (width == 0) {
            MLIRControl
          } else {
            MLIRChannel(MLIRInt(width))
          }
        }
      }
    }

    val insType = ins.map(getIOType(_))
    val outsType = outs.map {
      p =>
        {
          if (p.width == 0) {
            MLIRControl
          } else {
            MLIRChannel(MLIRInt(p.width))
          }
        }
    }

    val functionType = ("function_type" -> MLIRFunc(insType, outsType))
    val symName = ("sym_name" -> AttrString(params.bench))

    (argNames :: resNames :: functionType :: symName :: Nil).toMap
  }

  // Assumes the circuit is already in mlir form
  // In particular, should be in Impl representation and have an explicit end at the end instead of multiple out ports
  def apply(params: GlobalParamsInst, g: ElasticGraph): GenericOp = {
    val crktOps = mapNodes(g)

    val blockArgs = getBlockArgs(g)
    val hsBlock = Block("bb0", blockArgs, crktOps)
    val hsRegion = Region(Nil, hsBlock :: Nil)

    val hsOpAttr = getOpAttrs(params, g)
    val hsOpType = MLIRFunc(Nil, Nil)
    val hsGenOp = GenericOp(PrefixedName("handshake", "func"), Nil, hsOpAttr, hsRegion :: Nil, Map(), hsOpType)
    val hsOp = Operation(Nil, hsGenOp)

    val moduleOpType = MLIRFunc(Nil, Nil)
    val moduleRegion = Region(hsOp :: Nil, Nil)

    GenericOp(PrefixedName("builtin", "module"), Nil, Map(), moduleRegion :: Nil, Map(), moduleOpType)
  }
}
