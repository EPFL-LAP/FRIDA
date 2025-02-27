package crkt

import mlir._
import archs._
import arch.Pin
import arch.BlockPortID
import arch.PTInput
import arch.PTOutput
import arch.PortMeaningWrapper
import arch.PMCond
import arch.Impl
import arch.Regular
import arch.PortType
import arch.PortMeaning
import arch.PMData
import arch.BlockInterface
import util.Util.log2ceil
import arch.PTUndef
import frontend.GlobalParamsInst
import printers.DotPrinter
import packerv2.AnnotateBasicBlocks

import collection.mutable.{Map => MMap}
import util.Util

object MlirIndexInformation {
  def getPin(width: Int, pt: PortType, pm: PortMeaning, loc: Int): Pin = {
    Pin(BlockPortID(width, pt, PortMeaningWrapper(pm, Impl), Regular), loc)
  }

  def apply(p: Primitive, op: Option[Operation] = None): Pin => Int = {
    p.p match {
      case MuxParams(width, num, _) => {
        val cond = (getPin(1, PTInput, PMCond(None), 0) -> 0)
        val dataIn = (0 until num).toList.map(
          i => (getPin(width, PTInput, PMData(None), i) -> (1 + i))
        )
        val dataOut = (getPin(width, PTOutput, PMData(None), 0) -> 0)

        val locMap = (cond :: dataOut :: dataIn).toMap

        (pin: Pin) => { locMap(pin) }
      }

      case SelectParams(width, num, _) => {
        val cond = (getPin(1, PTInput, PMCond(None), 0) -> 0)
        val dataIn = (0 until num).toList.map(
          i => (getPin(width, PTInput, PMData(None), i) -> (1 + i))
        )
        val dataOut = (getPin(width, PTOutput, PMData(None), 0) -> 0)

        val locMap = (cond :: dataOut :: dataIn).toMap

        (pin: Pin) => locMap(pin)
      }

      case BranchParams(width, _) => {
        val cond = (getPin(1, PTInput, PMCond(None), 0) -> 0)
        val dataIn = (getPin(width, PTInput, PMData(None), 0) -> 1)
        val dataOut = (0 until 2).toList.map(
          i => (getPin(width, PTOutput, PMData(None), i) -> i)
        )

        val locMap = (cond :: dataIn :: dataOut).toMap

        (pin: Pin) => locMap(pin)
      }

      case CntrlMergeParams(num, indexWidth, _) => {
        val dataIn = (0 until num).toList.map(
          i => (getPin(0, PTInput, PMData(None), i) -> i)
        )
        val dataOut = (getPin(0, PTOutput, PMData(None), 0) -> 0)
        val condOut = (getPin(log2ceil(num).toInt, PTOutput, PMCond(None), 0) -> 1)

        val locMap = (dataOut :: condOut :: dataIn).toMap

        (pin: Pin) => locMap(pin)
      }

      case other => (pin: Pin) => pin.loc
    }
  }
}

object MlirToCrktConverter {
  def getCrktOp(mlir: GenericOp): Operation = {
    assert(mlir.regions.size == 1, mlir.regions.size)
    val hsRegion = mlir.regions.head

    assert(hsRegion.ops.size == 1, hsRegion.ops.size)
    hsRegion.ops.head
  }

  def getCrkt(mlir: GenericOp): Block = {
    val hsOp = getCrktOp(mlir)

    assert((hsOp.genOp.regions.size == 1) && (hsOp.genOp.regions.head.blocks.size == 1))
    hsOp.genOp.regions.head.blocks.head
  }

  def getCrktFunctionType(mlir: GenericOp): MLIRFunc = {
    getCrktOp(mlir).genOp.attrs("function_type").asInstanceOf[MLIRFunc]
  }

  def toNodeInterface(prim: Primitive, nodeName: String): Map[BlockPortID, List[Port]] = {
    val primInst = prim.instantiate(prim.p)
    prim match {
      case BlackBox(p) => {
        val inPs = p.ins.map(_._1).zipWithIndex.map {
          (w, loc) => {
            val id = BlockPortID(w, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
            Port(id, "", Map(), nodeName, Map(), loc)
          }
        }

        val outPs = p.outs.map(_._1).zipWithIndex.map {
          (w, loc) => {
            val id = BlockPortID(w, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)
            Port(id, "", Map(), nodeName, Map(), loc)
          }
        }

        (inPs ++ outPs).groupBy(_.id)
      }

      case others => {
        primInst.blockInterface.ports
          .map(_._2)
          .map {
            bp =>
            {
              (0 until bp.words).map {
                loc =>
                {
                  Port(bp.id, "", Map(), nodeName, Map(), loc)
                }
              }
            }
          }
          .flatten
          .toList
          .groupBy(_.id)
      }
    }
  }

  def getDisconnectedNode(prim: Primitive, op: Operation, mlir: GenericOp): TNode = {
    val name = op.genOp.attrs("handshake.name").str().replace("\"", "")

    val attrs = if(name.contains("end")) { // For the end, get the name of the outputs
      val hsOp = getCrktOp(mlir)

      val resNames = hsOp.genOp.attrs("resNames")

      op.genOp.attrs + ("resNames" -> resNames)
    } else {
      op.genOp.attrs
    }

    Node(name, prim, attrs, Map(), Set(), toNodeInterface(prim, name))
  }

  // TODO Only instantiate Entries for now
  def instantiateIo(mlir: GenericOp, argIndex: Int, arg: Argument): TNode = {
    val crktOp = getCrktOp(mlir)

    val width = getCrktFunctionType(mlir).ins(argIndex) match {
      case MLIRHs      => 0
      case MLIRInt(w)  => w
      case MLIRUInt(w) => w
      case MLIRMemref(shape, t) => {
        assert(shape.size == 1)
        Util.log2ceil(shape.head).toInt
      }
      case MLIRControl              => 0
      case MLIRChannel(MLIRInt(w))  => w
      case MLIRChannel(MLIRUInt(w)) => w
      case other                    => ???
    }

    val origType = getCrktFunctionType(mlir).ins(argIndex)
    val params = EntryParams(width, Set(Impl), Some(origType))
    val prim = Entry(params)
    val name = crktOp.genOp.attrs("argNames").asInstanceOf[ArrayAttr].value(argIndex).asInstanceOf[AttrString].value

    Node(name, prim, Map(), Map(), Set(), toNodeInterface(prim, name))
  }

  // We assume all the circuit is put within a single basic block
  def getNodes(mlir: GenericOp): List[TNode] = {
    val crktBlock = getCrkt(mlir)

    val internalNodes = crktBlock.ops.map {
      op =>
        {
          val prim = op.toPrim

          getDisconnectedNode(prim, op, mlir)
        }
    }

    val crktOp = getCrktOp(mlir)

    val entries = crktBlock.args.zipWithIndex.map {
      (arg, i) =>
        {
          instantiateIo(mlir, i, arg)
        }
    }

    internalNodes ++ entries
  }

  def portToPin(p: Port): Pin = Pin(p.id, p.loc)

  def toArgList(value: Value): List[Value] = {
    assert(!value.isArg)

    if (value.num.isEmpty) {
      Value(value.name, value.num, value.isArg) :: Nil
    } else {
      (0 until value.num.get)
        .map(
          loc => Value(value.name, Some(loc), value.isArg)
        )
        .toList
    }
  }

  def getProducedValues(mlir: GenericOp): Map[Value, PortNodeID] = {
    val crktBlock = getCrkt(mlir)

    val internalProduced = crktBlock.ops
      .map {
        op =>
          {
            val prim = op.toPrim
            val node = getDisconnectedNode(prim, op, mlir)
            val mlirLocs = MlirIndexInformation(prim, Some(op))

            node.ports.map(_._2).flatten.filter(_.id.pt == PTOutput).map {
              p => {
                val mlirLoc = mlirLocs(portToPin(p))

                val mlirValue = p.id.pt match {
                  case PTOutput => op.values.map(toArgList(_)).flatten.toList(mlirLoc)
                  case other    => scala.sys.error("Expected output port.")
                }

                (mlirValue, p.nodeID())
              }
            }
          }
      }
      .flatten
      .toMap

    val crktOp = getCrktOp(mlir)

    val argProduced = crktBlock.args.zipWithIndex.map {
      (arg, i) =>
        {
          val n = instantiateIo(mlir, i, arg)
          val pId = n.ports.head._2.head.nodeID()

          (arg.v.flipped, pId)
        }
    }

    internalProduced ++ argProduced
  }

  def getEdges(mlir: GenericOp): Map[PortNodeID, Set[PortNodeID]] = {
    val crktBlock = getCrkt(mlir)

    val edges = MMap[PortNodeID, Set[PortNodeID]]()
    val producedValues = getProducedValues(mlir)

    crktBlock.ops.foreach {
      op =>
        {
          val prim = op.toPrim
          val node = getDisconnectedNode(prim, op, mlir)
          val mlirLocs = MlirIndexInformation(prim, Some(op))

          val name = op.genOp.attrs("handshake.name").str().replace("\"", "")

          node.ports.map(_._2).flatten.filter(_.id.pt == PTInput).foreach {
            p =>
              {
                val mlirLoc = mlirLocs(portToPin(p))

                val mlirValue = p.id.pt match {
                  case PTInput => op.genOp.args(mlirLoc)
                  case other   => scala.sys.error("Expected Input port.")
                }

                val producer = producedValues(mlirValue.flipped)
                val target = edges.getOrElse(producer, Set()) + p.nodeID()

                edges(producer) = target
              }
          }
        }
    }

    edges.toMap
  }

  def setEdges(nodes: List[TNode], edges: Map[PortNodeID, Set[PortNodeID]]): ElasticGraph = {
    val invEdges = edges.map((p, dps) => dps.map(dp => (dp, p))).flatten.toMap
    val pInfo = nodes.map(_.ports.map(_._2)).flatten.flatten.map(p => (p.nodeID(), p)).toMap

    nodes.map {
      n =>
        {
          n.ports.map(_._2).flatten.map {
            p =>
              {
                val dps = if (p.id.pt == PTInput) {
                  val prod = invEdges(p.nodeID())
                  val prodPort = pInfo(prod)

                  Map((prod -> prodPort))
                } else {
                  edges(p.nodeID()).map(dp => (dp, pInfo(dp))).toMap
                }

                p.distPorts = dps
              }
          }
        }
    }

    ElasticGraph(nodes)
  }

  def apply(params: GlobalParamsInst, mlir: GenericOp): ElasticGraph = {
    // collect all nodes together with their attributes and ports
    val nodes = getNodes(mlir)

    // collect all values
    val edges = getEdges(mlir)

    // groupby node name and extend node info
    setEdges(nodes, edges)
  }
}
