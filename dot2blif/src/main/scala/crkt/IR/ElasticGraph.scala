package crkt

import arch._
import archs.Params
import archs.Primitive
import core.Namer
import core.Annotation
import printers.DotPrinter

import math.max
import math.min
import io.AnsiColor._
import mlir.BufSlotConstraint
import mlir.BufDelayConstraint

sealed trait Assignable

object ElasticEdge {
  def apply(p0: Port, p1: Port): ElasticEdge = {
    assert(p0.pt == PortType.not(p1.pt))

    if (p0.pt == PTOutput) {
      new ElasticEdge(p0, p1)
    } else {
      new ElasticEdge(p1, p0)
    }
  }
}

case class ElasticEdge(fromPort: Port, toPort: Port)

sealed trait ValueConstraint

case class BufferConstraint(sc: BufSlotConstraint, dc: BufDelayConstraint) extends ValueConstraint {
  override def toString(): String = {
    sc.toString() + " at " + dc.toString()
  }
}

object ElasticGraph {
  def apply(nodes: List[TNode]): ElasticGraph = {
    ElasticGraph(
      nodes
        .map(
          n => (n.name, n)
        )
        .toMap,
      Map()
    )
  }

  def apply(nodes: Map[String, TNode]): ElasticGraph = {
    ElasticGraph(nodes, Map())
  }
}

case class ElasticGraph(nodes: Map[String, TNode], properties: Map[PortNodeID, ValueConstraint]) {
  def apply(nName: String): TNode = {
    nodes(nName)
  }

  def map(f: ((String, TNode)) => (String, TNode)): ElasticGraph = {
    ElasticGraph(nodes.map(f), properties)
  }

  def ios(): Seq[TNode] = {
    nodes.map(_._2).filter(_.nType.isIo).toList
  }

  def successors(n: TNode): Seq[TNode] = {
    n.ports
      .map(_._2)
      .flatten
      .filter(_.id.pt == PTOutput)
      .map {
        p =>
          {
            p.distPorts.map(_._2).map(_.thisNode)
          }
      }
      .flatten
      .toSet
      .toSeq
      .map {
        nName =>
          {
            nodes(nName)
          }
      }
  }

  def predecessors(n: TNode): Seq[TNode] = {
    n.ports
      .map(_._2)
      .flatten
      .filter(_.id.pt == PTInput)
      .map {
        p =>
          {
            p.distPorts.map(_._2).map(_.thisNode)
          }
      }
      .flatten
      .toSet
      .toSeq
      .map {
        nName =>
          {
            nodes(nName)
          }
      }
  }

  def neighbors(n: TNode, dir: PortType): Seq[TNode] = {
    dir match {
      case PTInput => {
        predecessors(n)
      }

      case PTOutput => {
        successors(n)
      }

      case other => ???
    }
  }

  lazy val totalOrder: Map[String, Int] = {
    nodes.map(_._2.name).toSeq.sorted.zip((0 until nodes.size)).toMap
  }

  def getPort(pId: PortNodeID): Port = {
    nodes(pId.nodeName).ports(pId.pId).filter(_.loc == pId.loc).head
  }

  def findEntries(): List[TNode] = {
    nodes.values
      .filter(
        (n: TNode) => n.nType == archs.Entry
      )
      .toList
  }

  def findExits(): List[TNode] = {
    nodes.values
      .filter(
        (n: TNode) => n.nType == archs.Exit
      )
      .toList
  }

  def hasExplicitHandshake(): Boolean = {
    nodes
      .filter(
        (nodeName: String, node: TNode) => node.ports.filter(!_._1.pmw.pb.isImpl()).nonEmpty
      )
      .nonEmpty
  }

  def contains(e: ElasticEdge): Boolean = {
    val srcIn = nodes.contains(e.fromPort.thisNode)
    val dstIn = nodes.contains(e.toPort.thisNode)

    if (srcIn && dstIn) {
      val srcPort = nodes(e.fromPort.thisNode).ports.map(_._2).flatten.filter(_ == e.fromPort)

      if(srcPort.nonEmpty) {
        assert(srcPort.size == 1, "" + e + " -> " + srcPort)
        srcPort.head.distPorts.keySet.contains(e.toPort.nodeID())
      } else  {
        false
      }
    } else {
      false
    }
  }

  def hasEdgesBetweenNodes(fromNode: TNode, toNode: TNode): Seq[ElasticEdge] = {
    val portMappings = fromNode.ports
      .filter(_._1.pt == PTOutput)
      .map {
        (pId, ps) =>
          {
            ps.map {
              p =>
                {
                  (
                    p,
                    p.distPorts.toList.filter {
                      (dpId, dp) =>
                        {
                          dp.thisNode == toNode.name
                        }
                    }
                  )
                }
            }.filter(_._2.nonEmpty)
          }
      }
      .flatten
      .filter(_._2.nonEmpty)

    portMappings
      .map {
        (p, dps) =>
          {
            dps.map {
              (_, dp) =>
                {
                  ElasticEdge(p, dp)
                }
            }
          }
      }
      .flatten
      .map(
        ee => ((ee.fromPort.nodeID(), ee.toPort.nodeID()), ee)
      )
      .toMap
      .toSeq
      .map(_._2)
  }
}

type TNode = Node

// TODO remove attr and make this support annotations
// TODO equals and hashcode should only work on the name
case class Node(
  name: String,
  nType: Primitive,
  mlirAttr: Map[String, mlir.AttrValue],
  attr: Map[String, String],
  annos: Set[Annotation],
  // TODO this should be a List of ports, and the map is constructed only when needed.
  ports: Map[BlockPortID, List[Port]]
) extends Assignable {
  val typeStr = nType.typeString

  def nonEmptyUnder(arch: Arch): Boolean = {
    ports.values.flatten
      .map(
        (p: Port) => p.nonEmptyUnder(arch)
      )
      .reduceOption(_ || _)
      .getOrElse(false)
  }

  def withAttr(k: String, v: String): Node = {
    Node(name, nType, mlirAttr, attr + (k -> v), annos, ports)
  }

  def getDistNodesNames(): Set[String] = {
    ports
      .map(_._2)
      .flatten
      .map {
        p =>
          {
            p.distPorts.map(_._2).map {
              dp =>
                {
                  dp.thisNode
                }
            }
          }
      }
      .flatten
      .toSet
  }

  def neighbors(g: ElasticGraph): Set[TNode] = {
    ports.map(_._2).flatten.map(_.distPorts.map(_._2.thisNode)).flatten.map(g(_)).toSet
  }

  def successors(g: ElasticGraph): Set[TNode] = {
    ports.map(_._2).flatten.filter(_.id.pt == PTOutput).map(_.distPorts.map(_._2.thisNode)).flatten.map(g(_)).toSet
  }

  def predecessors(g: ElasticGraph): Set[TNode] = {
    ports.map(_._2).flatten.filter(_.id.pt == PTInput).map(_.distPorts.map(_._2.thisNode)).flatten.map(g(_)).toSet
  }

  def inPorts(): Map[BlockPortID, List[Port]] = {
    ports.filter(_._1.pt == PTInput)
  }

  def outPorts(): Map[BlockPortID, List[Port]] = {
    ports.filter(_._1.pt == PTOutput)
  }

  override def toString(): String = {
    val attrs = attr
      .map(
        (k, v) => (k + " -> " + v)
      )
      .mkString(", ")
    name + "[" + typeStr + "] { " + attrs + " }: \n  " + ports.mkString(",\n  ") + "\n)"
  }

  def inPortsList(): List[Port] = {
    inPorts().values.flatten.toList
  }

  def outPortsList(): List[Port] = {
    outPorts().values.flatten.toList
  }

  def nInDataPorts(): Int = {
    inDataPorts().size
  }

  def nOutDataPorts(): Int = {
    outDataPorts().size
  }

  def inDataPorts(): List[Port] = {
    this
      .inPortsList()
      .filter(
        p => (p.pm.isInstanceOf[PMData]) && (p.pmw.pb.isImpl() && !p.pmw.pb.isHandshake())
      )
  }

  def outDataPorts(): List[Port] = {
    this
      .outPortsList()
      .filter(
        p => (p.pm.isInstanceOf[PMData]) && (p.pmw.pb.isImpl() && !p.pmw.pb.isHandshake())
      )
  }

  def values: Seq[PortNodeID] = {
    ports.map(_._2).flatten.filter(_.id.pt == PTOutput).map(_.nodeID()).toSeq
  }

  def targets: Seq[PortNodeID] = {
    ports.map(_._2).flatten.filter(_.id.pt == PTInput).map(_.nodeID()).toSeq
  }

  def sinks(value: PortNodeID): Set[PortNodeID] = {
    assert((value.nodeName == this.name) && (value.pId.pt == PTOutput), value)

    this.ports(value.pId).filter(_.loc == value.loc).head.distPorts.map(_._2).map(_.nodeID()).toSet
  }

  def sources(value: PortNodeID): Set[PortNodeID] = {
    assert((value.nodeName == this.name) && (value.pId.pt == PTInput), value)

    this.ports(value.pId).filter(_.loc == value.loc).head.distPorts.map(_._2).map(_.nodeID()).toSet
  }

  def getPort(pId: PortNodeID): Port = {
    this.ports(pId.pId).filter(_.loc == pId.loc).head
  }

  def getPin(pin: Pin): Port = {
    this.ports(pin.id).filter(_.loc == pin.loc).head
  }
}

case class PortNodeID(nodeName: String, pId: BlockPortID, loc: Int) extends Assignable {
  override def toString(): String = {
    nodeName + "." + pId.detailedString() + "[" + loc + "]"
  }
}

object Port {
  def emptyPort = Port(BlockPortID.empty, "", Map(), "", Map(), 0)

  // remap nodes pointing to p to map to np
  def updateDistPorts(p: Port, nP: Port): Unit = {
    nP.distPorts.foreach {
      (dpid: PortNodeID, dp: Port) =>
        {
          dp.distPorts = dp.distPorts.toList.map {
            (id: PortNodeID, dpdp: Port) =>
              {
                if (id == p.nodeID()) {
                  (nP.nodeID(), nP)
                } else {
                  (id, dpdp)
                }
              }
          }.toMap
        }
    }
  }

  def rewire(from: Port, oldP: Port, newP: Port): Unit = {
    from.distPorts = from.distPorts.map {
      (id, dp) =>
        {
          if (id == oldP.nodeID()) {
            (newP.nodeID(), newP)
          } else {
            (id, dp)
          }
        }
    }
  }
}

// TODO distPorts should be local, so, just the set of PortNodeIDs
// TODO and if so we should be able to make it immutable
// TODO Attr and name should go away
// TODO thisNode should be a pointer on the node itself, with the overriden hashcode and equals
case class Port(
  id: BlockPortID,
  name: String,
  attr: Map[String, String],
  thisNode: String,
  var distPorts: Map[PortNodeID, Port],
  loc: Int// Loc within all equivalent BlockPortID, given by MLIR
) {
  val width = id.width
  val pt = id.pt
  val pmw = id.pmw
  val pm = id.pmw.pm
  val pb = id.pmw.pb
  val dummy = id.dummy

  // TODO really not sure about this....
  override def hashCode(): Int = this.nodeID().hashCode()

  override def equals(x: Any): Boolean = {
    if (x.isInstanceOf[Port]) {
      val p1 = x.asInstanceOf[Port]
      this.nodeID() == p1.nodeID()
    } else {
      false
    }
  }

  def flipped = Port(id.flipped(), name, attr, thisNode, distPorts, loc)
  def toDummy = Port(id.toDummy, name, attr, thisNode, distPorts, loc)

  def stringRep(): String = thisNode + "." + id.toString() + "[" + loc + "]"

  def stringRep(indent: String) = thisNode + "." + DotPrinter.printId(id) + "[" + loc + "]"

  override def toString(): String = {
    val pmS = id.pmw.pm.str
    val isHs = id.pmw.pb
    val ptS = if (pt == PTInput) "in" else "out"
    val dum = dummy.str

    assert(pm.matcher.isEmpty)

    stringRep()
  }

  def nodeID(): PortNodeID = {
    PortNodeID(thisNode, id, loc)
  }

  def withId(nId: BlockPortID): Port = {
    val nP = Port(nId, name, attr, thisNode, distPorts, loc)
    Port.updateDistPorts(this, nP)
    nP
  }

  def withNodeName(nName: String): Port = {
    val nP = Port(id, name, attr, nName, distPorts, loc)
    Port.updateDistPorts(this, nP)
    nP
  }

  def withAttr(key: String, value: String): Port = {
    val nP = Port(id, name, attr + (key -> value), thisNode, distPorts, loc)
    Port.updateDistPorts(this, nP)
    nP
  }

  def copyWithLoc(nLoc: Int): Port = {
    val nP = Port(id, name, attr, thisNode, distPorts, nLoc)
    Port.updateDistPorts(this, nP)
    nP
  }

  def isDataPort(): Boolean = {
    this.pmw.pb.isImpl() || (!this.pmw.pb.isHandshake())
  }

  def isHandshakePort(): Boolean = {
    this.pmw.pb.isImpl() || this.pmw.pb.isHandshake()
  }

  def nonEmptyUnder(arch: Arch): Boolean = {
    arch.contains(this.id)
  }

  def isLinkedToExitNode(graph: ElasticGraph): Boolean = {
    if (distPorts.isEmpty) {
      false
    } else {
      distPorts
        .map(_._2)
        .map {
          (dp: Port) =>
            {
              graph.nodes(dp.thisNode).nType == archs.Exit
            }
        }
        .reduce(_ || _)
    }
  }

  def isLinkedToEntryNode(graph: ElasticGraph): Boolean = {
    if (distPorts.isEmpty) {
      false
    } else {
      distPorts
        .map(_._2)
        .map {
          (dp: Port) =>
            {
              graph.nodes(dp.thisNode).nType == archs.Entry
            }
        }
        .reduce(_ || _)
    }
  }
}
