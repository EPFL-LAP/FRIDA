package crkt

import arch._
import archs.Primitive
import core.PatternRewriter
import core.AWireizeLast
import core.AWireizeFirst
import archs.Branch
import archs.Fork
import archs.ForkParams
import archs.Sink
import archs.BranchParams
import archs.SinkParams
import archs.Constant
import archs.Mux
import archs.Merge
import archs.MuxParams

import scala.collection.mutable.{Set => MSet, Map => MMap}
import math.max

object RaiseToImpl extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    n.ports.exists(_._1.pmw.pb != Impl)
  }

  def getPrimDataWidth(ports: List[Port]): Int = {
    ports.filter(_.id.pmw.pm.isInstanceOf[PMData]).map(_.width).reduce(max)
  }

  def getPrimDataWidth(n: Node): Int = {
    getPrimDataWidth(n.ports.map(_._2).flatten.toList)
  }

  def getPrimDataWidth(n: Node, p: Port): Int = {
    n.nType match {
      case Constant(_) => getPrimDataWidth(n.ports.map(_._2).flatten.filter(_.id.pt == p.id.pt).toList)
      case other => getPrimDataWidth(n)
    }
  }

  def partiallyRaisedWidth(n: Node): Option[Int] = {
    n.ports.map(_._2)
      .flatten
      .filter(_.id.pmw.pb == Impl)
      .filter(_.id.pmw.pm.isInstanceOf[PMData])
      .map(_.width)
      .reduceOption(max)
  }

  def getOriginalWidth(p: Port, g: ElasticGraph): Option[Int] = {
    val seen = MSet[Port]()

    def followEdge(ap: Port): List[Port] = {
      val recPorts = ap.distPorts.map(_._2).toList
      seen ++= recPorts

      recPorts
    }

    def crossNode(ap: Port): List[Port] = {
      val n = g(ap.thisNode)

      val recPorts = n.ports.map(_._2).flatten
        .filter(_.id.pmw.pm.isInstanceOf[PMData])
        .filter(_.id.pt == ap.id.pt.flipped)
        .map(_.distPorts.map(_._2))
        .flatten
        .filter(!seen.contains(_))

      seen ++= recPorts

      recPorts.toList
    }

    def rec(recP: Port): Option[Int] = {
      val n = g(recP.thisNode)

      val typeAmbiguous = (n.nType.instantiate(n.nType.p).canWire()
        || (n.nType.isInstanceOf[Mux] && n.name.contains("identity")) // TODO fixme, should not be required eventually..
        || (n.nType.isInstanceOf[Merge] && n.name.contains("identity"))
      )

      val notAlreadyVisited = partiallyRaisedWidth(n).isEmpty
      val isAmbiguous = typeAmbiguous && notAlreadyVisited

      if(!recP.id.pmw.pm.isInstanceOf[PMData]){
        Some(recP.width)
      } else if(!isAmbiguous) {
        if(partiallyRaisedWidth(n).nonEmpty) {
          partiallyRaisedWidth(n)
        } else {
          Some(getPrimDataWidth(n, recP))
        }
      } else {
        val widths = if(recP.id.pt == p.id.pt) {
          followEdge(recP).map(rec(_)).flatten
        } else {
          crossNode(recP).map(rec(_)).flatten
        }

        if(widths.isEmpty) {
          None
        } else {
          Some(widths.max)
        }
      }
    }

    rec(p)
  }

  def raisePrimitive(prim: Primitive, nPorts: Map[BlockPortID, List[Port]]): Primitive = {
    val portsDataWidth = getPrimDataWidth(nPorts.map(_._2).flatten.toList)

    prim match {
      case Fork(p) => Fork(ForkParams(portsDataWidth, p.num, p.variant, p.recursivelyExtendPrefix))
      case Branch(p) => Branch(BranchParams(portsDataWidth, p.recursivelyExtendPrefix))
      case Sink(p) => Sink(SinkParams(portsDataWidth, p.recursivelyExtendPrefix))

      case Mux(p) => {
        // TODO If this happens, there is a bug...
        Mux(MuxParams(portsDataWidth, p.num, p.recursivelyExtendPrefix))
      }

      case other => other
    }
  }

  def mapPort(p: Port, g: ElasticGraph): Port = {
    assert(p.id.pmw.pb == Hs || p.id.pmw.pb == Vld || p.id.pmw.pb == Impl)

    val nWidth = getOriginalWidth(p, g)

    if(nWidth.isEmpty) {
      p
    } else {
      val nId = BlockPortID(nWidth.get, p.pt, PortMeaningWrapper(p.pmw.pm, Impl), p.dummy)
      val nP = Port(nId, p.name, p.attr, p.thisNode, p.distPorts, p.loc)

      nP
    }
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    val nPorts = n.ports.map(_._2).flatten.filter {
      p => {
        p.id.pmw.pb match {
          case Impl => true
          case D => false
          case Hs => true
          case Vld => true
          case Rdy => false
        }
      }
    }.map {
      p => {
        val nP = mapPort(p, g)
        Port.updateDistPorts(p, nP)

        nP
      }
    }.toList.groupBy(_.id)

    val nPrim = raisePrimitive(n.nType, nPorts)

    nPrim match {
      case Fork(p) => {
        assert(nPorts.map(_._2).flatten.forall(_.width == p.width),
          n.name + ":\n" + nPorts.map(_._2).flatten.mkString("\n"))
      }

      case others => true
    }

    Node(n.name, nPrim, n.mlirAttr, n.attr, n.annos, nPorts) :: Nil
  }
}

