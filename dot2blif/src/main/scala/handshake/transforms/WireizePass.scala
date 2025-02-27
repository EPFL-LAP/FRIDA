package handshake

import core._
import crkt._
import arch._
import archs.Primitive
import archs.Branch
import archs.BranchParams
import archs.ForkParams
import archs.Fork
import archs.Sink
import archs.SinkParams
import archs.EagerFork

object WireId {
  def apply(p: Port): WireId = WireId(p.id.width, p.id.pmw, p.id.dummy)
}

case class WireId(width: Int, pmw: PortMeaningWrapper, dummy: DummyType)

object WireizePass extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    val wirePorts = getWireDPorts(n)
    n.ports
      .map(_._2)
      .flatten
      .filter(
        p => wirePorts.contains(p.id)
      )
      .nonEmpty
  }

  def getWirePorts(n: TNode, pb: PortBundle): Set[BlockPortID] = {
    n.nType
      .instantiate(n.nType.p)
      .blockInterface
      .ports
      .map(_._2)
      .filter(_.annotations.contains(AWirePort))
      .map {
        bp =>
          {
            BlockPortID(bp.width, bp.pt, PortMeaningWrapper(bp.pm, pb), bp.dummy)
          }
      }
      .toSet
  }

  def getWireHsPorts(n: TNode): Set[BlockPortID] = {
    getWirePorts(n: TNode, Hs)
  }

  def getWireDPorts(n: TNode): Set[BlockPortID] = {
    getWirePorts(n: TNode, D)
  }

  def rewire(n: TNode, wirePorts: Set[BlockPortID]): Unit = {
    n.ports
      .map(_._2)
      .flatten
      .filter(
        p => wirePorts.contains(p.id)
      )
      .groupBy(WireId(_))
      .map {
        (wId, ps) =>
          {
            val (inPort, outPorts) = ps.partition(_.id.pt == PTInput)
            assert(inPort.size == 1, inPort)

            val srcPs = inPort.head.distPorts.map(_._2)
            assert(srcPs.size == 1)

            val srcP = srcPs.head
            val dstPs = outPorts.map(_.distPorts.map(_._2)).flatten

            dstPs.map {
              dp =>
                {
                  dp.distPorts = Map((srcP.nodeID() -> srcP))
                }
            }

            val nSrcPDistPorts = srcP.distPorts.filter(_._1 != inPort.head.nodeID())
            srcP.distPorts = (nSrcPDistPorts ++ dstPs.map(
              dp => (dp.nodeID(), dp)
            )).toMap
          }
      }
  }

  // TODO not great but will do for now...
  def lowerPrim(prim: Primitive): Primitive = {
    prim match {
      case Branch(p) => Branch(BranchParams(0))
      case Fork(p)   => Fork(ForkParams(0, p.num, p.variant))
      case Sink(p)   => Sink(SinkParams(0))
      case other     => scala.sys.error("Unexpected primitive.")
    }
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    val wirePorts = getWireDPorts(n)
    rewire(n, wirePorts)

    val hsWirePorts = getWireHsPorts(n)
    val nPorts = n.ports
      .map(_._2)
      .flatten
      .filter(
        p => !wirePorts.contains(p.id)
      )
      .map {
        p =>
          {
            if (hsWirePorts.contains(p.id)) {
              val nId = BlockPortID(0, p.id.pt, p.id.pmw, p.id.dummy)
              val nP = Port(nId, p.name, p.attr, p.thisNode, p.distPorts, p.loc)

              Port.updateDistPorts(p, nP)

              nP
            } else {
              p
            }
          }
      }
      .toList
      .groupBy(_.id)

    Node(n.name, lowerPrim(n.nType), n.mlirAttr, n.attr, n.annos, nPorts) :: Nil
  }
}
