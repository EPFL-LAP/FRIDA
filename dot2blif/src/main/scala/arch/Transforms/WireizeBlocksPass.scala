package arch

import core.AWirePort
import archs.Params
import archs._
import core.AWireizeLast
import core.AWireizeFirst
import frontend.GlobalParamsInst
import archs.EagerFork

object WireizeBlockPass {
  def getWirePorts(b: TBlock, pb: PortBundle): Set[BlockPortID] = {
    b.prim
      .instantiate(b.prim.p)
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

  def getWireHsPorts(b: TBlock): Set[BlockPortID] = {
    getWirePorts(b: TBlock, Hs)
  }

  def getWireDPorts(b: TBlock): Set[BlockPortID] = {
    getWirePorts(b: TBlock, D)
  }

  // match all params that can be wired, and return with 0 width
  def lowerPrim(prim: Primitive): Primitive = {
    prim match {
      case Branch(p) => Branch(BranchParams(0))
      case Fork(p)   => Fork(ForkParams(0, p.num, p.variant))
      case Sink(p)   => Sink(SinkParams(0))
      case other     => other
    }
  }

  def canonPorts(
      ports: List[BlockPort],
      dWirePorts: Set[BlockPortID],
      hsWirePorts: Set[BlockPortID]
  ): List[BlockPort] = {
    ports
      .filter(
        bp => !dWirePorts.contains(bp.id)
      )
      .map {
        bp =>
          {
            if (hsWirePorts.contains(bp.id)) {
              val nId = BlockPortID(0, bp.id.pt, bp.id.pmw, bp.id.dummy)
              BlockPort(nId, bp.words, bp.annotations)
            } else {
              bp
            }
          }
      }
  }

  def apply(params: GlobalParamsInst, block: TBlock): TBlock = {
    val dWirePorts = getWireDPorts(block)
    val hsWirePorts = getWireHsPorts(block)

    val ports = block.blockInterface.ports.map(_._2).toList
    val nBPs = canonPorts(ports, dWirePorts, hsWirePorts)
      .map(
        bp => (bp.id, bp)
      )
      .toMap

    val nBi = BlockInterface(nBPs, block.blockInterface.pbName, block.blockInterface.clocked)
    val nPrim = lowerPrim(block.prim)

    Block(nPrim, nBi, nPrim.getTimings(params), block.annotations, block.namedAttrs)
  }
}
