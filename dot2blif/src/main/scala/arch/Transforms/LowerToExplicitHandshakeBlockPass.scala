package arch

import core.Annotation
import core.ADataOnly
import crkt.LowerToExplicitHandshakeCrkt

object LowerToExplicithandshakeBlock {
  def convertId(id: BlockPortID): (BlockPortID, BlockPortID) = LowerToExplicitHandshakeCrkt.convertId(id)

  def decouple(bp: BlockPort): List[BlockPort] = {
    assert(bp.pmw.pb == Impl)

    val (dId, hsId) = convertId(bp.id)

    val dataBp = BlockPort(dId, bp.words, bp.annotations)
    val hsBp = BlockPort(hsId, bp.words, bp.annotations)

    dataBp :: hsBp :: Nil
  }

  def apply(prim: TBlock): TBlock = {
    val bi = prim.blockInterface
    val nPorts = bi.ports
      .map(_._2)
      .map(decouple(_))
      .flatten
      .map(
        bp => (bp.id, bp)
      )
      .toMap
    val nBi = BlockInterface(nPorts, bi.pbName, bi.clocked)

    Block(prim.prim, nBi, prim.physicalInfo, prim.annotations, prim.namedAttrs)
  }
}
