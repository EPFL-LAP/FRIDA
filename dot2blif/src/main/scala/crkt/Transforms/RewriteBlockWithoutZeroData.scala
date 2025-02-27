package arch

import core.PatternRewriter

object RewriteBlockWithoutZeroData {
  def keepBlockPort(bp: BlockPort): Boolean = {
    (bp.id.pmw.pb == Impl) || (bp.id.pmw.pb == Hs) || (bp.id.width > 0)
  }

  def apply(block: TBlock): TBlock = {
    val nPorts = block.blockInterface.ports
      .map(_._2)
      .filter(keepBlockPort(_))
      .map(
        bp => (bp.id, bp)
      )
      .toMap
    val nBi = BlockInterface(nPorts, block.blockInterface.pbName, block.blockInterface.clocked)

    Block(block.prim, nBi, block.physicalInfo, block.annotations, block.namedAttrs)
  }
}
