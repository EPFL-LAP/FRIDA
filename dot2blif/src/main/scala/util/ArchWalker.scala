package util

import arch.PbType

object ArchWalker {
  def pbMapping(blockName: String, fromPb: PbType): Option[PbType] = {
    val filtered = fromPb.subBlocks.filter {
      (subPb: PbType) =>
        {
          subPb.name == blockName
        }
    }

    assert((filtered.size == 1) || (filtered.size == 0))

    filtered.headOption
  }
}
