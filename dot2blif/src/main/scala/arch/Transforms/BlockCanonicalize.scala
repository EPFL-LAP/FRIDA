package arch

object BlockCanonicalize {
  def apply(blocks: Map[String, TBlock]): Map[String, TBlock] = {
    blocks.map {
      (bName, block) =>
        {
          val nBlock = block.canonicalize()

          (nBlock.name, nBlock)
        }
    }
  }
}
