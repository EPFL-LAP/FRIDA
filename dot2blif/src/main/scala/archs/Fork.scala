package archs

import arch.TileCombinator
import arch.PTOutput
import arch.PTInput
import arch.PMData
import arch.PMCond
import arch.Hs
import arch.D
import arch.Mode
import arch.PrimMode
import arch.Tile
import arch.TileAnd
import arch.PortMeaning
import core._

case object ForkTileConfig extends VPRConfig {
  override val capacity = 1
  override val loc = Fill(30)
  override val pinLocation = Split
  override val height = 1
  override val width = 1
  override val switchblockPattern = SWExtFullIntStraight
}

object ForkTile extends TileGen {
  import ArchGeneratorUtils._
  import Common._

  def apply(): Tile = {
    val fP = ForkParams(0, 16, EagerFork)
    val fork = (c: TileCombinator) => {
      PrimMode("Fork", c, Map(), Set(AMoleculeRoot, AForkNumExits(16)), Map(), None, Fork(fP))
    }

    Tile("ForkTile", fork(TileAnd) :: Nil, Set(ASpanning), PackSimpleConfig)
  }
}
