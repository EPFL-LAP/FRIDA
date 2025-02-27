package archs

import arch._
import core.AMoleculeRoot
import core.AIo

case object DupIoVPRConfig extends VPRConfig {
  override val capacity = 4
  override val loc = Perimeter(50)
  override val pinLocation = Dup
  override val height = 1
  override val width = 1
  override val switchblockPattern = SWExtFullIntStraight
}

case object IoTile extends TileGen {
  import ArchGeneratorUtils._

  def entryMode(ep: EntryParams): ModeInstantiator = {
    val inst = (c: TileCombinator) => {
      PrimMode(Entry(ep).vprNameString, c, Map(), Set(AMoleculeRoot), Map(), None, Entry(ep))
    }

    (c: TileCombinator) => and(Entry(ep).vprNameString + "mol", inst :: Nil, c, Set())
  }

  def exitMode(ep: ExitParams): ModeInstantiator = {
    val inst = (c: TileCombinator) => {
      PrimMode(Exit(ep).vprNameString, c, Map(), Set(AMoleculeRoot), Map(), None, Exit(ep))
    }

    (c: TileCombinator) => and(Exit(ep).vprNameString + "mol", inst :: Nil, c, Set())
  }

  def apply(): Tile = {
    val in0 = entryMode(EntryParams(0, Set(Impl)))
    val in1 = entryMode(EntryParams(1, Set(Impl)))
    val in32 = entryMode(EntryParams(32, Set(Impl)))

    val out0 = exitMode(ExitParams(0, Set(Impl)))
    val out1 = exitMode(ExitParams(1, Set(Impl)))
    val out32 = exitMode(ExitParams(32, Set(Impl)))

    val t = and("ios", in0 :: in1 :: in32 :: out0 :: out1 :: out32 :: Nil, TileAnd, Set())
    Tile("ioTile", t :: Nil, Set(AIo), DupIoVPRConfig)
  }
}
