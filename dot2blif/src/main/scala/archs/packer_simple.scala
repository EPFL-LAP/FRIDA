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

case object PackSimpleConfig extends VPRConfig {
  override val capacity = 1
  override val loc = Fill(30)
  override val pinLocation = Split
  override val height = 1
  override val width = 1
  override val switchblockPattern = SWExtFullIntStraight
}

object PackSimple extends TileGen {
  import ArchGeneratorUtils._
  import Common._

  def apply(): Tile = {
    val cstP = ConstantParams(32)
    val cst = (c: TileCombinator) => {
      PrimMode("Constant", c, Map(), Set(), Map(), None, SrcConstant(cstP))
    }

    val bpCst = (c: TileCombinator) => {
      or("bpCst", cst :: bypassOutMode :: Nil, c)
    }

    val opP = OperatorParams(32, ALUOperation.anyop)
    val op = (c: TileCombinator) => {
      PrimMode("ALU", c, Map(), Set(AMoleculeRoot), Map(), None, Operator(opP))
    }

    val multP = MultParams(32)
    val mult = (c: TileCombinator) => {
      PrimMode("multiplier", c, Map(), Set(AMoleculeRoot), Map(), None, Mult(multP))
    }

    val compute = (c: TileCombinator) => or("compute", op :: mult :: Nil, c, Set())
    val tile = (c: TileCombinator) => chain("tile", bpCst :: compute :: Nil, c, Set())

    Tile("packSimple", tile(TileAnd) :: Nil, Set(ASpanning), PackSimpleConfig)
  }
}
