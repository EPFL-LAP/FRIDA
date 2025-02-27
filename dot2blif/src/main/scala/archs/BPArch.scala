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

case object BPArchConfig extends VPRConfig {
  override val capacity = 1
  override val loc = Fill(30)
  override val pinLocation = Split
  override val height = 1
  override val width = 1
  override val switchblockPattern = SWExtFullIntStraight
}

object BPArch extends TileGen {
  import ArchGeneratorUtils._
  import Common._

  def apply(): Tile = {
    val multP = MultParams(32)
    val mult = (c: TileCombinator) => {
      PrimMode("multiplier", c, Map(), Set(AMoleculeRoot), Map(), None, Mult(multP))
    }

    val bpMult = (c: TileCombinator) => {
      or("bpMult", mult :: bypassOutMode :: Nil, c)
    }

    val divP = DivParams(32)
    val div = (c: TileCombinator) => {
      PrimMode("divider", c, Map(), Set(AMoleculeRoot), Map(), None, Div(divP))
    }

    val bpDiv = (c: TileCombinator) => {
      or("bpDiv", div :: bypassOutMode :: Nil, c)
    }

    val opP0 = OperatorParams(32, ALUOperation.anyop)
    val op0 = (c: TileCombinator) => {
      PrimMode("ALU0", c, Map(), Set(AMoleculeRoot), Map(), None, Operator(opP0))
    }

    val bpOp0 = (c: TileCombinator) => {
      or("bpOp0", op0 :: bypassOutMode :: Nil, c)
    }

    val opP1 = OperatorParams(32, ALUOperation.anyop)
    val op1 = (c: TileCombinator) => {
      PrimMode("ALU1", c, Map(), Set(AMoleculeRoot), Map(), None, Operator(opP1))
    }

    val bpOp1 = (c: TileCombinator) => {
      or("bpOp1", op1 :: bypassOutMode :: Nil, c)
    }

    val opP2 = OperatorParams(32, ALUOperation.anyop)
    val op2 = (c: TileCombinator) => {
      PrimMode("ALU2", c, Map(), Set(AMoleculeRoot), Map(), None, Operator(opP2))
    }

    val bpOp2 = (c: TileCombinator) => {
      or("bpOp2", op2 :: bypassOutAnyMode :: Nil, c)
    }

    val left = (c: TileCombinator) => {
      chain("left", bpDiv :: bpOp0 :: Nil, c)
    }

    val right = (c: TileCombinator) => {
      chain("right", bpMult :: bpOp1 :: Nil, c)
    }

    val top = (c: TileCombinator) => {
      and("top", left :: right :: Nil, c)
    }

    val all = (c: TileCombinator) => {
      chain("all", top :: bpOp2 :: Nil, c)
    }

    Tile("BPArch", all(TileAnd) :: Nil, Set(ASpanning), PackSimpleConfig)
  }
}

