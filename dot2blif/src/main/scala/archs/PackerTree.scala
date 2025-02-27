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

case object PackTreeConfig extends VPRConfig {
  override val capacity = 1
  override val loc = Fill(30)
  override val pinLocation = Split
  override val height = 1
  override val width = 1
  override val switchblockPattern = SWExtFullIntStraight
}

object PackTree extends TileGen {
  import ArchGeneratorUtils._
  import Common._

  def apply(): Tile = {
    val opP0 = OperatorParams(32, ALUOperation.anyop)
    val op0 = (c: TileCombinator) => {
      PrimMode("ALU0", c, Map(), Set(AMoleculeRoot), Map(), None, Operator(opP0))
    }

    val bpOp0 = (c: TileCombinator) => {
      or("bpOp0", op0 :: bypassOutMode :: Nil, c)
    }

    val opP1 = OperatorParams(32, ALUOperation.anyop)
    val op1 = (c: TileCombinator) => {
      PrimMode("ALU1", c, Map(), Set(), Map(), None, Operator(opP1))
    }

    val bpOp1 = (c: TileCombinator) => {
      or("bpOp1", op1 :: bypassOutMode :: Nil, c)
    }

    val opP2 = OperatorParams(32, ALUOperation.anyop)
    val op2 = (c: TileCombinator) => {
      PrimMode("ALU2", c, Map(), Set(), Map(), None, Operator(opP2))
    }

    val bpOp2 = (c: TileCombinator) => {
      or("bpOp2", op2 :: bypassOutMode :: Nil, c)
    }

    val ops = (c: TileCombinator) => chain("ops", bpOp2 :: bpOp1 :: Nil, c, Set())

    val multP = MultParams(32)
    val mult = (c: TileCombinator) => {
      PrimMode("multiplier", c, Map(), Set(AMoleculeRoot), Map(), None, Mult(multP))
    }

    val multBp = (c: TileCombinator) => {
      or("bpmult", mult :: bypassOutMode :: Nil, c)
    }

    val cstP = ConstantParams(32)
    val cst = (c: TileCombinator) => {
      PrimMode("Constant", c, Map(), Set(AMoleculeRoot), Map(), None, SrcConstant(cstP))
    }

    val bpCst = (c: TileCombinator) => {
      or("bpCst", cst :: bypassOutMode :: Nil, c)
    }

    val multCst = (c: TileCombinator) => {
      chain("multCst", bpCst :: multBp :: Nil, c)
    }


    val two = (c: TileCombinator) => {
      and("macadd", multCst :: ops :: Nil, c)
    }

    val all = (c: TileCombinator) => {
      chain("tree", two :: bpOp0 :: Nil, c)
    }

    val tile = (c: TileCombinator) => chain("tile", all :: Nil, c, Set())

    Tile("PackTree", tile(TileAnd) :: Nil, Set(ASpanning), PackSimpleConfig)
  }
}


