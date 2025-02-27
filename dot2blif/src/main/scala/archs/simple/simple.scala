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

case object SimpleConfig extends VPRConfig {
  override val capacity = 1
  override val loc = Fill(30)
  override val pinLocation = Split
  override val height = 1
  override val width = 1
  override val switchblockPattern = SWExtFullIntStraight
}

object Simple extends TileGen {
  import ArchGeneratorUtils._
  import Common._

  def getComponent(comp: Primitive, baseName: String, modifier: String, annos: Set[Annotation]): ModeInstantiator = {
    val bName = comp.name.replace(":", "")
    val primId = getPrimId(name(bName, modifier))
    val inst = (c: TileCombinator) => {
      PrimMode(comp.name, c, Map(), annos, Map(), None, comp)
    }

    (c: TileCombinator) => and(name(bName + "_mol", baseName), inst :: Nil, c, Set())
  }

  def apply(): Tile = {
    val brP = BranchParams(0)
    val br = getComponent(Branch(brP), "", "", Set(AWireizeFirst, AMoleculeRoot))

    val frkP = ForkParams(0, 8, EagerFork)
    val frk = getComponent(Fork(frkP), "", "", Set(AWireizeFirst, AMoleculeRoot, AForkNumExits(8)))

    val muxMergeBufs = (0 :: 1 :: 32 :: Nil).map {
      width =>
        {
          val mergeP = MergeParams(width, 2)
          val merge = getComponent(Merge(mergeP), "", "", Set(AMoleculeRoot))

          val muxP = MuxParams(width, 2)
          val mux = getComponent(Mux(muxP), "", "", Set(AMoleculeRoot))

          val tehbP = TEHBParams(width, 1)
          val tehb = getComponent(TEHB(tehbP), "", "", Set(AMoleculeRoot))

          val oehbP = OEHBParams(width, 1)
          val oehb = getComponent(OEHB(oehbP), "", "", Set(AMoleculeRoot))

          merge :: mux :: tehb :: oehb :: Nil
        }
    }.flatten

    val cntrlMergeP = CntrlMergeParams(2)
    val cntrlMerge = getComponent(CntrlMerge(cntrlMergeP), "", "", Set(AMoleculeRoot))

    val source = getComponent(Source(SourceParams()), "", "", Set(AMoleculeRoot))
    val csts = (1 :: 32 :: Nil).map {
      width =>
        {
          val cstP = ConstantParams(width)
          getComponent(Constant(cstP), "", "", Set(AMoleculeRoot))
        }
    }

    val sinkp = SinkParams(0)
    val sink = getComponent(Sink(sinkp), "", "", Set(AWireizeFirst, AMoleculeRoot))

    val icmpP = ComparatorParams(32, CmpAnyComp)
    val icmp = getComponent(Comparator(icmpP), "", "", Set(AMoleculeRoot))

    val opP = OperatorParams(32, ALUOperation.anyop)
    val op = getComponent(Operator(opP), "", "", Set(AMoleculeRoot))

    val multP = MultParams(32)
    val mult = getComponent(Mult(multP), "", "", Set(AMoleculeRoot))

    val allComps = br :: frk :: cntrlMerge :: source :: sink :: icmp :: op :: mult :: (muxMergeBufs ++ csts)

    val t = or("simple_m", allComps, TileAnd, Set())

    Tile("simple", t :: Nil, Set(ASpanning), SimpleConfig)
  }
}
