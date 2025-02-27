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
import arch.Arch
import arch.PortType
import core._
import arch.PTUndef
import arch.LocatedBlockPortID
import arch.LocType

case object ThirdConfig extends VPRConfig {
  override val capacity = 2 // 1
  override val loc = Region(30, "2", "2", "2", "1", "2", "2", "2", "1") // Col(30, "3", "4", "1", "h")
  override val pinLocation = Split
  override val height = 1 // 2
  override val width = 1
  override val switchblockPattern = SWExtFullIntStraight
}

object Third extends TileGen {
  import ArchGeneratorUtils._

  // TODO deprecate and remove this...
  case class TileParams(full: Boolean, onlyBranches: Boolean, onlyMuxs: Boolean) extends Params {
    def unpackLibRep(s: String): Params = ???
    protected def config: archs.ParamsConfig = ???
    def recursivelyExtendPrefix: Boolean = ???
  }

  case class ArithComp[P <: ParamsWithWidth, TP <: Params](comp: Namer[P, TP]) extends Namer[P, TP] {
    def apply(p: P, primP: Option[PrimitiveParams] = None): ArchGenerator[TP] = {
      (baseName: String, modifier: String, tp: TP) =>
        {
          (c: TileCombinator) =>
            {
              val cstsName = "Csts" + name(baseName, modifier)
              val lName = "L" + name(baseName, modifier)
              val rName = "R" + name(baseName, modifier)

              val lPrimP = Some(PrimitiveParams(Map(), Set(), Map()))
              val lcst = Common.SrcCst(lPrimP)(ConstantParams(p.width))("SrcCst", lName, tp)

              val rPrimP = Some(PrimitiveParams(Map(), Set(), Map()))
              val rcst = Common.SrcCst(rPrimP)(ConstantParams(p.width))("SrcCst", rName, tp)

              val andComps = if (p.isInstanceOf[OperatorParams] && (p.asInstanceOf[OperatorParams].width == 32)) {
                val aluBufsPre =
                  genBufParams(32, PMData(None), PMData(None), Map())(baseName + "alu_bufsprer", modifier, tp)

                val rPart = (c0: TileCombinator) =>
                  chain(name(baseName + "rpart", "bp"), rcst :: aluBufsPre :: Nil, c0, Set())
                lcst :: rPart :: Nil
              } else {
                lcst :: rcst :: Nil
              }

              val csts = (c0: TileCombinator) => and(cstsName, andComps, c0)

              val cgLocr = getLoc(p.width, PTInput, PMData(None), Hs, 1, "arithr" + modifier)
              val cgLocl = getLoc(p.width, PTInput, PMData(None), Hs, 0, "arithl" + modifier)

              val compLocConf = (cgLocl :: cgLocr :: Nil).toMap
              val compPrimP = if (primP.isEmpty) {
                Some(PrimitiveParams(Map(), Set(), Map()))
              } else {
                Some(PrimitiveParams(primP.get.locConfig, primP.get.annos, primP.get.castedPorts))
              }

              val compInst = comp(p, compPrimP)(baseName, modifier, tp)

              val bpComp = if (p.isInstanceOf[ComparatorParams]) {
                compInst
              } else {
                (c0: TileCombinator) => or(name(baseName, "bp"), compInst :: bypassOutMode :: Nil, c0, Set())
              }

              chain(name("Op" + baseName, modifier), csts :: bpComp :: Nil, c)
            }
        }
    }
  }

  def genBufParams[TP <: Params](
      width: Int,
      inPM: PortMeaning,
      outPM: PortMeaning,
      locConfig: Map[LocatedBlockPortID, LocType],
      annos: Set[Annotation] = Set()
  ): ArchGenerator[TP] = {
    (baseName: String, modifier: String, tp: TP) =>
      {
        val fName = name(baseName, "bufs" + modifier)

        val castedPorts = Map(
          (getId(width, PTInput, PMData(None), Hs)
            -> getId(width, PTInput, inPM, Hs)),
          (getId(width, PTOutput, PMData(None), Hs)
            -> getId(width, PTOutput, outPM, Hs)),
          (getId(width, PTInput, PMData(None), D)
            -> getId(width, PTInput, inPM, D)),
          (getId(width, PTOutput, PMData(None), D)
            -> getId(width, PTOutput, outPM, D))
        )

        val obufPrimP = PrimitiveParams(locConfig, Set(), castedPorts)
        val tbufPrimP = PrimitiveParams(locConfig, Set(), castedPorts)

        Common.buffers(TEHBParams(width, 1), tbufPrimP, OEHBParams(width, 1), obufPrimP, annos)(
          baseName,
          "bufs" + modifier,
          tp
        )
      }
  }

  def genForkParams[TP <: Params](
      fp: ForkParams,
      annos: Set[Annotation],
      locConfig: Map[LocatedBlockPortID, LocType],
      cp: Common.ForkCastParams
  ): ArchGenerator[TP] = {
    (baseName: String, modifier: String, tp: TP) =>
      {
        val fName = name(baseName + "Fork", modifier)

        val castedPorts = if (annos contains AWireizeFirst) {
          Map(
            (getId(fp.width, PTInput, PMData(None), Hs)
              -> getId(cp.inW, PTInput, cp.inPM, Hs)),
            (getId(fp.width, PTOutput, PMData(None), Hs)
              -> getId(cp.outW, PTOutput, cp.outPM, Hs))
          )
        } else {
          Map(
            (getId(fp.width, PTInput, PMData(None), Hs)
              -> getId(cp.inW, PTInput, cp.inPM, Hs)),
            (getId(fp.width, PTOutput, PMData(None), Hs)
              -> getId(cp.outW, PTOutput, cp.outPM, Hs)),
            (getId(fp.width, PTInput, PMData(None), D)
              -> getId(cp.inW, PTInput, cp.inPM, D)),
            (getId(fp.width, PTOutput, PMData(None), D)
              -> getId(cp.outW, PTOutput, cp.outPM, D))
          )
        }

        val forkPrimP = PrimitiveParams(locConfig, annos, castedPorts)
        Common.fork(fp, Some(forkPrimP))(fName, modifier, tp)
      }
  }

  object Icmp extends Namer[ComparatorParams, TileParams] {
    def apply(p: ComparatorParams, iPrimP: Option[PrimitiveParams] = None): ArchGenerator[TileParams] = {
      (baseName: String, modifier: String, tp: TileParams) =>
        {
          (c: TileCombinator) =>
            {
              val primP = iPrimP.fold(PrimitiveParams.empty)(
                ip => ip
              )

              PrimMode(
                name(baseName, modifier),
                c,
                primP.locConfig,
                primP.annos,
                primP.castedPorts,
                None,
                Comparator(p)
              )
            }
        }
    }
  }

  object Multiplier extends Namer[MultParams, TileParams] {
    def apply(p: MultParams, mPrimP: Option[PrimitiveParams] = None): ArchGenerator[TileParams] = {
      (baseName: String, modifier: String, tp: TileParams) =>
        {
          (c: TileCombinator) =>
            {
              val primP = mPrimP.fold(PrimitiveParams.empty)(
                mp => mp
              )
              PrimMode(name(baseName, modifier), c, primP.locConfig, primP.annos, primP.castedPorts, None, Mult(p))
            }
        }
    }
  }

  object Op extends Namer[OperatorParams, TileParams] {
    def apply(p: OperatorParams, oPrimP: Option[PrimitiveParams] = None): ArchGenerator[TileParams] = {
      (baseName: String, modifier: String, tp: TileParams) =>
        {
          (c: TileCombinator) =>
            {
              val primP = oPrimP.fold(PrimitiveParams.empty)(
                op => op
              )
              PrimMode(name(baseName, modifier), c, primP.locConfig, primP.annos, primP.castedPorts, None, Operator(p))
            }
        }
    }
  }

  def apply(): Tile = {
    val defTp = TileParams(true, false, false)

    val outHs = getLoc(1, PTOutput, PMData(None), Hs, 0, "outHs")
    val inHs = getLoc(1, PTInput, PMData(None), Hs, 0, "outHs")
    val icmpLoc = (outHs :: Nil).toMap
    val bufsLoc = (inHs :: outHs :: Nil).toMap

    val multBufsPrel = genBufParams(32, PMData(None), PMData(None), Map())("mult_bufsprel", "", defTp)
    val multBufsPrer = genBufParams(32, PMData(None), PMData(None), Map())("mult_bufsprer", "", defTp)
    val mutPre = (c: TileCombinator) => and("multbufspre", multBufsPrel :: multBufsPrer :: Nil, c, Set())

    val multP = Some(PrimitiveParams(Map(), Set(AMoleculeRoot), Map()))
    val mult = ArithComp(Multiplier)(MultParams(32), multP)("Mult", "", defTp)
    val multBufs = genBufParams(32, PMData(None), PMData(None), Map())("mult_bufs", "", defTp)

    val aluP = Some(PrimitiveParams(Map(), Set(AMoleculeRoot), Map()))
    val alu = ArithComp(Op)(OperatorParams(32, ALUOperation.anyop), aluP)("Alu", "", defTp)
    val aluBufs = genBufParams(32, PMData(None), PMData(None), Map())("alu_bufs", "", defTp)

    val alu1P = Some(PrimitiveParams(icmpLoc, Set(AMoleculeRoot), Map()))
    val alu1 = ArithComp(Op)(OperatorParams(1, ALUOperation.anyop), alu1P)("Alu1", "", defTp)

    val icmpCast = Map(
      getId(1, PTOutput, PMCond(None), Hs) -> getId(1, PTOutput, PMData(None), Hs),
      getId(1, PTOutput, PMCond(None), D) -> getId(1, PTOutput, PMData(None), D)
    )

    val icmpP = Some(PrimitiveParams(icmpLoc, Set(AMoleculeRoot, ALogicalMode), icmpCast))
    val icmp = ArithComp(Icmp)(ComparatorParams(32, CmpAnyComp), icmpP)("Icmpp", "", defTp)
    val icmpBufs = genBufParams(1, PMData(None), PMData(None), bufsLoc)("icmp_bufs", "", defTp)

    val outHsF = getLoc(32, PTOutput, PMData(None), Hs, 0, "outHs")
    val frkLoc = (outHsF :: Nil).toMap
    val frk = genForkParams(
      ForkParams(0, 1, EagerFork),
      Set(AWireizeLast, AForkNumExits(0)),
      frkLoc,
      Common.ForkCastParams(PMData(None), 32, PMData(None), 32)
    )("dummyFork", "", defTp)

    val dFrk = (c: TileCombinator) => and(name("dFork", ""), frk :: Nil, c, Set())
    val ddFrk = (c: TileCombinator) => and(name("ddFork", ""), dFrk :: Nil, c, Set())
    val dicmp = (c: TileCombinator) => or(name("ddicmp", ""), icmp :: alu1 :: Nil, c, Set())
    val bpIcmp = (c: TileCombinator) => or(name("DIcmp", ""), dicmp :: ddFrk :: Nil, c, Set())

    val dbpicmp = (c: TileCombinator) => and(name("DbpIcmp", ""), bpIcmp :: Nil, c, Set())

    val inHsOut = getLoc(0, PTInput, PMData(None), Hs, 0, "outHs")
    val outForkLoc = (inHsOut :: Nil).toMap

    val outFSize = 8
    val outFork = genForkParams(
      ForkParams(0, outFSize, EagerFork),
      Set(AWireizeFirst, AForkNumExits(outFSize)),
      outForkLoc,
      Common.ForkCastParams(PMData(None), 0, PMData(None), 0)
    )("outF", "", defTp)

    val modes = mutPre :: mult :: multBufs :: alu :: aluBufs :: dbpicmp :: icmpBufs :: outFork :: Nil
    val bpMultBufs = (c: TileCombinator) => chain(name("Arith", ""), modes, c, Set())

    val comps = bpMultBufs :: Nil

    val t = or("compute", comps, TileAnd)

    Tile("Arith", t :: Nil, Set(ASpanning), ThirdConfig)
  }
}
