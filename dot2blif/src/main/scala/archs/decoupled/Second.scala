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
import arch.AbsMode
import arch.Tile
import arch.TileAnd
import arch.PortMeaning
import arch.Arch
import arch.PortType
import core._
import arch.PTUndef
import arch.LocatedBlockPortID
import arch.LocType
import arch.DagId

case object SecondConfig extends VPRConfig {
  override val capacity = 3 // 1
  override val loc = Fill(20)
  override val pinLocation = Split
  override val height = 1
  override val width = 1
  override val switchblockPattern = SWExtFullIntStraight
}

object Second extends TileGen {
  import ArchGeneratorUtils._

  val outForkSize = 8
  val nCst32 = outForkSize / 4
  val TEHBSize = 7

  // TODO deprecate and remove this...
  case class TileParams(full: Boolean, onlyBranches: Boolean, onlyMuxs: Boolean) extends Params {
    def unpackLibRep(s: String): Params = ???
    protected def config: archs.ParamsConfig = ???
    def recursivelyExtendPrefix: Boolean = ???
  }

  object Mx extends Namer[MuxParams, TileParams] {
    def apply(p: MuxParams, mPrimP: Option[PrimitiveParams] = None): ArchGenerator[TileParams] = {
      (baseName: String, modifier: String, tp: TileParams) =>
        {
          val csts = (c: TileCombinator) => {
            val inLocPre0 = getLoc(0, PTInput, PMData(None), Hs, 0, "in0" + modifier)
            val inLocPre1 = getLoc(0, PTInput, PMData(None), Hs, 0, "in1" + modifier)

            val inLocPost0 = getLoc(p.width, PTOutput, PMData(None), Hs, 0, "in0" + modifier)
            val inLocPost1 = getLoc(p.width, PTOutput, PMData(None), Hs, 0, "in1" + modifier)

            val cstsName = "Csts" + name(baseName, modifier)
            val lName = "L" + name(baseName, modifier)
            val rName = "R" + name(baseName, modifier)

            val llocConfig = (inLocPre0 :: inLocPost0 :: Nil).toMap
            val rlocConfig = (inLocPre1 :: inLocPost1 :: Nil).toMap

            val lPrimP = Some(PrimitiveParams(llocConfig, Set(), Map()))
            val rPrimP = Some(PrimitiveParams(rlocConfig, Set(), Map()))

            val lcst = Common.SrcCst(lPrimP)(ConstantParams(p.width))("SrcCst", lName, tp)
            val rcst = Common.SrcCst(rPrimP)(ConstantParams(p.width))("SrcCst", rName, tp)

            val bufs = {
              val inLocPre0Hs = getLoc(p.width, PTInput, PMData(None), Hs, 0, "in0" + modifier)
              val outLocPost0Hs = getLoc(p.width, PTOutput, PMData(None), Hs, 0, "in0" + modifier)

              val inLocPre0D = getLoc(p.width, PTInput, PMData(None), D, 0, "in0" + modifier)
              val outLocPost0D = getLoc(p.width, PTOutput, PMData(None), D, 0, "in0" + modifier)

              val bufLocConfig = (inLocPre0Hs :: outLocPost0Hs :: Nil).toMap
              val primP = PrimitiveParams(bufLocConfig, Set(AMoleculeRoot), Map())

              Common.buffers(
                TEHBParams(p.width, TEHBSize),
                primP,
                OEHBParams(p.width, 1),
                primP,
                Set()
              )("MxSlct" + baseName, "bufs" + modifier, tp)
            }

            val lcstbufs = (c: TileCombinator) => {
              chain("lcstbufs" + p.width, lcst :: bufs :: Nil, c)
            }

            if (p.width > 0) {
              and(cstsName, lcstbufs :: rcst :: Nil, c)
            } else {
              and(cstsName, bufs :: Nil, c)
            }
          }

          val mux = (c: TileCombinator) => {
            val inLocPre0 = getLoc(p.width, PTInput, PMData(None), Hs, 0, "in0" + modifier)
            val inLocPre1 = getLoc(p.width, PTInput, PMData(None), Hs, 1, "in1" + modifier)

            val outLocPostHs = getLoc(p.width, PTOutput, PMData(None), Hs, 0, "mxmg" + modifier)
            val outLocPostD = getLoc(p.width, PTOutput, PMData(None), D, 0, "out0")

            val locConfig = (inLocPre0 :: inLocPre1 :: outLocPostD :: outLocPostHs :: Nil).toMap

            val primId = getPrimId(name("mux" + baseName, modifier))
            val primP = mPrimP.fold(PrimitiveParams.empty)(
              mp => mp
            )

            PrimMode(name("mux" + baseName, modifier), c, locConfig, primP.annos, primP.castedPorts, None, Mux(p))
          }

          val select = (c: TileCombinator) => {
            val inLocPre0 = getLoc(p.width, PTInput, PMData(None), Hs, 0, "in0" + modifier)
            val inLocPre1 = getLoc(p.width, PTInput, PMData(None), Hs, 1, "in1" + modifier)

            val outLocPostHs = getLoc(p.width, PTOutput, PMData(None), Hs, 0, "mxmg" + modifier)
            val outLocPostD = getLoc(p.width, PTOutput, PMData(None), D, 0, "out0")

            val locConfig = (inLocPre0 :: inLocPre1 :: outLocPostD :: outLocPostHs :: Nil).toMap

            val primP = mPrimP.fold(PrimitiveParams.empty)(
              mp => mp
            )
            val sP = SelectParams(p.width, p.num)

            PrimMode(
              name("select" + baseName, modifier),
              c,
              locConfig,
              primP.annos,
              primP.castedPorts,
              None,
              Select(sP)
            )
          }

          val muxOrSelect = (c: TileCombinator) => {
            or(name("MuxSelect" + baseName, modifier), mux :: select :: Nil, c, Set())
          }

          val muxCsts = if (p.width > 0) {
            (c: TileCombinator) => chain(name("Sel" + baseName, modifier), csts :: muxOrSelect :: Nil, c)
          } else {
            (c: TileCombinator) => chain(name("Sel" + baseName, modifier), csts :: muxOrSelect :: Nil, c)
          }

          (c: TileCombinator) => {
            chain(name("MxMerge" + baseName + "mbuf", modifier), muxCsts :: Nil, c, Set())
          }
        }
    }
  }

  def dmuxMerges(baseName: String, modifier: String, tp: TileParams): ModeInstantiator = {
    val br = (c: TileCombinator) => {
      val p = BranchParams(0)

      val brInLoc = getLoc(0, PTInput, PMData(None), Hs, 0, "in0" + modifier)
      val brInCHsLoc = getLoc(1, PTInput, PMCond(None), Hs, 0, "brinC" + modifier)
      val brInCDLoc = getLoc(1, PTInput, PMCond(None), D, 0, "brinC" + modifier)
      val brOutLoc0 = getLoc(0, PTOutput, PMData(None), Hs, 0, "in0" + modifier)
      val brOutLoc1 = getLoc(0, PTOutput, PMData(None), Hs, 1, "brOut" + modifier)
      val brLocConfig = (brInLoc :: brInCHsLoc :: brInCDLoc :: brOutLoc0 :: brOutLoc1 :: Nil).toMap

      PrimMode(name("dBr", modifier), c, brLocConfig, Set(AWireizeFirst, AMoleculeRoot), Map(), None, Branch(p))
    }

    val sink = (c: TileCombinator) => {
      val sinkLoc = getLoc(0, PTInput, PMData(None), Hs, 0, "brOut" + modifier)
      val sinkLocConfig = (sinkLoc :: Nil).toMap

      val p = SinkParams(0)

      PrimMode("dsink" + modifier, c, sinkLocConfig, Set(AWireizeFirst, AMoleculeRoot), Map(), None, Sink(p))
    }

    val bpSink = (c: TileCombinator) => or(name("SinkBp", modifier), sink :: bypassInMode :: Nil, c)

    val srcCstBr = (c: TileCombinator) => {
      val brInCHsLoc = getLoc(1, PTOutput, PMCond(None), Hs, 0, "brinC" + modifier)
      val brInCDLoc = getLoc(1, PTOutput, PMCond(None), D, 0, "brinC" + modifier)

      val cCast = Map(
        (getId(1, PTOutput, PMData(None), Hs) -> getId(1, PTOutput, PMCond(None), Hs)),
        (getId(1, PTOutput, PMData(None), D) -> getId(1, PTOutput, PMCond(None), D))
      )

      val cLocConfig = (brInCHsLoc :: brInCDLoc :: Nil).toMap
      val cstP = PrimitiveParams(cLocConfig, Set(AIdentityCrkt), cCast)

      val cCst = Common.Cst(ConstantParams(1), Some(cstP))("brIdCst", modifier, tp)
      val bpCst = (c0: TileCombinator) => {
        or("bpcCst", cCst :: bypassOutMode :: Nil, c0)
      }

      val srcP = PrimitiveParams(Map(), Set(AIdentityCrkt), Map())
      val src = Common.Src(SourceParams(), Some(srcP))("brIdSrcaa", modifier, tp)
      val bpSrc = (c0: TileCombinator) => {
        or("bpcSrc", src :: bypassOutMode :: Nil, c0)
      }

      chain(name("brSrcCstIdCst", modifier), bpSrc :: bpCst :: Nil, c, Set())
    }

    val idBr = (c: TileCombinator) => {
      chain("idBr", srcCstBr :: br :: Nil, c, Set())
    }

    val brSink = (c: TileCombinator) => {
      chain("brSink", idBr :: bpSink :: Nil, c, Set())
    }

    val modes = (0 :: 1 :: 32 :: Nil).map {
      width =>
        {
          val muxParams = MuxParams(width, 2)
          val mx = Mx(muxParams, Some(PrimitiveParams(Map(), Set(AMoleculeRoot), Map())))("" + width, modifier, tp)

          mx :: Nil
        }
    }.flatten

    val muxMgs = (c: TileCombinator) => or(name(baseName, modifier), modes, c)

    val brMuxMgs = (c: TileCombinator) => {
      chain("brMuxMgs", brSink :: muxMgs :: Nil, c, Set())
    }

    val fLocConfigIn = getLoc(0, PTInput, PMData(None), Hs, 0, "mxmg" + modifier)
    val fLocConfig = (fLocConfigIn :: Nil).toMap

    val fCastParams = Common.ForkCastParams(PMData(None), 0, PMData(None), 0)

    val fork = frkCst("MuxMerge", modifier, tp)

    val cstName = baseName + "idcst"
    val cstModifier = "brc" + modifier

    val cCast = Map(
      (getId(1, PTOutput, PMData(None), Hs) -> getId(1, PTOutput, PMCond(None), Hs)),
      (getId(1, PTOutput, PMData(None), D) -> getId(1, PTOutput, PMCond(None), D))
    )

    val cstP = Some(PrimitiveParams(Map(), Set(AIdentityCrkt), cCast))
    val srcCst = Common.SrcCst(cstP)(ConstantParams(1))(cstName + modifier, "idcst", tp)

    val cmerge = (c: TileCombinator) => {
      val cmInLoc0 = getLoc(0, PTInput, PMData(None), Hs, 0, "cmIn0" + modifier)
      val cmInLoc1 = getLoc(0, PTInput, PMData(None), Hs, 1, "cmIn1" + modifier)
      val cmOutLoc0 = getLoc(0, PTOutput, PMData(None), Hs, 0, "cmOut0" + modifier)

      val cmLocConf = (cmInLoc0 :: cmInLoc1 :: cmOutLoc0 :: Nil).toMap

      val cmergeP = CntrlMergeParams(2)
      val primParams = PrimitiveParams(cmLocConf, Set(), Map())

      val primId = getPrimId(name("cmerge" + baseName, modifier))
      val annos: Set[Annotation] = Set(AMoleculeRoot)

      PrimMode(name("cmerge" + baseName, modifier), c, primParams.locConfig, annos, Map(), None, CntrlMerge(cmergeP))
    }

    val cMergeF = (c: TileCombinator) => {
      val cMergeFork = genForkParams (
        ForkParams(0, 2, EagerFork),
        Set(AWireizeLast, AForkNumExits(1)),
        Map(),
        Common.ForkCastParams(PMCond(None), 1, PMCond(None), 1)
      )("cmergeF", modifier, tp)

      chain(name("CMF", modifier), cmerge :: cMergeFork :: Nil, c, Set())
    }

    val forkSrc = source("SrcF", modifier)

    val muxInCst = (c: TileCombinator) => {
      or("CMergeOrCst", srcCst :: cMergeF :: Nil, c, Set())
    }

    val idMuxMgs = (c: TileCombinator) => {
      chain(name("SrcCstMuxMerge", modifier), muxInCst :: brMuxMgs :: Nil, c, Set())
    }

    val srcOrMux = (c: TileCombinator) => {
      or("ForkOrMux", forkSrc :: idMuxMgs :: Nil, c, Set())
    }

    (c: TileCombinator) => chain(name(baseName + "mb", modifier), srcOrMux :: fork :: Nil, c, Set())
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

  def source(baseName: String, modifier: String): ModeInstantiator = {
    (c: TileCombinator) =>
      {
        val fLocConfIn = getLoc(0, PTOutput, PMData(None), Hs, 0, "mxmg" + modifier)
        val locConf = Map((fLocConfIn))

        PrimMode(name(baseName, modifier), c, locConf, Set(), Map(), None, Source(SourceParams()))
      }
  }

  def frkCst(baseName: String, modifier: String, tp: TileParams): ModeInstantiator = {
    val fp = ForkParams(0, outForkSize, EagerFork)
    val fLocConfIn = getLoc(0, PTInput, PMData(None), Hs, 0, "mxmg" + modifier)
    val fLocConfOut = (0 until outForkSize)
      .map(
        i => getLoc(0, PTOutput, PMData(None), Hs, i, "out" + i)
      )
      .toList
    val fLocConfig = (fLocConfIn :: fLocConfOut).toMap
    val fCastParams = Common.ForkCastParams(PMData(None), 0, PMData(None), 0)

    val annos: Set[Annotation] = Set(AWireizeFirst, AMoleculeRoot, AForkNumExits(outForkSize))

    val frk = genForkParams(fp, annos, fLocConfig, fCastParams)(baseName + "fork", modifier, tp)

    val csts32 = (0 until nCst32).map {
      i =>
        {
          val outCst32 = getLoc(32, PTOutput, PMData(None), Hs, 0, "out" + i)
          val inCst32 = getLoc(0, PTInput, PMData(None), Hs, 0, "out" + i)
          val lConf = (inCst32 :: outCst32 :: Nil).toMap

          val rootPCst32 = if (i == 0) {
            Some(PrimitiveParams(lConf, Set(AMoleculeRoot), Map()))
          } else {
            Some(PrimitiveParams(lConf, Set(), Map()))
          }

          val cstBp = Common.Cst(ConstantParams(32), rootPCst32)("DCst32", modifier + i, tp) :: bypassInMode :: Nil
          (c: TileCombinator) => {
            or(name("BpCst32", modifier + i), cstBp, c)
          }
        }
    }

    val csts1 = (0 until (outForkSize - nCst32)).map {
      i =>
        {
          val outCst1 = getLoc(1, PTOutput, PMData(None), Hs, 0, "out" + (i + nCst32))
          val inCst1 = getLoc(0, PTInput, PMData(None), Hs, 0, "out" + (i + nCst32))
          val lConf = (outCst1 :: inCst1 :: Nil).toMap

          val rootPCst1 = if (i == 0) {
            Some(PrimitiveParams(lConf, Set(AMoleculeRoot), Map()))
          } else {
            Some(PrimitiveParams(lConf, Set(), Map()))
          }

          val cstBp = Common.Cst(ConstantParams(1), rootPCst1)("DCst1", modifier + i, tp) :: bypassInMode :: Nil

          (c: TileCombinator) => {
            or(name("BpCst1", modifier + i), cstBp, c)
          }
        }
    }

    val csts = (c: TileCombinator) => and(name("csts", modifier), (csts1 ++ csts32).toList, c, Set())

    (c: TileCombinator) => {
      chain(name(baseName + "_top", modifier), frk :: csts :: Nil, c, Set())
    }
  }

  def danglingComp[P <: Params, T <: Params](p: P, comp: Namer[P, T]) = {
    (baseName: String, modifier: String, tp: T) =>
      {
        val comps = comp(p, Some(PrimitiveParams(Map(), Set(AMoleculeRoot), Map())))(baseName, modifier, tp) :: Nil
        (c: TileCombinator) => and("Dangling" + baseName, comps, c, Set())
      }
  }

  def apply(): Tile = {
    val defTp = TileParams(true, false, false)

    val danglingMuxMerge = dmuxMerges("MuxMerges", "", defTp)

    val second = and(name("Ancillary_t", ""), danglingMuxMerge :: Nil, TileAnd)

    Tile("Ancillary", second :: Nil, Set(ASpanning), SecondConfig)
  }
}
