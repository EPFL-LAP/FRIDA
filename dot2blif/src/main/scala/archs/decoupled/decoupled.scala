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
import core.AForkNumExits

case object ControlConfig extends VPRConfig {
  override val capacity = 1
  override val loc = Region(30, "1", "1", "2", "1", "1", "1", "2", "1") // Col(30, "1", "4", "1", "h")
  override val pinLocation = Split
  override val height = 1 // 4
  override val width = 1
  override val switchblockPattern = SWExtFullIntStraight
}

object Control extends TileGen {
  import ArchGeneratorUtils._
  import Common._

  val bufferDepth = 1
  val outForkSize = 4

  // TODO deprecate and remove this...
  case class TileParams(full: Boolean, onlyBranches: Boolean, onlyMuxs: Boolean) extends Params {
    def unpackLibRep(s: String): Params = ???
    protected def config: archs.ParamsConfig = ???
    def recursivelyExtendPrefix: Boolean = ???
  }

  // TODO deprecate this and make cast useless by making everything go through matchers...
  def genForkParams[TP <: Params](
      fp: ForkParams,
      annos: Set[Annotation],
      locConfig: Map[LocatedBlockPortID, LocType],
      cp: ForkCastParams
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
        fork(fp, Some(forkPrimP))(fName, modifier, tp)
      }
  }

  def genBufParams[TP <: Params](
      width: Int,
      inPM: PortMeaning,
      outPM: PortMeaning,
      locConfig: Map[LocatedBlockPortID, LocType],
      bothLocs: Boolean
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

        val a = if (bothLocs) {
          locConfig
        } else {
          Map()
        }

        val obufPrimP = PrimitiveParams(a, Set(), castedPorts)
        val tbufPrimP = PrimitiveParams(locConfig, Set(), castedPorts)

        buffers(TEHBParams(width, bufferDepth), tbufPrimP, OEHBParams(width, 1), obufPrimP)(
          baseName,
          "bufs" + modifier,
          tp
        )
      }
  }

  def branch(baseName: String, modifier: String, tp: TileParams): ModeInstantiator = {
    val brLoc = getLoc(0, PTOutput, PMData(None), Hs, 0, "br" + modifier)
    val sinkLoc = getLoc(0, PTOutput, PMData(None), Hs, 1, "sink" + modifier)
    val inLoc = getLoc(0, PTInput, PMData(None), Hs, 0, "inBr" + modifier)
    val inBHs = getLoc(1, PTInput, PMCond(None), Hs, 0, "brb" + Hs + modifier)
    val outBHs = getLoc(1, PTOutput, PMCond(None), Hs, 0, "brb" + Hs + modifier)
    val inBD = getLoc(1, PTInput, PMCond(None), D, 0, "brb" + D + modifier)
    val outBD = getLoc(1, PTOutput, PMCond(None), D, 0, "brb" + D + modifier)

    val locConfig = (brLoc :: sinkLoc :: inLoc :: inBHs :: inBD :: Nil).toMap
    val bufLocConfig = (inBHs :: outBHs :: inBD :: outBD :: Nil).toMap

    val cBufCast = Map(
      (getId(1, PTInput, PMData(None), Hs)
        -> getId(1, PTInput, PMCond(None), Hs)),
      (getId(1, PTOutput, PMData(None), Hs)
        -> getId(1, PTOutput, PMCond(None), Hs)),
      (getId(1, PTInput, PMData(None), D)
        -> getId(1, PTInput, PMCond(None), D)),
      (getId(1, PTOutput, PMData(None), D)
        -> getId(1, PTOutput, PMCond(None), D))
    )

    val bufName = baseName + "bufs"
    val cModifier = "brb" + modifier
    val cBuf = buffers(
      TEHBParams(1, bufferDepth),
      PrimitiveParams(bufLocConfig, Set(), cBufCast),
      OEHBParams(1, 1),
      PrimitiveParams(bufLocConfig, Set(), cBufCast)
    )(bufName, cModifier, tp)

    val cCast = Map(
      (getId(1, PTOutput, PMData(None), Hs) -> getId(1, PTOutput, PMCond(None), Hs)),
      (getId(1, PTOutput, PMData(None), D) -> getId(1, PTOutput, PMCond(None), D))
    )

    val cLocConfig = (outBHs :: outBD :: Nil).toMap
    val cstP = PrimitiveParams(cLocConfig, Set(AIdentityCrkt), cCast)

    val cstName = baseName + "idcst"
    val cstModifier = "brc" + modifier
    val cCst = Cst(ConstantParams(1), Some(cstP))(cstName, cstModifier, tp)

    val srcP = PrimitiveParams(Map(), Set(AIdentityCrkt), Map())
    val src = Common.Src(SourceParams(), Some(srcP))("brIdSrc", modifier, tp)

    val srcCst = (c: TileCombinator) => {
      chain(name(baseName + "brbufsrccst", modifier), src :: cCst :: Nil, c, Set())
    }

    val bufOrCst = (c: TileCombinator) => or(name(baseName + "brbufcst", modifier), cBuf :: srcCst :: Nil, c, Set())

    val p = BranchParams(0)
    val br = (c: TileCombinator) => {
      PrimMode(name(baseName, modifier), c, locConfig, Set(AWireizeFirst), Map(), None, Branch(p))
    }

    // val inBrBufs = (0 :: 1 :: 32 :: Nil).map {
    //   width => {
    //     val inBufLoc = getLoc(width, PTInput, PMData(None), Hs, 0, "inBr" + modifier)
    //     val inBufDLoc = getLoc(width, PTInput, PMData(None), D, 0, "muxInCst1" + D + modifier)
    //     val outBufLoc = getLoc(width, PTOutput, PMData(None), Hs, 0, "inBr" + modifier)
    //     val outBufDLoc = getLoc(width, PTOutput, PMData(None), D, 0, "muxInCst1" + D + modifier)
    //     val bufLocConf = (inBufDLoc :: outBufDLoc :: inBufLoc :: outBufLoc :: Nil).toMap
    //     Third.genBufParams(width, PMData(None), PMData(None), bufLocConf)("brInBufs" + width, modifier, tp)
    //   }
    // }

    // val inBrBufsModes = (c: TileCombinator) => or(name(baseName + "_inbufs", modifier), inBrBufs, c)

    val inBufLoc = getLoc(0, PTInput, PMData(None), Hs, 0, "inBr" + modifier)
    val outBufLoc = getLoc(0, PTOutput, PMData(None), Hs, 0, "inBr" + modifier)
    val bufLocConf = (inBufLoc :: outBufLoc :: Nil).toMap
    val branchInBufs = Third.genBufParams(0, PMData(None), PMData(None), bufLocConf)("brInBufs", modifier, tp)

    val andBeforeBr = (c0: TileCombinator) => {
      and(name("brAndBefore", modifier), bufOrCst :: branchInBufs :: Nil, c0, Set())
    }

    (c: TileCombinator) => chain(name(baseName + "brbuf", modifier), andBeforeBr :: br :: Nil, c, Set())
  }

  def bpSink(baseName: String, modifier: String, tp: TileParams): ModeInstantiator = {
    val sink = (c: TileCombinator) => {
      val sinkLoc = getLoc(0, PTInput, PMData(None), Hs, 0, "sink" + modifier)

      val locConfig = (sinkLoc :: Nil).toMap
      val p = SinkParams(0)

      PrimMode(name(baseName, modifier), c, locConfig, Set(AWireizeFirst), Map(), None, Sink(p))
    }

    (c: TileCombinator) => or(name("SinkBp", modifier), sink :: bypassInMode :: Nil, c)
  }

  def brSink(baseName: String, modifier: String, tp: TileParams): ModeInstantiator = {
    (c: TileCombinator) =>
      {
        chain(name(baseName, modifier), branch("Br", modifier, tp) :: bpSink("Sink", modifier, tp) :: Nil, c)
      }
  }

  object CMg extends Namer[MergeParams, TileParams] {
    def apply(p: MergeParams, mPrimP: Option[PrimitiveParams] = None): ArchGenerator[TileParams] = {
      (baseName: String, modifier: String, tp: TileParams) =>
        {
          (c: TileCombinator) =>
            {
              val primP = mPrimP.fold(PrimitiveParams.empty)(
                mp => mp
              )

              PrimMode(name(baseName, modifier), c, primP.locConfig, primP.annos, primP.castedPorts, None, Merge(p))
            }
        }
    }
  }

  object Mx extends Namer[MuxParams, TileParams] {
    def apply(p: MuxParams, mPrimP: Option[PrimitiveParams] = None): ArchGenerator[TileParams] = {
      (baseName: String, modifier: String, tp: TileParams) =>
        {
          (c: TileCombinator) =>
            {
              val inHsLLoc = getLoc(p.width, PTInput, PMData(None), Hs, 0, "br" + modifier)
              val inHsRLoc = getLoc(p.width, PTInput, PMData(None), Hs, 1, "muxInExt" + modifier)
              val inDRLoc = getLoc(p.width, PTInput, PMData(None), D, 1, "muxInExt" + modifier)

              val outHsLoc = getLoc(p.width, PTOutput, PMData(None), Hs, 0, "mxmg" + modifier)
              val cLocHs = getLoc(1, PTInput, PMCond(None), Hs, 0, "muxc" + Hs + modifier)
              val cLocD = getLoc(1, PTInput, PMCond(None), D, 0, "muxc" + D + modifier)

              val locConfig = (inHsLLoc :: inHsRLoc :: cLocHs :: cLocD :: outHsLoc :: inDRLoc :: Nil).toMap

              val primId = getPrimId(name(baseName, modifier))
              val primP = mPrimP.fold(PrimitiveParams.empty)(
                mp => mp
              )

              PrimMode(name(baseName, modifier), c, locConfig, primP.annos, primP.castedPorts, None, Mux(p))
            }
        }
    }
  }

  def muxMerges(baseName: String, modifier: String, tp: TileParams): ModeInstantiator = {
    val modes = (0 :: 1 :: 32 :: Nil).map {
      width =>
        {
          val muxParams = MuxParams(width, 2)
          val mx = Mx(muxParams, None)("Mux" + width, modifier, tp)

          if (true) {
            val inHsLoc0 = getLoc(0, PTInput, PMData(None), Hs, 0, "muxInExt" + modifier)

            val inHsLocW = getLoc(width, PTInput, PMData(None), Hs, 0, "muxInExt" + modifier)
            val inDLoc = getLoc(width, PTInput, PMData(None), D, 0, "muxInExt" + modifier)

            val outHsLoc = getLoc(width, PTOutput, PMData(None), Hs, 0, "muxInExt" + modifier)
            val outDLoc = getLoc(width, PTOutput, PMData(None), D, 0, "muxInExt" + modifier)

            val cLocConfig = (inHsLoc0 :: outHsLoc :: outDLoc :: Nil).toMap
            val cstP = PrimitiveParams(cLocConfig, Set(), Map())

            val muxCst = Cst(ConstantParams(width), Some(cstP))("Mux" + width + "_incst", modifier, tp)

            val bpCst = (c: TileCombinator) => {
              or(name("BpCst" + width, modifier), muxCst :: bypassOutMode :: Nil, c)
            }

            val bufLocConfig = (inHsLocW :: inDLoc :: outHsLoc :: outDLoc :: Nil).toMap

            // TODO hack for fixupWIdth in ExtendPacking
            val nameModifier = if (width == 32) {
              "aa"
            } else if (width == 1) {
              "bb"
            } else {
              "cc"
            }

            val preMuxBuffers = buffers(
              TEHBParams(width, 1),
              PrimitiveParams(bufLocConfig, Set(), Map()),
              OEHBParams(width, 1),
              PrimitiveParams(bufLocConfig, Set(), Map())
            )("muxInExtBuf" + width + nameModifier, modifier, tp)

            val outBufHsInLoc = getLoc(width, PTInput, PMData(None), Hs, 0, "mxmg" + modifier)
            val outBufHsOutLoc = getLoc(width, PTOutput, PMData(None), Hs, 0, "mxmg" + modifier)

            val outBufLocConfig = (outBufHsInLoc :: outBufHsOutLoc :: Nil).toMap

            val postMuxBuffers = buffers(
              TEHBParams(width, 1),
              PrimitiveParams(outBufLocConfig, Set(), Map()),
              OEHBParams(width, 1),
              PrimitiveParams(outBufLocConfig, Set(), Map())
            )("muxOutExtBuf" + width, modifier, tp)

            if (width > 0) {
              ((c: TileCombinator) => {
                chain(name(baseName + "mxcst" + width, modifier), preMuxBuffers :: bpCst :: mx :: Nil, c, Set())
              }) :: Nil
            } else {
              ((c: TileCombinator) => {
                chain(name(baseName + "mxcst", modifier), preMuxBuffers :: mx :: Nil, c, Set())
              }) :: Nil
            }
          } else {
            mx :: Nil
          }
        }
    }.flatten

    val muxMgs = (c: TileCombinator) => or(name(baseName, modifier), modes, c)

    val fLocConfigOut = (0 until 8).map {
      i =>
        {
          getLoc(0, PTOutput, PMData(None), Hs, i, "glob")
        }
    }
    val fLocConfigIn = getLoc(0, PTInput, PMData(None), Hs, 0, "mxmg" + modifier)
    // val fLocConfig = (fLocConfigIn :: fLocConfigOut.toList).toMap
    val fLocConfig = (fLocConfigIn :: Nil).toMap

    val fCastParams = ForkCastParams(PMData(None), 0, PMData(None), 0)

    val fAnnos = Set(AWireizeFirst, AForkNumExits(outForkSize))
    val forkParams = ForkParams(0, outForkSize, EagerFork)
    val fork = genForkParams(forkParams, fAnnos, fLocConfig, fCastParams)("MuxMerge", modifier, tp)

    val cbufName = baseName + "cbufs"
    val cModifier = "c" + modifier

    val incLocHs0 = getLoc(0, PTInput, PMCond(None), Hs, 0, "muxc" + Hs + modifier)
    val incLocHs1 = getLoc(1, PTInput, PMCond(None), Hs, 0, "muxc" + Hs + modifier)
    val incLocD = getLoc(1, PTInput, PMCond(None), D, 0, "muxc" + D + modifier)
    val outcLocHs0 = getLoc(0, PTOutput, PMCond(None), Hs, 0, "muxc" + Hs + modifier)
    val outcLocHs1 = getLoc(1, PTOutput, PMCond(None), Hs, 0, "muxc" + Hs + modifier)
    val outcLocD = getLoc(1, PTOutput, PMCond(None), D, 0, "muxc" + D + modifier)

    val bufLocConfig = (incLocHs1 :: incLocD :: outcLocHs1 :: outcLocD :: Nil).toMap

    val cBufCast = Map(
      (getId(1, PTInput, PMData(None), Hs)
        -> getId(1, PTInput, PMCond(None), Hs)),
      (getId(1, PTOutput, PMData(None), Hs)
        -> getId(1, PTOutput, PMCond(None), Hs)),
      (getId(1, PTInput, PMData(None), D)
        -> getId(1, PTInput, PMCond(None), D)),
      (getId(1, PTOutput, PMData(None), D)
        -> getId(1, PTOutput, PMCond(None), D))
    )

    val cBufs = buffers(
      TEHBParams(1, bufferDepth),
      PrimitiveParams(bufLocConfig, Set(), cBufCast),
      OEHBParams(1, 1),
      PrimitiveParams(bufLocConfig, Set(), cBufCast)
    )(cbufName, cModifier, tp)

    val cCast = Map(
      (getId(0, PTInput, PMData(None), Hs) -> getId(0, PTInput, PMCond(None), Hs)),
      (getId(1, PTOutput, PMData(None), Hs) -> getId(1, PTOutput, PMCond(None), Hs)),
      (getId(1, PTOutput, PMData(None), D) -> getId(1, PTOutput, PMCond(None), D))
    )

    val cLocConfig = (incLocHs0 :: outcLocHs1 :: outcLocD :: Nil).toMap
    val cstP = PrimitiveParams(cLocConfig, Set(AIdentityCrkt), cCast)

    val cstName = baseName + "idcst"
    val cstModifier = "brc" + modifier
    val cCst = Cst(ConstantParams(1), Some(cstP))(cstName, cstModifier, tp)

    val bpCst = (c0: TileCombinator) => {
      or("bp" + cstModifier, cCst :: bypassOutMode :: Nil, c0)
    }

    val srcCast = Map(
      (getId(0, PTOutput, PMData(None), Hs) -> getId(0, PTOutput, PMCond(None), Hs))
    )

    val srcLocConfig = (outcLocHs0 :: Nil).toMap
    val srcP = PrimitiveParams(srcLocConfig, Set(AIdentityCrkt), srcCast)
    val src = Common.Src(SourceParams(), Some(srcP))("brcidsrc", modifier, tp)

    val infLocHs = getLoc(1, PTInput, PMCond(None), Hs, 0, "fin" + Hs + modifier)
    val infLocD = getLoc(1, PTInput, PMCond(None), D, 0, "fin" + D + modifier)

    val dfLocConfig = (infLocHs :: infLocD :: outcLocHs1 :: outcLocD :: Nil).toMap

    val idCstDFork = genForkParams(
      ForkParams(1, 1, EagerFork),
      Set(AWireizeLast, AForkNumExits(0)),
      dfLocConfig,
      Common.ForkCastParams(PMCond(None), 1, PMCond(None), 1)
    )("brcidcstfork", modifier, tp)

    val bpSrc = (c0: TileCombinator) => {
      or("bp" + "brcidsrc" + modifier, src :: bypassOutMode :: Nil, c0)
    }

    val srcCst = (c: TileCombinator) => {
      chain(name(baseName + "brbufsrccst", modifier), bpSrc :: bpCst :: Nil, c, Set())
    }

    val forkSrcCst = (c: TileCombinator) => {
      chain(name(baseName + "forkSrcCst", modifier), idCstDFork :: srcCst :: Nil, c, Set())
    }

    val bufOrCst = (c: TileCombinator) =>
      or(name(baseName + "brbufcst", modifier), cBufs :: forkSrcCst :: Nil, c, Set())

    (c: TileCombinator) => chain(name(baseName + "mb", modifier), bufOrCst :: muxMgs :: fork :: Nil, c, Set())
  }

  def branchTree(baseName: String, modifier: String, tp: TileParams): ModeInstantiator = {
    (c: TileCombinator) =>
      {
        val modes = brSink("brSink", modifier, tp) :: muxMerges("muxMerges", modifier, tp) :: Nil

        chain(name(baseName, modifier), modes, c)
      }
  }

  def branchTrees(baseName: String, modifier: String, tp: TileParams): ModeInstantiator = {
    repeat(baseName, modifier, 7, branchTree, tp)
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

  object Mg extends Namer[MergeParams, TileParams] {
    def apply(p: MergeParams, mPrimP: Option[PrimitiveParams] = None): ArchGenerator[TileParams] = {
      (baseName: String, modifier: String, tp: TileParams) =>
        {
          (c: TileCombinator) =>
            {
              val primP = mPrimP.fold(PrimitiveParams.empty)(
                mp => mp
              )

              val inHsLLoc = getLoc(p.width, PTInput, PMData(None), Hs, 0, "br" + modifier)
              val inHsRLoc = getLoc(p.width, PTInput, PMData(None), Hs, 1, "glob" + modifier)
              val outHsLoc = getLoc(p.width, PTOutput, PMData(None), Hs, 0, "mxmg" + modifier)

              val locConfig = if (primP.locConfig.isEmpty) {
                (inHsLLoc :: inHsRLoc :: outHsLoc :: Nil).toMap
              } else {
                primP.locConfig
              }

              PrimMode(
                name(baseName, modifier),
                c,
                locConfig,
                primP.annos + ALogicalMode,
                primP.castedPorts,
                None,
                Merge(p)
              )
            }
        }
    }
  }

  object CMerge extends Namer[CntrlMergeParams, TileParams] {
    def apply(p: CntrlMergeParams, primP: Option[PrimitiveParams] = None): ArchGenerator[TileParams] = {
      (baseName: String, modifier: String, tp: TileParams) =>
        {
          (c: TileCombinator) =>
            {
              val primParams = primP.getOrElse(PrimitiveParams.empty)
              val annos: Set[Annotation] = Set(AMoleculeRoot)

              PrimMode(name(baseName, modifier), c, primParams.locConfig, annos, Map(), None, CntrlMerge(p))
            }
        }
    }
  }

  def cmergeOut(baseName: String, modifier: String, tp: TileParams): ModeInstantiator = {
    val br = brSink("brSink", "7" + modifier, tp)

    val brLoc = getLoc(0, PTInput, PMData(None), Hs, 0, "br7" + modifier)
    val gLoc = getLoc(0, PTInput, PMData(None), Hs, 1, "cmmghsin")
    val mLocConf = (brLoc :: gLoc :: Nil).toMap

    val cmgPrimParams = Some(PrimitiveParams(mLocConf, Set(), Map()))

    val cMerge = CMerge(CntrlMergeParams(2), cmgPrimParams)(baseName, modifier, tp)

    val locConfig = (0 until 8)
      .map {
        i =>
          {
            if (i < 7) {
              getLoc(1, PTOutput, PMCond(None), Hs, i, "muxc" + Hs + i + modifier)
                :: getLoc(1, PTOutput, PMCond(None), D, i, "muxc" + D + i + modifier)
                :: Nil
            } else {
              getLoc(1, PTOutput, PMCond(None), Hs, i, "muxcout" + Hs + i + modifier)
                :: getLoc(1, PTOutput, PMCond(None), D, i, "muxcout" + D + i + modifier)
                :: Nil
            }
          }
      }
      .flatten
      .toMap

    val fAnnos = Set(AWireizeLast, AForkNumExits(1))
    val cMergeFork = genForkParams(
      ForkParams(0, 8, EagerFork),
      fAnnos,
      locConfig,
      ForkCastParams(PMCond(None), 1, PMCond(None), 1)
    )(baseName + "cf", modifier, tp)

    val cMergeF = (c: TileCombinator) => chain(name(baseName + "CMF", modifier), cMerge :: cMergeFork :: Nil, c, Set())

    val merge = Mg(MergeParams(0, 2), cmgPrimParams)(baseName + "mg", modifier, tp)
    val cmOrM = (c: TileCombinator) => or(name(baseName + "or", modifier), cMergeF :: merge :: Nil, c, Set())

    val brCmLocIn = getLoc(0, PTInput, PMData(None), Hs, 0, "br7" + modifier)
    val brCmLocOut = getLoc(0, PTOutput, PMData(None), Hs, 0, "br7" + modifier)
    val bufLocConfig = (brCmLocIn :: brCmLocOut :: Nil).toMap

    val ibufs = buffers(
      TEHBParams(0, bufferDepth),
      PrimitiveParams(bufLocConfig, Set(), Map()),
      OEHBParams(0, 1),
      PrimitiveParams(bufLocConfig, Set(), Map())
    )("brSinkB", "7" + modifier, tp)

    val oBufs = genBufParams(0, PMData(None), PMData(None), Map(), false)("CMMgBuf", modifier, tp)

    val sinkP = SinkParams(0)
    val sink = (c: TileCombinator) => {
      val primId = getPrimId(name(baseName + "sink", modifier))
      PrimMode(name(baseName + "sink", modifier), c, Map(), Set(AWireizeFirst), Map(), None, Sink(sinkP))
    }

    val fsink = (c: TileCombinator) => or(name(baseName + "fsink", modifier), sink :: bypassInMode :: Nil, c) // frk ::

    val lazyBufs = buffers(
      TEHBParams(0, 1),
      PrimitiveParams(Map(), Set(), Map()),
      OEHBParams(0, 1),
      PrimitiveParams(Map(), Set(), Map())
    )("lazyBufs", "7" + modifier, tp)

    val bbSize = 4
    val bbAnnos = Set(AWireizeLast, AForkNumExits(bbSize))
    val bbForkParams = ForkParams(0, bbSize, EagerFork)
    val bbFork = genForkParams(bbForkParams, bbAnnos, Map(), ForkCastParams(PMData(None), 0, PMData(None), 0)) (
      "bbeFork",
      modifier,
      tp
    )

    val lazySize = 4
    val lazyAnnos = Set(AWireizeLast, AForkNumExits(bbSize))
    val lazyParams = ForkParams(0, bbSize, LazyFork)
    val lazyFork = genForkParams(lazyParams, lazyAnnos, Map(), ForkCastParams(PMData(None), 0, PMData(None), 0)) (
      "bblFork",
      modifier,
      tp
    )

    val lazyF = (c: TileCombinator) => {
      or(name("bbel", modifier), bbFork :: lazyFork :: Nil, c)
    }

    val bbForkOrSink = (c: TileCombinator) => {
      chain(name("bbForkSinkMode", modifier), fsink :: lazyF :: Nil, c)
    }

    val lfIn = getLoc(0, PTInput, PMData(None), Hs, 0, "br7" + modifier)
    val lfOut0 = getLoc(0, PTOutput, PMData(None), Hs, 0, "br7" + modifier)
    val lfOut1 = getLoc(0, PTOutput, PMData(None), Hs, 1, "tolsq0" + modifier)
    val lfOut2 = getLoc(0, PTOutput, PMData(None), Hs, 2, "tolsq1" + modifier)
    val lfOut3 = getLoc(0, PTOutput, PMData(None), Hs, 3, "tolsq2" + modifier)
    val lfLocConf = (lfIn :: lfOut0 :: lfOut1 :: lfOut2 :: lfOut3 :: Nil).toMap

    val eagerFork = genForkParams(
      ForkParams(0, lazySize, EagerFork),
      lazyAnnos,
      lfLocConf,
      ForkCastParams(PMData(None), 0, PMData(None), 0)
    )(baseName + "ef", modifier, tp)

    // TODO Why is this here already??
    val oName = name(baseName + "olf", modifier)
    val oehbP = OEHBParams(0, 1)
    val oehbLocConf = (lfIn :: lfOut0 :: Nil).toMap
    val oehb = (c: TileCombinator) => {
      PrimMode(oName, c, oehbLocConf, Set(), Map(), None, OEHB(oehbP))
    }

    val bpOehb = (c: TileCombinator) => or(name(oName + "bp", ""), oehb :: bypassInMode :: Nil, c)

    (c: TileCombinator) => {
      chain("cmBufs" + modifier, br :: ibufs :: eagerFork :: bpOehb :: cmOrM :: lazyBufs :: bbForkOrSink :: Nil, c)
    }
  }

  def cmergemux(baseName: String, modifier: String, tp: TileParams): ModeInstantiator = {
    (c: TileCombinator) =>
      {
        val comps = cmergeOut("CMerge", modifier, tp) :: branchTrees("brMuxs", modifier, tp) :: Nil
        val annos: Set[Annotation] = Set()

        chain(name(baseName, modifier), comps, c, annos)
      }
  }

  object BBCond extends Namer[EmptyParams.type, TileParams] {
    def apply(p: EmptyParams.type, primP: Option[PrimitiveParams] = None): ArchGenerator[TileParams] = {
      (baseName: String, modifier: String, tp: TileParams) =>
        {
          (c: TileCombinator) =>
            {
              val icmpP = ComparatorParams(32, CmpAnyComp)
              val icmp = ArithComp(Icmp)

              val castedPorts = Map(
                (getId(1, PTOutput, PMData(None), Hs) -> getId(1, PTOutput, PMCond(None), Hs)),
                (getId(1, PTOutput, PMData(None), D) -> getId(1, PTOutput, PMCond(None), D))
              )

              val primP = PrimitiveParams(Map(), Set(), castedPorts)

              // val cst1 = Cst(ConstantParams(1), Some(primP))("CCst1", modifier, tp)
              val cstByp = (c: TileCombinator) => {
                val cstComps = Cst(ConstantParams(1), Some(primP))("CCst1", modifier, tp) :: bypassOutMode :: Nil
                or(name("CCst1bp", modifier), cstComps, c)
              }

              val dummyMode = (c: TileCombinator) => and("dummyMode", cstByp :: Nil, c, Set())

              val comps = icmp(icmpP)("Icmp", modifier, tp) // icmpPre
                :: dummyMode
                :: Nil

              or(name(baseName, modifier), comps, c, Set())
            }
        }
    }
  }

  def all(baseName: String, modifier: String, tp: TileParams): ModeInstantiator = {
    val condForkSize = 9

    val locConfig = (0 until condForkSize)
      .map {
        i =>
          {
            if (i < (condForkSize - 1)) {
              getLoc(1, PTOutput, PMCond(None), Hs, i, "brb" + Hs + i + modifier)
                :: getLoc(1, PTOutput, PMCond(None), D, i, "brb" + D + i + modifier)
                :: Nil
            } else {
              getLoc(1, PTOutput, PMCond(None), Hs, i, "brbout" + Hs + i + modifier)
                :: getLoc(1, PTOutput, PMCond(None), D, i, "brbout" + D + i + modifier)
                :: Nil
            }
          }
      }
      .flatten
      .toMap

    val annos = Set[Annotation](AWireizeLast, AMoleculeRoot, AForkNumExits(1))

    val bbCond = BBCond(EmptyParams, None)("BBCond", modifier, tp)
    val bbCondFork = genForkParams(
      ForkParams(0, condForkSize, EagerFork),
      annos,
      locConfig,
      ForkCastParams(PMCond(None), 1, PMCond(None), 1)
    )("BBCondF", modifier, tp)

    val compChain = bbCond :: bbCondFork :: cmergemux("brcmergemux", modifier, tp) :: Nil

    (c: TileCombinator) => chain(name(baseName, modifier), compChain, c, Set())
  }

  def apply(): Tile = {
    val defTp = TileParams(true, false, false)

    val control = all("fullHs", "", TileParams(true, false, false))

    Tile("Control", control(TileAnd) :: Nil, Set(), ControlConfig)
  }
}
