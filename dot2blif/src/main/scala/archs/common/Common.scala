package archs

import arch._
import ArchGeneratorUtils._
import core._

object Common {
  case class ForkCastParams(inPM: PortMeaning, inW: Int, outPM: PortMeaning, outW: Int)

  object Src extends Namer[SourceParams, Params] {
    def apply(p: SourceParams, primitiveP: Option[PrimitiveParams] = None): ArchGenerator[Params] = {
      (baseName: String, modifier: String, _) =>
        {
          (c: TileCombinator) =>
            {
              val primP = primitiveP.fold(PrimitiveParams.empty)(
                primP => primP
              )

              PrimMode(
                name(baseName, modifier),
                c,
                primP.locConfig,
                primP.annos,
                primP.castedPorts,
                None,
                Source(SourceParams())
              )
            }
        }
    }
  }

  def source(baseName: String, modifier: String): ModeInstantiator = {
    (c: TileCombinator) =>
      {
        PrimMode(
          name(baseName, modifier),
          c,
          Map(),
          Set(),
          Map(),
          None,
          Source(SourceParams())
        )
      }
  }

  object Cst extends Namer[ConstantParams, Params] {
    def apply(p: ConstantParams, primitiveP: Option[PrimitiveParams] = None): ArchGenerator[Params] = {
      (baseName: String, modifier: String, _) =>
        {
          (c: TileCombinator) =>
            {
              val primP = primitiveP.fold(PrimitiveParams.empty)(
                primP => primP
              )
              PrimMode(
                name(baseName, modifier),
                c,
                primP.locConfig,
                primP.annos,
                primP.castedPorts,
                None,
                Constant(p)
              )
            }
        }
    }
  }

  case class SrcCst(cstPrimP: Option[PrimitiveParams]) extends Namer[ConstantParams, Params] {
    def apply(p: ConstantParams, primitiveP: Option[PrimitiveParams] = None): ArchGenerator[Params] = {
      (baseName: String, modifier: String, tp: Params) =>
        {
          val srcPrimP = if (cstPrimP.isEmpty) {
            PrimitiveParams.empty
          } else {
            val locConf = cstPrimP.get.locConfig.filter(_._1.id.pt == PTInput).map {
              (lid, s) =>
                {
                  val nId = lid.id.flipped()
                  val nLid = LocatedBlockPortID(nId, 0)

                  (nLid, s)
                }
            }

            PrimitiveParams(locConf, cstPrimP.get.annos, Map())
          }

          val bufLocConf = if (cstPrimP.isEmpty) {
            Map()
          } else {
            cstPrimP.get.locConfig
              .filter(_._1.id.pt == PTInput)
              .map {
                (lid, s) =>
                  {
                    val nId = lid.id.flipped()
                    val nLid = LocatedBlockPortID(nId, 0)

                    (nLid, s) :: (lid, s) :: Nil
                  }
              }
              .flatten
              .toMap
          }

          val obufPrimP = PrimitiveParams(bufLocConf, Set(), Map())
          val tbufPrimP = PrimitiveParams(bufLocConf, Set(), Map())

          val icmpBufsPreli =
            buffers(TEHBParams(0, 1), tbufPrimP, OEHBParams(0, 1), obufPrimP)("srccstprebuf", modifier, tp)

          val bpSrc = (c: TileCombinator) => {
            or(
              name("BpSrc", modifier),
              Src(SourceParams(), Some(srcPrimP))("src", modifier, tp) :: bypassOutMode :: Nil,
              c
            )
          }

          val bpCst = (c: TileCombinator) => {
            or(name("BpCst", modifier), Cst(p, cstPrimP)("Cst" + p.width, modifier, tp) :: bypassOutMode :: Nil, c)
          }

          (c: TileCombinator) => {
            chain(name(baseName, modifier), icmpBufsPreli :: bpSrc :: bpCst :: Nil, c, primitiveP.fold(Set())(_.annos))
          }
        }
    }
  }

  case class ArithComp[P <: Params, TP <: Params](comp: Namer[P, TP]) extends Namer[P, TP] {
    def apply(p: P, primP: Option[PrimitiveParams] = None): ArchGenerator[TP] = {
      val arithCst: ArchGenerator[TP] = (baseName: String, modifier: String, tp: TP) => {
        val cstLocIn = getLoc(0, PTInput, PMData(None), Hs, 0, "arith" + modifier)
        val cstLocOut = getLoc(32, PTOutput, PMData(None), Hs, 0, "arith" + modifier)
        val cstLocs = (cstLocIn :: cstLocOut :: Nil).toMap

        val cstPrimP = Some(PrimitiveParams(cstLocs, Set(), Map()))
        val cst = SrcCst(cstPrimP)(ConstantParams(32))("SrcCst", modifier, tp)

        val cstBufLocIn = getLoc(32, PTInput, PMData(None), Hs, 0, "arith" + modifier)
        val cstBufLocOut = getLoc(32, PTOutput, PMData(None), Hs, 0, "arith" + modifier)
        val cstLoc = (cstBufLocIn :: cstBufLocOut :: Nil).toMap

        val cstBufs = Control.genBufParams(32, PMData(None), PMData(None), cstLoc, true)("arithBufs", modifier, tp)

        (c: TileCombinator) => chain("arithCst" + modifier, cst :: cstBufs :: Nil, c)
      }

      (baseName: String, modifier: String, tp: TP) => {
        val arithIn = repeat("arithCst", modifier, 2, arithCst, tp)

        val arithlLoc = getLoc(32, PTInput, PMData(None), Hs, 1, "arith0" + modifier)
        val arithrLoc = getLoc(32, PTInput, PMData(None), Hs, 0, "arith1" + modifier)

        val compLocConf = (arithlLoc :: arithrLoc :: Nil).toMap
        val compPrimP = if (primP.isEmpty) {
          Some(PrimitiveParams(compLocConf, Set(), Map()))
        } else {
          Some(PrimitiveParams(compLocConf, primP.get.annos, primP.get.castedPorts))
        }

        val arith = comp(p, compPrimP)(baseName, modifier, tp)

        (c: TileCombinator) => chain(name("Op" + baseName, modifier), arithIn :: arith :: Nil, c)
      }
    }
  }

  def fork(p: ForkParams, fPrimP: Option[PrimitiveParams]): ArchGenerator[Params] = {
    (baseName: String, modifier: String, tp: Params) =>
      {
        (c: TileCombinator) =>
          {
            val primP = fPrimP.fold(PrimitiveParams.empty)(
              fp => fp
            )

            PrimMode(baseName + modifier, c, primP.locConfig, primP.annos, primP.castedPorts, None, Fork(p))
          }
      }
  }

  def danglingComp[P <: Params, T <: Params](p: P, comp: Namer[P, T]) = {
    (baseName: String, modifier: String, tp: T) =>
      {
        val comps = comp(p, Some(PrimitiveParams(Map(), Set(AMoleculeRoot), Map())))(baseName, modifier, tp) :: Nil
        (c: TileCombinator) => and("Dangling" + baseName, comps, c, Set())
      }
  }

  def buffers[TP <: Params](
      tehbP: TEHBParams,
      tehbPrimP: PrimitiveParams,
      oehbP: OEHBParams,
      oehbPrimP: PrimitiveParams,
      annos: Set[Annotation] = Set()
  ): ArchGenerator[TP] = {
    (baseName: String, modifier: String, tp: TP) =>
      {
        val tName = name(baseName + "t", modifier)
        val tehb = (c: TileCombinator) => {
          PrimMode(tName, c, tehbPrimP.locConfig, tehbPrimP.annos, tehbPrimP.castedPorts, None, TEHB(tehbP))
        }

        val bpTehb = (c: TileCombinator) => or(name(tName + "bp", ""), tehb :: bypassInMode :: Nil, c)

        val oName = name(baseName + "o", modifier)
        val oehb = (c: TileCombinator) => {
          PrimMode(oName, c, oehbPrimP.locConfig, oehbPrimP.annos, oehbPrimP.castedPorts, None, OEHB(oehbP))
        }

        val bpOehb = (c: TileCombinator) => or(name(oName + "bp", ""), oehb :: bypassInMode :: Nil, c)

        (c: TileCombinator) => chain(name(baseName, modifier), bpOehb :: bpTehb :: Nil, c, annos)
      }
  }
}
