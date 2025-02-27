package analysis

import archs.ConfigurationBits
import archs.ConfParams
import frontend.GlobalParamsInst
import core.ALogicalMode
import archs.MuxConfig
import archs.MuxConfigParams
import archs.VPRConfig
import archs.Abs
import archs.Frac
import archs.Params
import archs.Primitive
import archs.OEHB
import archs.TEHB
import archs.EB
import arch._
import archs.TEHBParams
import archs.OEHBParams
import archs.EBParams

import math.floor
import math.sqrt
import math.pow
import math.ceil
import collection.mutable.{Set => MSet}
import util.Util.log2ceil

sealed trait AreaInfo {
  def confBits: Map[TBlock, Int]
  def allComponents(): List[(TBlock, Int)]

  def recArea(components: List[(TBlock, Int)]): Double = {
    components.map {
      (b, num) =>
        {
          b.physicalInfo.area * num
        }
    }.sum
  }

  def area(): Double = {
    recArea(allComponents())
  }

  def confArea(): Double = {
    recArea(confBits.toList)
  }
}

case class CMuxInfo(muxs: Map[TBlock, Int], confBits: Map[TBlock, Int]) extends AreaInfo {
  def allComponents(): List[(TBlock, Int)] = muxs.toList ++ confBits.toList
}

case class PrimitiveInfo(prims: Map[TBlock, Int], confBits: Map[TBlock, Int]) extends AreaInfo {
  def allComponents(): List[(TBlock, Int)] = prims.toList ++ confBits.toList
}

case class CBInfo(cbMuxs: Map[TBlock, Int], confBits: Map[TBlock, Int]) extends AreaInfo {
  def allComponents(): List[(TBlock, Int)] = cbMuxs.toList ++ confBits.toList
}

case class SBInfo(sbMuxs: Map[TBlock, Int], confBits: Map[TBlock, Int]) extends AreaInfo {
  def allComponents(): List[(TBlock, Int)] = sbMuxs.toList ++ confBits.toList
}

case class TileAreaInfo(
    prims: PrimitiveInfo,
    cmuxs: CMuxInfo,
    cb: CBInfo,
    sb: SBInfo,
    tile: RootPb
) extends AreaInfo {
  lazy val confBits: Map[TBlock, Int] = {
    (prims.confBits.toList ++ cmuxs.confBits.toList ++ cb.confBits.toList ++ sb.confBits.toList)
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2).sum)
      )
  }

  def allComponents(): List[(TBlock, Int)] = {
    prims.allComponents() ++ cmuxs.allComponents() ++ cb.allComponents() ++ sb.allComponents()
  }

  def areaWithCap(): Double = {
    val cap = tile.vprConfig.capacity

    val duplicated = (prims.allComponents() ++ cmuxs.allComponents() ++ cb.allComponents()).map(
      (b, i) => (b, i * cap)
    )

    recArea(duplicated ++ sb.allComponents())
  }
}

object AreaExtractor {
  def getAssociatedConfBits(params: GlobalParamsInst, components: Map[TBlock, Int]): Map[TBlock, Int] = {
    components.toSeq
      .filter(
        (b, num) => b.prim.numConfBits(b.prim.p) > 0
      )
      .map {
        (b, num) =>
          {
            val numConf = b.prim.numConfBits(b.prim.p)
            val conf = ConfigurationBits(ConfParams(numConf))

            val confInst = conf.instantiate(params)

            (confInst, num)
          }
      }
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2).sum)
      )
      .toMap
  }

  def getPrimitives(params: GlobalParamsInst, pb: PbType): PrimitiveInfo = {
    def rec(recPb: PbType): List[TBlock] = {
      recPb match {
        case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
          prim :: Nil
        }

        case other => recPb.subBlocks.map(rec(_)).flatten.toList
      }
    }

    val prims = rec(pb)
      .groupBy(b => b)
      .map((b, bs) => (b, bs.size))
      .toMap

    val confBits = getAssociatedConfBits(params, prims)

    PrimitiveInfo(prims, confBits)
  }

  def getCBMuxs(
      params: GlobalParamsInst,
      root: RootPb,
      archs: List[Arch]
  ): CBInfo = {
    val cbMuxs = root
      .ioInfo()
      .filter(_._1._1 == PTInput)
      .map {
        case ((pt, width), num) => {
          val archCandidates = archs.filter(_.width == width)
          assert(archCandidates.size == 1)

          val arch = archCandidates.head
          val muxIn = arch.fcIn.getFc(arch.chanWidth)

          val mux = MuxConfig(MuxConfigParams(width, muxIn, 0, 0)).instantiate(params)

          if (muxIn > 1 && (mux.physicalInfo.area == 0)) {
            println(MuxConfigParams(width, muxIn, 0, 0))
          }

          assert(
            muxIn == 1 || (mux.physicalInfo.area != 0),
            "Missing mux implementation: " + MuxConfigParams(width, muxIn, 0, 0)
          )

          (mux, num)
        }
      }
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2).sum)
      )

    val confBits = getAssociatedConfBits(params, cbMuxs)

    CBInfo(cbMuxs, confBits)
  }

  def isMuxBypssing(mux: Mux): Boolean = {
    val bypassPath = mux.sources.filter {
      loc =>
        {
          val samePb = (loc.pbName == mux.dstLoc.pbName)
          val sameLoc = (loc.pin.loc == mux.dstLoc.pin.loc)
          val sameId = loc.pin.id == mux.dstLoc.pin.id.flipped()

          samePb && sameLoc && sameId
        }
    }.size == 1

    val takenPath = mux.sources.filter {
      loc =>
        {
          val otherPb = (loc.pbName != mux.dstLoc.pbName)
          val sameId = loc.pin.id == mux.dstLoc.pin.id.flipped()

          otherPb && sameId
        }
    }.size == 1

    (mux.sources.size == 2) && bypassPath && takenPath
  }

  def isConfigMux(mux: Mux): Boolean = {
    !isMuxBypssing(mux)
  }

  def getBypassedPbName(mux: Mux): String = {
    assert(isMuxBypssing(mux))

    mux.sources
      .filter {
        loc =>
          {
            val otherPb = (loc.pbName != mux.dstLoc.pbName)
            val sameId = loc.pin.id == mux.dstLoc.pin.id.flipped()

            otherPb && sameId
          }
      }
      .map(_.pbName)
      .head
  }

  def configGroupKey(mux: Mux): Set[String] = {
    assert(isConfigMux(mux))

    mux.sources.map(_.pbName).toSet + mux.dstLoc.pbName
  }
  def aggregateMuxs[T](
      params: GlobalParamsInst,
      cmuxs: List[Mux],
      groupKey: Mux => T
  ): (Map[TBlock, Int], Map[TBlock, Int]) = {
    val cmuxBlocks = cmuxs
      .map {
        mux =>
          {
            assert(isConfigMux(mux))

            val muxIn = mux.sources.size
            val width = mux.dstLoc.pin.id.concreteWidth

            val muxBlock = MuxConfig(MuxConfigParams(width, muxIn, 0, 0)).instantiate(params)

            (muxBlock, 1)
          }
      }
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2).sum)
      )

    val confBits = cmuxs
      .groupBy(groupKey(_))
      .map {
        (_, muxs) =>
          {
            val numConf = muxs
              .map(
                mux => log2ceil(mux.sources.size).toInt
              )
              .max
            val conf = ConfigurationBits(ConfParams(numConf))

            val confInst = conf.instantiate(params)

            (confInst, 1)
          }
      }
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2).sum)
      )
      .toMap

    (cmuxBlocks, confBits)
  }

  // Bypass muxs have 2 inputs one output, 1 input to a child pbType, 1 input and 1 output to the parent PbType
  // ConfigMuxs have N inputs and one output, N different input PbType, to the parent PbType
  def getConfigMuxs(params: GlobalParamsInst, root: RootPb): CMuxInfo = {
    def collectMultiplexers(pb: PbType): List[Mux] = {
      (pb.subBlocks.map(collectMultiplexers(_)).flatten ++ pb.links.collect {
        case mux: Mux => mux
      }).toList
    }

    val allCMuxs = collectMultiplexers(root)

    val bpMuxs = allCMuxs.filter(isMuxBypssing(_))
    val configMuxs = allCMuxs.filter(isConfigMux(_))

    val bpMuxsInfo = aggregateMuxs(params, bpMuxs, getBypassedPbName)
    val configMuxsInfo = aggregateMuxs(params, configMuxs, configGroupKey)

    val muxsInfo = (bpMuxsInfo._2.toSeq ++ configMuxsInfo._1.toSeq)
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2).sum)
      )

    val confBitsInfo = (bpMuxsInfo._2.toSeq ++ configMuxsInfo._2.toSeq)
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2).sum)
      )

    CMuxInfo(muxsInfo, confBitsInfo)
  }

  // Area without SB
  def getTileUnitArea(
      params: GlobalParamsInst,
      root: RootPb,
      archs: List[Arch]
  ): TileAreaInfo = {
    val cbInfo = getCBMuxs(params, root, archs)
    val primsInfo = getPrimitives(params, root)
    val cmuxsInfo = getConfigMuxs(params, root)

    TileAreaInfo(primsInfo, cmuxsInfo, cbInfo, SBInfo(Map(), Map()), root)
  }

  def getBiggestPb(
      tiles: Map[String, RootPb],
      archs: List[Arch],
      params: GlobalParamsInst
  ): RootPb = {
    tiles.map(_._2).reduce {
      (pb0, pb1) =>
        {
          val area0 = getTileUnitArea(params, pb0, archs)
          val area1 = getTileUnitArea(params, pb1, archs)

          if (area0.areaWithCap() > area1.areaWithCap()) {
            pb0
          } else {
            pb1
          }
        }
    }
  }

  def getSBMuxs(
      params: GlobalParamsInst,
      tiles: Map[String, RootPb],
      archs: List[Arch],
      conInfo: Map[Arch, RRGConInfo]
  ): SBInfo = {
    val biggestPb = getBiggestPb(tiles, archs, params)
    val baseAreaInfo = getTileUnitArea(params, biggestPb, archs)

    def rec(area: Double): Map[Block, Int] = {
      val side = getTileSide(area).toInt

      val sbMuxs = archs
        .filter(!_.place)
        .map {
          arch =>
            {
              conInfo(arch).rMuxIns.toList.map {
                (l, muxIn) =>
                  {
                    val mux = MuxConfig(MuxConfigParams(arch.width, muxIn, side, l)).instantiate(params)

                    if (((mux.physicalInfo.area == 0) && muxIn > 1)) {
                      println(MuxConfigParams(arch.width, muxIn, side, l))
                    }

                    assert(
                      muxIn == 1 || (mux.physicalInfo.area != 0),
                      "Missing mux implementation: " + MuxConfigParams(arch.width, muxIn, side, l)
                    )

                    val numRmuxs = conInfo(arch).routingNum(l)

                    (mux, numRmuxs)
                  }
              }
            }
        }
        .flatten
        .groupBy(_._1)
        .map(
          (k, v) => (k, v.map(_._2).sum)
        )

      val nAreaInfo = TileAreaInfo(
        baseAreaInfo.prims,
        baseAreaInfo.cmuxs,
        baseAreaInfo.cb,
        SBInfo(sbMuxs, getAssociatedConfBits(params, sbMuxs)),
        biggestPb
      )

      val nArea = roundTileArea(nAreaInfo.areaWithCap())

      if (nArea > area) {
        rec(nArea)
      } else {
        sbMuxs
      }
    }

    val sbMuxs = rec(baseAreaInfo.areaWithCap())
    val confBits = getAssociatedConfBits(params, sbMuxs)

    SBInfo(sbMuxs, confBits)
  }

  // Area with SB
  def getTileArea(
      params: GlobalParamsInst,
      root: RootPb,
      archs: List[Arch],
      tiles: Map[String, RootPb],
      conInfo: Map[Arch, RRGConInfo]
  ): TileAreaInfo = {
    val cbInfo = getCBMuxs(params, root, archs)
    val primsInfo = getPrimitives(params, root)
    val cmuxsInfo = getConfigMuxs(params, root)
    val sbInfo = getSBMuxs(params, tiles, archs, conInfo)

    TileAreaInfo(primsInfo, cmuxsInfo, cbInfo, sbInfo, root)
  }

  def getTileSB(
      tiles: Map[String, RootPb],
      archs: List[Arch],
      params: GlobalParamsInst,
      conInfo: Map[Arch, RRGConInfo]
  ): Map[Arch, Map[WireLength, TBlock]] = {
    val sbMuxs = getSBMuxs(params, tiles, archs, conInfo)

    sbMuxs.sbMuxs.toList
      .map {
        (mux, _) =>
          {
            val muxPrim = mux.prim.asInstanceOf[MuxConfig]
            val arch = archs.filter(_.width == muxPrim.p.width).head
            val wireLength = muxPrim.p.wireLength

            (arch, (wireLength, mux))
          }
      }
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2).toMap)
      )
  }

  def getBiggestTileArea(
      params: GlobalParamsInst,
      tiles: Map[String, RootPb],
      archs: List[Arch],
      conInfo: Map[Arch, RRGConInfo]
  ): TileAreaInfo = {
    tiles.map(_._2).map(getTileArea(params, _, archs, tiles, conInfo)).reduce {
      (a, b) =>
        {
          if (a.areaWithCap() > b.areaWithCap()) {
            a
          } else {
            b
          }
        }
    }
  }

  def getGridUnitArea(
      params: GlobalParamsInst,
      tiles: Map[String, RootPb],
      archs: List[Arch],
      conInfo: Map[Arch, RRGConInfo]
  ): Double = {
    val biggestPb = getBiggestPb(tiles, archs, params)
    val biggestCap = biggestPb.vprConfig.capacity

    val biggestAreaInfo = getBiggestTileArea(params, tiles, archs, conInfo)

    roundTileArea(biggestAreaInfo.areaWithCap())
  }

  def getTileSide(tileArea: Double): Double = {
    (ceil(sqrt(tileArea) / 10.0).toInt * 10).toInt
  }

  def roundTileArea(tileArea: Double): Double = {
    pow(getTileSide(tileArea), 2)
  }
}
