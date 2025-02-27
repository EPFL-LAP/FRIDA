package analysis

import arch.PbType
import arch.RootPb
import arch.TBlock
import arch.Arch
import frontend.GlobalParamsInst
import util.Util
import archs.ConfigurationBits
import archs.ConfParams
import archs.DummyType
import frontend.GlobalParams

import sys.process._
import math.ceil

object AnalysisArch {
  def printIoInfo(reportDir: String, tiles: Map[String, RootPb]): Unit = {
    tiles.map {
      (tName, t) =>
        {
          val fName = reportDir + "/" + tName + "_io.csv"
          println(fName)

          val f = Util.writeOpen(reportDir + "/" + tName + "_io.csv")
          f.write("portType, portWidth, numIoPorts\n")

          t.ioInfo().foreach {
            case ((pt, width), num) => {
              f.write(pt.str + ", " + width + ", " + num + "\n")
            }
          }

          f.close()
        }
    }
  }

  def printTileArea(params: GlobalParamsInst, reportDir: String, tilesArea: Map[String, TileAreaInfo]): Unit = {
    val fName = reportDir + "/" + "tiles_areas.csv"
    println(fName)

    val f = Util.writeOpen(fName)
    f.write("tileName, area\n")

    tilesArea.foreach {
      (tName, areaInfo) =>
        {
          f.write(tName + ", " + areaInfo.areaWithCap() + "\n")
        }
    }

    f.close()
  }

  def printTileClassAreaWithCap(
      params: GlobalParamsInst,
      reportDir: String,
      tilesArea: Map[String, TileAreaInfo],
      tiles: Map[String, RootPb]
  ): Unit = {
    val fName = reportDir + "/" + "tiles_area_dist_cap.csv"
    println(fName)

    val f = Util.writeOpen(reportDir + "/" + "tiles_area_dist_cap.csv")

    f.write("tName, SB, Logic, Buffers, Configuration Multiplexers, CB, total\n")

    tilesArea.filter(_._1 != "ioTile").foreach {
      (tName, areaInfo) =>
        {
          val tile = tiles(tName)

          val computePrims = PrimitiveInfo(areaInfo.prims.prims.filter(!_._1.prim.isBuf), areaInfo.prims.confBits)
          val bufferPrims = PrimitiveInfo(areaInfo.prims.prims.filter(_._1.prim.isBuf), Map())

          val logic = ceil(computePrims.area()).toInt * areaInfo.tile.vprConfig.capacity
          val buffers = ceil(bufferPrims.area()).toInt * areaInfo.tile.vprConfig.capacity
          val config = ceil(areaInfo.cmuxs.area()).toInt * areaInfo.tile.vprConfig.capacity
          val cb = ceil(areaInfo.cb.area()).toInt * areaInfo.tile.vprConfig.capacity
          val sb = ceil(areaInfo.sb.area()).toInt
          val total = ceil(areaInfo.areaWithCap()).toInt

          val tStr = tName + ", " + sb + ", " + logic + ", " + buffers + ", " + config + ", " + cb + ", " + total

          f.write(tStr + "\n")
        }
    }

    f.close()
  }

  def printTileClassArea(
      params: GlobalParamsInst,
      reportDir: String,
      tilesArea: Map[String, TileAreaInfo],
      tiles: Map[String, RootPb]
  ): Unit = {
    val fName = reportDir + "/" + "tiles_area_dist.csv"
    println(fName)

    val f = Util.writeOpen(reportDir + "/" + "tiles_area_dist.csv")

    f.write("tName, SB, Logic, Buffers, Configuration Multiplexers, CB, total\n")

    tilesArea.filter(_._1 != "ioTile").foreach {
      (tName, areaInfo) =>
        {
          val tile = tiles(tName)

          val computePrims = PrimitiveInfo(areaInfo.prims.prims.filter(!_._1.prim.isBuf), areaInfo.prims.confBits)
          val bufferPrims = PrimitiveInfo(areaInfo.prims.prims.filter(_._1.prim.isBuf), Map())

          val logic = ceil(computePrims.area()).toInt
          val buffers = ceil(bufferPrims.area()).toInt
          val config = ceil(areaInfo.cmuxs.area()).toInt
          val cb = ceil(areaInfo.cb.area()).toInt
          val sb = ceil(areaInfo.sb.area()).toInt
          val total = ceil(areaInfo.area()).toInt

          val tStr = tName + ", " + sb + ", " + logic + ", " + buffers + ", " + config + ", " + cb + ", " + total

          f.write(tStr + "\n")
        }
    }

    f.close()
  }

  def printSBAreaDecomp(
      params: GlobalParamsInst,
      reportDir: String,
      tiles: Map[String, RootPb],
      archs: List[Arch],
      conInfo: Map[Arch, RRGConInfo]
  ): Unit = {
    val fName = reportDir + "/" + "sb_dist.csv"
    println(fName)

    val f = Util.writeOpen(fName)
    f.write("rMux, muxAra, num, totalArea, areaContrib\n")

    val areaInfo = AreaExtractor.getBiggestTileArea(params, tiles, archs, conInfo)
    val sbArea = areaInfo.area()

    areaInfo.sb.sbMuxs.map {
      (b, num) =>
        {
          val numConf = b.prim.numConfBits(b.prim.p)
          val conf = ConfigurationBits(ConfParams(numConf))

          val confInst = conf.instantiate(params)

          val rMuxUnitArea = (b.physicalInfo.area + confInst.physicalInfo.area)
          val rMuxArea = (b.physicalInfo.area + confInst.physicalInfo.area) * num
          val contrib = rMuxArea / sbArea

          f.write(b.prim.libName + ", " + rMuxUnitArea + ", " + num + ", " + rMuxArea + ", " + f"$contrib%.2f" + "\n")
        }
    }

    f.close()
  }

  def printTileClassAreaConf(
      params: GlobalParamsInst,
      reportDir: String,
      tilesArea: Map[String, TileAreaInfo]
  ): Unit = {
    val fName = reportDir + "/" + "tiles_conf_dist.csv"
    println(fName)

    val f = Util.writeOpen(fName)
    f.write("tile name, primitive, CMux, CB, SB, total\n")

    tilesArea.foreach {
      (tName, areaInfo) =>
        {
          val primConf = areaInfo.prims.confArea()
          val cmuxConf = areaInfo.cmuxs.confArea()
          val cbConf = areaInfo.cb.confArea()
          val sbConf = areaInfo.sb.confArea()
          val total = areaInfo.confArea()

          val tStr = tName + ", " + primConf + ", " + cmuxConf + ", " + cbConf + ", " + sbConf + ", " + total

          f.write(tStr + "\n")
        }
    }

    f.close()
  }

  def printTileBlocksInfo(params: GlobalParamsInst, reportDir: String, tilesArea: Map[String, TileAreaInfo]): Unit = {
    tilesArea.foreach {
      (tName, areaInfo) =>
        {
          val fName = reportDir + "/" + tName + "_blocks.csv"
          println(fName)

          val f = Util.writeOpen(fName)
          f.write("prim name, num, area\n")

          val blocks = areaInfo.prims.prims
            .filter(!_._1.prim.isInstanceOf[DummyType])
            .map {
              (prim, num) =>
                {
                  val area = prim.physicalInfo.area

                  (prim.prim.libName + ", " + num + ", " + area)
                }
            }
            .mkString("\n")

          f.write(blocks)
          f.close()
        }
    }
  }

  def apply(params: GlobalParamsInst, tiles: Map[String, RootPb], archs: List[Arch]): Unit = {
    val conInfo = RRGConnectivityExtractor(params, archs)

    val tilesArea = tiles.map {
      (tName, t) =>
        {
          (tName, AreaExtractor.getTileArea(params, t, archs, tiles, conInfo))
        }
    }

    // printConfigMuxLocs(params, tiles, blocks)

    val reportDir = GlobalParams.analysisBaseLoc + "/" + params.scArch.name + "/csv/"

    Seq("mkdir", "-p", reportDir).!!

    Seq("cp", GlobalParams.root + "/scripts/plot_conf.py", reportDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_area_dist.py", reportDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_compute_dist.py", reportDir).!!

    printIoInfo(reportDir, tiles)
    printTileArea(params, reportDir, tilesArea)
    printSBAreaDecomp(params, reportDir, tiles, archs, conInfo)

    printTileClassArea(params, reportDir, tilesArea, tiles)
    printTileClassAreaWithCap(params, reportDir, tilesArea, tiles)
    printTileClassAreaConf(params, reportDir, tilesArea)

    // Rename the Block names
    printTileBlocksInfo(params, reportDir, tilesArea)
  }
}
