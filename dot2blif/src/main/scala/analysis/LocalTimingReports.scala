package analysis

import frontend.GlobalParamsInst
import readers.VprTimingReportReader
import readers.TimingInfo
import util.Util
import arch.Tile
import frontend.GlobalParamsInst
import frontend.GlobalParams
import frontend.Plot

import sys.process._
import java.io.File
import readers.GeneralInt
import readers.LocalInt
import readers.ConfigInt
import readers.Compute
import math.max
import math.ceil
import io.AnsiColor._
import core.CompilerState
import arch.RootPb
import io.Source
import arch.PbType
import arch.PrimPb
import arch.InterPb
import packerv2.ImplementedMolecule
import archs.ConfigurationBits
import archs.ConfParams
import archs.DummyType
import core.ATileIo
import core.AIo
import crkt.ElasticGraph
import archs.DummyTypeParams

object LocalReports {
  // TODO why not save all of this in one file?
  def saveSegments(csvDir: String, pathsInfo: Map[GlobalParamsInst, TimingInfo]): Unit = {
    pathsInfo.map {
      (p, pInfo) =>
        {
          val paths = pInfo.pathInfos
            .map(
              pi => (pi.crit, pi.segments)
            )
            .sortBy(_._1)
          val pathsStr = paths
            .map(
              (del, segs) => ("" + del + ", " + segs)
            )
            .mkString("\n")

          val fName = csvDir + "/path_segments_" + p.config() + ".csv"
          println(CYAN + "Printing: " + fName + RESET)

          val f = Util.writeOpen(fName)
          f.write("path criticality, segments\n")
          f.write(pathsStr)
          f.close()
        }
    }
  }

  def savePrimitives(csvDir: String, pathsInfo: Map[GlobalParamsInst, TimingInfo]): Unit = {
    pathsInfo.map {
      (p, pInfo) =>
        {
          val paths = pInfo.pathInfos
            .map(
              pi => (pi.crit, pi.primitives)
            )
            .sortBy(_._1)
          val pathsStr = paths
            .map(
              (del, segs) => ("" + del + ", " + segs)
            )
            .mkString("\n")

          val fName = csvDir + "/path_primitives_" + p.config() + ".csv"
          println(CYAN + "Printing: " + fName + RESET)

          val f = Util.writeOpen(fName)
          f.write("path criticality, primitives\n")
          f.write(pathsStr)
          f.close()
        }
    }
  }

  def savePathType(csvDir: String, pathsInfo: Map[GlobalParamsInst, TimingInfo]): Unit = {
    pathsInfo.map {
      (p, pInfo) =>
        {
          val paths = pInfo.pathInfos
            .map(
              pi => (pi.delay, pi.pathType)
            )
            .sortBy(-_._1)
          val pathsStr = paths
            .map(
              (del, pt) => ("" + del + ", " + pt.str)
            )
            .mkString("\n")

          val fName = csvDir + "/path_type_" + p.config() + ".csv"
          println(CYAN + "Printing: " + fName + RESET)

          val f = Util.writeOpen(fName)
          f.write("path criticality, path type\n")
          f.write(pathsStr)
          f.close()
        }
    }
  }

  def savePathDir(csvDir: String, pathsInfo: Map[GlobalParamsInst, TimingInfo]): Unit = {
    pathsInfo.map {
      (p, pInfo) =>
        {
          val paths = pInfo.pathInfos
            .map(
              pi => (pi.delay, pi.pathDir)
            )
            .sortBy(-_._1)
          val pathsStr = paths
            .map(
              (del, pd) => ("" + del + ", " + pd.str)
            )
            .mkString("\n")

          val fName = csvDir + "/path_dir_" + p.config() + ".csv"
          println(CYAN + "Printing: " + fName + RESET)

          val f = Util.writeOpen(fName)
          f.write("path criticality, path direction\n")
          f.write(pathsStr)
          f.close()
        }
    }
  }

  def saveTileHops(csvDir: String, pathsInfo: Map[GlobalParamsInst, TimingInfo]): Unit = {
    pathsInfo.map {
      (p, pInfo) =>
        {
          val paths = pInfo.pathInfos
            .map(
              pi => (pi.delay, pi.tileHops)
            )
            .sortBy(-_._1)
          val pathsStr = paths
            .map(
              (del, hops) => ("" + del + ", " + hops)
            )
            .mkString("\n")

          val fName = csvDir + "/path_hops_" + p.config() + ".csv"
          println(CYAN + "Printing: " + fName + RESET)

          val f = Util.writeOpen(fName)
          f.write("path criticality, path type\n")
          f.write(pathsStr)
          f.close()
        }
    }
  }

  def saveManhattan(csvDir: String, pathsInfo: Map[GlobalParamsInst, TimingInfo]): Unit = {
    pathsInfo.map {
      (p, pInfo) =>
        {
          val crit = pInfo.criticalPath
          val info = crit.dist
            .zip(crit.manhattanDist)
            .map(
              (dist, man) => (dist.toDouble / man)
            )
            .zipWithIndex
            .map(
              (dist, i) => "" + i + ", " + dist
            )
            .mkString("\n")

          val fName = csvDir + "/path_dist_over_man_crit_" + p.config() + ".csv"
          println(CYAN + "Printing: " + fName + RESET)

          val f = Util.writeOpen(fName)
          f.write("Hop, Dist\n")
          f.write(info)
          f.close()
        }
    }
  }

  def saveManhattanAv(csvDir: String, pathsInfo: Map[GlobalParamsInst, TimingInfo]): Unit = {
    pathsInfo.map {
      (p, pInfo) =>
        {
          val dists = pInfo.pathInfos
            .map(
              pi => (pi.dist, pi.manhattanDist)
            )
            .map {
              (dists, mans) =>
                {
                  val distsOverMans = dists.zip(mans).map {
                    (dist, man) =>
                      {
                        if (man == 0) dist.toDouble else (dist.toDouble / man)
                      }
                  }

                  distsOverMans.sum / distsOverMans.size
                }
            }
            .zipWithIndex
            .map(
              (d, i) => ("" + i + ", " + d)
            )
            .mkString("\n")

          val fName = csvDir + "/path_dist_over_man_" + p.config() + ".csv"
          println(CYAN + "Printing: " + fName + RESET)

          val f = Util.writeOpen(fName)
          f.write("Crit, Dist\n")
          f.write(dists)
          f.close()
        }
    }
  }

  def savePathContribs(csvDir: String, pathsInfo: Map[GlobalParamsInst, TimingInfo]): Unit = {
    pathsInfo.map {
      (p, pInfo) =>
        {
          val header = (GeneralInt.str :: LocalInt.str :: ConfigInt.str :: Compute.str :: Nil).mkString(",")

          val paths = pInfo.pathInfos
            .map(
              pi => (pi.delay, pi.delays)
            )
            .sortBy(-_._1)
          val pathsStr = paths
            .map {
              (delay, delays) =>
                {
                  val gi = delays.getOrElse(GeneralInt, 0.0)
                  val li = delays.getOrElse(LocalInt, 0.0)
                  val ci = delays.getOrElse(ConfigInt, 0.0)
                  val comp = delays.getOrElse(Compute, 0.0)

                  "" + f"$gi%.3f" + ", " + f"$li%.3f" + ", " + f"$ci%.3f" + ", " + f"$comp%.3f"
                }
            }
            .mkString("\n")

          val fName = csvDir + "/path_dist_" + p.config() + ".csv"
          println(CYAN + "Printing: " + fName + RESET)

          val f = Util.writeOpen(fName)
          f.write(header + "\n")
          f.write(pathsStr + "\n")
          f.close()
        }
    }
  }

  def savePlacementInfo(csvDir: String, pathsInfo: Map[GlobalParamsInst, TimingInfo]): Unit = {
    pathsInfo.foreach {
      (p, pInfo) =>
        {
          val archInfo = pInfo.placeInfo.utilInfo.archInfo
          val circuitInfo = pInfo.placeInfo.utilInfo.netlistInfo

          val rows = archInfo
            .map {
              (t, num) =>
                {
                  t.name + ", " + circuitInfo(t) + ", " + num
                }
            }
            .mkString("\n")

          val header = "tile, circuit, architecture\n"

          val fName = csvDir + "/path_place_" + p + ".csv"
          println(CYAN + "Printing: " + fName + RESET)

          val f = Util.writeOpen(fName)
          f.write(header)
          f.write(rows + "\n")
          f.close()
        }
    }
  }

  // TODO place at a proper location
  def getBufferDepths(cs: CompilerState[RootPb]): Map[String, Map[Int, Int]] = {
    cs.crkt.nodes.toList
      .map(_._2)
      .filter(_.nType.isBuf)
      .map {
        n =>
          {
            val slots = n.nType.slots(n.nType.p)
            // println(n.name + ": " + slots)

            val width = if (n.nType.libName.contains("W1")) "W1" else "W32"

            (n.nType.typeString + width, slots)
          }
      }
      .groupBy(_._1)
      .map {
        (bName, slots) =>
          {
            val nSlots = slots
              .map(_._2)
              .groupBy(
                s => s
              )
              .map(
                (k, v) => (k, v.size)
              )
            (bName, nSlots)
          }
      }
  }

  def copyScripts(csvDir: String): Unit = {
    Seq("cp", GlobalParams.root + "/scripts/plot_dist.py", csvDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_dir.py", csvDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_cp_seeds.py", csvDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_hops.py", csvDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_type.py", csvDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_prims_over_segments.py", csvDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_placement_util.py", csvDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_packing_util.py", csvDir).!!
  }

  def getSBArea(params: GlobalParamsInst, cs: CompilerState[RootPb], gridInfo: GridInfo): Double = {
    AreaExtractor.getSBMuxs(params, cs.tiles, cs.archMasks, gridInfo.conInfo).area()
  }

  def getArea(cs: CompilerState[RootPb], params: GlobalParamsInst, gridInfo: GridInfo, tInfo: TimingInfo): Int = {
    val tilesArea = cs.tiles.map {
      (tName, t) =>
        {
          (tName, AreaExtractor.getTileUnitArea(params, t, cs.archMasks).area())
        }
    }

    val sbUnitArea = AreaExtractor.getSBMuxs(params, cs.tiles, cs.archMasks, gridInfo.conInfo).area()

    val tileUnitArea = cs.mols
      .filter(
        mol => mol.tile.name != "ioTile"
      )
      .map(
        mol => tilesArea(mol.tile.name)
      )
      .sum
    val sbArea = tInfo.placeInfo.utilInfo.locs
      .map(
        pli => (pli.x, pli.y)
      )
      .toSet
      .size * sbUnitArea

    ceil(tileUnitArea + sbArea).toInt
  }

  def savePerformance(
      csvDir: String,
      cs: CompilerState[RootPb],
      params: GlobalParamsInst,
      gridInfo: GridInfo,
      tInfo: TimingInfo
  ): Unit = {
    val area = getArea(cs, params, gridInfo, tInfo)

    val header = "Benchmark, Area, Critical Path\n"
    val content = params.bench + ", " + area + ", " + tInfo.criticalPath.delay

    val fName = csvDir + "/performance.csv"
    println(CYAN + "Printing: " + fName + RESET)

    val f = Util.writeOpen(fName)
    f.write(header)
    f.write(content)
    f.close()

    Seq("cp", GlobalParams.root + "/scripts/plot_performance.py", csvDir).!!
  }

  // Parse Union and find CFDFCs with no strict subsets in inner CFDFCs
  // Reccord their place in the union
  // Then, reccord their IIs

  def saveTiles(
      csvDir: String,
      params: GlobalParamsInst,
      cs: CompilerState[RootPb]
  ): Unit = {
    val count = cs.mols
      .map(_.tile.name)
      .groupBy(
        tName => tName
      )
      .map(
        (tName, num) => (tName, num.size)
      )
    val header = "Tile, Count"

    val rows = count
      .map(
        (k, v) => "" + k + ", " + v
      )
      .toList

    val csv = (header :: rows).mkString("\n")

    val fName = csvDir + "/tileUsage.csv"
    println(CYAN + "Printing: " + fName + RESET)

    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  def getUsedPrimitiveArea(mol: ImplementedMolecule, g: ElasticGraph, params: GlobalParamsInst): Int = {
    mol.blockMap.toList.filter(!_._1.p.isInstanceOf[DummyTypeParams]).map {
      (prim, nodeName) =>
        {
          val numConf = prim.prim.numConfBits(prim.prim.p)
          val conf = ConfigurationBits(ConfParams(numConf))

          val confInst = conf.instantiate(params)

          val n = g(nodeName)

          val primArea = n.nType.instantiate(params).physicalInfo.area
          val confArea = confInst.physicalInfo.area

          // println(nodeName + ": " + (primArea + confArea).toInt)

          (primArea + confArea).toInt
        }
    }.sum
  }

  // area of used primitives / area of tile, sum all normalize to 1
  def saveUtilizationArea(
      csvDir: String,
      cs: CompilerState[RootPb],
      params: GlobalParamsInst,
      gridInfo: GridInfo,
      tInfo: TimingInfo
  ): Unit = {
    val area = getArea(cs, params, gridInfo, tInfo)
    val primArea = cs.mols.toSeq.map {
      mol =>
        {
          getUsedPrimitiveArea(mol, cs.crkt, params)
        }
    }.sum

    val sbUnitArea = getSBArea(params, cs, gridInfo)

    val tileUtils = cs.mols
      .filter(!_.tile.annotations.contains(AIo))
      .toSeq
      .groupBy(_.getLoc(tInfo.placeInfo.utilInfo.locs))
      .map {
        (loc, mols) =>
          {
            val locPrimArea = mols.map(getUsedPrimitiveArea(_, cs.crkt, params)).sum

            val t = mols.head.tile
            assert(mols.forall(_.tile.name == t.name))

            val locArea = (sbUnitArea + mols.size * AreaExtractor.getTileUnitArea(params, t, cs.archMasks).area()).toInt

            (t.name -> (locPrimArea, locArea, mols.size))
          }
      }
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2))
      )
      .map {
        (tName, utils) =>
          {
            val (totPrim, totLoc, totMols) = utils.reduce(
              (a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3)
            )

            val tileAverageUtil = totPrim.toDouble / totLoc.toDouble
            val tileAverageLocUtil = totMols.toDouble / utils.size.toDouble

            (tName -> (tileAverageUtil, tileAverageLocUtil))
          }
      }

    val headerSuf = tileUtils
      .map(_._1)
      .toSeq
      .sorted
      .map {
        tName =>
          {
            tName + "Util" + ", " + tName + "LocUtil"
          }
      }
      .mkString(", ")
    val header = "Benchmark, Area, PrimArea, " + headerSuf

    val tileData = tileUtils.toSeq
      .sortBy(_._1)
      .map {
        (tName, data) =>
          {
            val tileUtil = data._1
            val tileLocUtil = data._2

            "" + f"$tileUtil%2.2f , $tileLocUtil%2.2f"
          }
      }
      .mkString(", ")

    val rows = params.bench + ", " + area + ", " + primArea + ", " + tileData
    val csv = (header :: rows :: Nil).mkString("\n")

    val fName = csvDir + "utilization.csv"

    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  // area of used primitives / area of primitives in tile, sum all normalize to 1
  def saveUtilizationAreaPrims(
    csvDir: String,
    cs: CompilerState[RootPb],
    params: GlobalParamsInst
  ): Unit = {
    val areaInfo = cs.mols
      .groupBy(_.tile.name)
      .map {
        (tName, mols) =>
          {
            val primsArea = AreaExtractor.getPrimitives(params, mols.head.tile).area() * mols.size

            val usedPrimsArea = mols.map {
              mol =>
                {
                  getUsedPrimitiveArea(mol, cs.crkt, params)
                }
            }.sum

            println(tName + " -> " + usedPrimsArea + "/" + primsArea + " (" + mols.size + ")")

            (tName -> (usedPrimsArea, primsArea))
          }
      }
      .toMap

    val utilization = areaInfo.map(_._2._1).sum.toDouble / areaInfo.map(_._2._2).sum.toDouble

    val control = areaInfo("Control")._1.toDouble / areaInfo("Control")._2.toDouble
    val arith = areaInfo("Arith")._1.toDouble / areaInfo("Arith")._2.toDouble
    val ancil = areaInfo("Ancillary")._1.toDouble / areaInfo("Ancillary")._2.toDouble

    val header = "Benchmark, Control, Arithmetic, Ancillary, Total"
    val rows = params.bench + ", " + f"$control%3.2f," + f"$arith%3.2f," + f"$ancil%3.2f," + f"$utilization%3.2f"

    val csv = (header :: rows :: Nil).mkString("\n")
    val fName = csvDir + "utilization_prims.csv"

    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  def saveASICArea(
    csvDir: String,
    cs: CompilerState[RootPb],
    params: GlobalParamsInst
  ): Unit = {
    val asicArea = cs.mols.map {
      mol =>
      {
        getUsedPrimitiveArea(mol, cs.crkt, params)
      }
    }.sum

    val header = "Benchmark, ASIC"
    val rows = params.bench + ", " + asicArea

    val csv = (header :: rows :: Nil).mkString("\n")
    val fName = csvDir + "asic_area.csv"

    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  def getPrimsCount(root: RootPb): Int = {
    def rec(pbs: List[PbType], acc: Int): Int = {
      if (pbs.isEmpty) {
        acc
      } else {
        val pb = pbs.head

        pb match {
          case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
            rec(pbs.tail, acc + 1)
          }

          case InterPb(bi, links, modeLinks, c, annotations, name, subBlocks) => {
            rec(pbs.tail ++ subBlocks, acc)
          }

          case RootPb(bi, links, modeLinks, c, annotations, name, subBlocks, vprConfig) => {
            rec(pbs.tail ++ subBlocks, acc)
          }
        }
      }
    }

    rec(root :: Nil, 0)
  }

  // count used primitives / # prims in tile, sum all normalize to 1
  def saveUtilizationCount(
      csvDir: String,
      cs: CompilerState[RootPb],
      params: GlobalParamsInst
  ): Unit = {
    val areaInfo = cs.mols
      .groupBy(_.tile.name)
      .map {
        (tName, mols) =>
          {
            val primsCount = getPrimsCount(mols.head.tile) * mols.size
            val usedPrimsCount = mols.map(_.primMap.size).sum

            (tName -> (usedPrimsCount, primsCount))
          }
      }
      .toMap

    val utilization = areaInfo.map(_._2._1).sum.toDouble / areaInfo.map(_._2._2).sum.toDouble

    val control = areaInfo("Control")._1.toDouble / areaInfo("Control")._2.toDouble
    val arith = areaInfo("Arith")._1.toDouble / areaInfo("Arith")._2.toDouble
    val ancil = areaInfo("Ancillary")._1.toDouble / areaInfo("Ancillary")._2.toDouble

    val header = "Benchmark, Control, Arithmetic, Ancillary, Total"
    val rows = params.bench + ", " + f"$control%3.2f," + f"$arith%3.2f," + f"$ancil%3.2f," + f"$utilization%3.2f"

    val csv = (header :: rows :: Nil).mkString("\n")
    val fName = csvDir + "utilization_count.csv"

    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  // Contribution of each tile to the overall area.
  def saveAreaDetailed(
      csvDir: String,
      cs: CompilerState[RootPb],
      gridInfo: GridInfo,
      params: GlobalParamsInst,
      tInfo: TimingInfo
  ): Unit = {
    val tilesArea = cs.tiles.map {
      (tName, t) =>
        {
          (tName, AreaExtractor.getTileUnitArea(params, t, cs.archMasks).area())
        }
    }

    val sbUnitArea = AreaExtractor.getSBMuxs(params, cs.tiles, cs.archMasks, gridInfo.conInfo).area()

    val tileUnitArea = cs.mols
      .map(_.tile.name)
      .groupBy(
        tName => tName
      )
      .map {
        (tName, count) =>
          {
            (tName, count.size * tilesArea(tName))
          }
      }

    val control = tileUnitArea("Control").toInt
    val ancillary = tileUnitArea("Ancillary").toInt
    val arith = tileUnitArea("Arith").toInt
    val io = tileUnitArea("ioTile").toInt

    val numSbs = tInfo.placeInfo.utilInfo.locs
      .map(
        pli => (pli.x, pli.y)
      )
      .toSet
      .size
    val areaSbs = (numSbs * sbUnitArea).toInt

    val header = "Benchmark, Control, Arithmetic, Ancillary, SB Count, SB Area"
    val rows = (
      params.bench + ", " + f"$control%7d, " + f"$ancillary%7d, " + f"$arith%7d, "
        + f"$numSbs%7d, " + f"$areaSbs%7d, "
    )

    val csv = (header :: rows :: Nil).mkString("\n")
    val fName = csvDir + "area_contrib_detailed.csv"

    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  def apply(cs: CompilerState[RootPb], params: GlobalParamsInst, gridInfo: GridInfo): Unit = {
    val timingRpt = params.vprDir + "/report_timing.setup.rpt"
    val placeLog = params.vprDir + "/place_full.log"
    val placeFile = params.vprDir + "/circuit.place"

    val tInfo = VprTimingReportReader(params, timingRpt, placeLog, placeFile)
    val csvDir = params.buildDir + "/csv/"

    savePerformance(csvDir, cs, params, gridInfo, tInfo)
    Plot.saveIIs(params :: Nil)
    saveTiles(csvDir, params, cs)
    saveUtilizationArea(csvDir, cs, params, gridInfo, tInfo)
    saveUtilizationAreaPrims(csvDir, cs, params)
    saveASICArea(csvDir, cs, params)

    // saveUtilizationArea(csvDir, cs, params)
    // saveUtilizationCount(csvDir, cs, params)
    // saveAreaDetailed(csvDir, cs, gridInfo, params, tInfo)
  }
}
