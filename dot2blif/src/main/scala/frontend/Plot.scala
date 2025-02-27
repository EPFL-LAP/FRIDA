package frontend

import util.Util

import io.Source
import sys.process._
import io.AnsiColor._
import readers.FPGAReportReader.parseAreaReport
import analysis.FPGAReports
import readers.FPGAReportReader

case class GeneralPerformance(
    params: GlobalParamsInst,
    area: Double,
    cp: Double,
    iis: List[Int],
    tileAreas: Map[String, Int]
) {
  val bench = params.bench

  def perf(): Double = iis.map(_ * cp).sum / iis.size

  override def toString(): String = {
    val p = perf()
    val a = area.toInt

    f"$bench%15s" + ": " + f"$a%7d" + ", " + f"$cp%2.2f" + ", " + f"$p%2.2f" + ", " + iis.mkString(", ")
  }
}

case class DetailedAreaContribInfo(params: GlobalParamsInst, tileAreas: Map[String, Int]) {
  override def toString(): String = {
    params.bench + ": " + tileAreas
      .map(
        (k, v) => k + " -> " + v
      )
      .mkString(", ")
  }
}

case class DetailedUtilizationInfo(params: GlobalParamsInst, tileAreas: Map[String, Double]) {
  override def toString(): String = {
    params.bench + ": " + tileAreas
      .map(
        (k, v) => k + " -> " + v
      )
      .mkString(", ")
  }
}

object Plot extends Tool {
  val name = "plot"

  def parseArguments(args: String*): List[GlobalParams] = {
    val archNames = getCompositeArgValOpt(args, "arch=").get
    val dotName = getArgValOpt(args, "dot=")
    val cpFPGAs = getCompositeArgValOpt(args, "cpf=").fold(4.0 :: Nil)(_.map(_.toDouble))
    val cpas = getCompositeArgValOpt(args, "cpa=").fold(2.0 :: Nil)(_.map(_.toDouble))

    val dotList = if (dotName.isEmpty) {
      Benchmark.benchList
    } else {
      dotName.get :: Nil
    }

    archNames.map {
      archName =>
        {
          val baseDir = GlobalParams.defaultBaseDir(archName)

          val fpuParams = GlobalParams(
            archName,
            dotList,
            baseDir,
            cpas,
            fpga = false
          )

          val fpgaParams = GlobalParams(
            archName,
            dotList,
            baseDir,
            cpFPGAs,
            fpga = true
          )

          fpuParams :: fpgaParams :: Nil
        }
    }.flatten
  }

  def run(params: GlobalParamsInst): Unit = {}

  def parsePerformanceReport(fName: String): Option[(String, Int, Double)] = {
    if (new java.io.File(fName).exists) {
      val perf = Source.fromFile(fName).getLines().toList.tail.head.split(",")

      val bench = perf(0).trim()
      val area = perf(1).trim().toInt
      val cp = perf(2).trim().toDouble

      Some((bench, area, cp))
    } else {
      None
    }
  }

  def getBestPerformance(benchParams: Seq[GlobalParamsInst]): Option[GeneralPerformance] = {
    benchParams
      .map {
        params =>
          {
            val iisOpt = if (params.fpga) {
              val placementLog = params.fpgaBuild + "/buffer-placement/" + params.bench + "/placement.log"
              getInnermostCFDFCsIIs(placementLog)
            } else {
              val placementLog = params.buildMlirDir + "/buffer-placement/" + params.bench + "/placement.log"
              getInnermostCFDFCsIIs(placementLog)
            }

            val perfOpt = if (params.fpga) {
              parsePerformanceReport(params.fpgaBuild + "/csv/performance.csv")
            } else {
              parsePerformanceReport(params.buildDir + "/csv/performance.csv")
            }

            if (iisOpt.nonEmpty && perfOpt.nonEmpty) {
              val area = parseAreaBreakdownReport(params)

              if (iisOpt.get.forall(_._2 != -1)) {
                val gp = GeneralPerformance(params, perfOpt.get._2, perfOpt.get._3, iisOpt.get.map(_._2), area)
                Some(gp)
              } else {
                None
              }
            } else {
              None
            }
          }
      }
      .flatten
      .reduceOption(
        (p0, p1) => if (p0.perf() < p1.perf()) p0 else p1
      )
  }

  def plotPerformanceComparison(
      allParams: Seq[GlobalParamsInst],
      partition: GlobalParamsInst => Boolean,
      p0: String,
      p1: String
  ): Unit = {
    val normalizedPerf = allParams
      .groupBy(_.bench)
      .map {
        (benchName, params) =>
          {
            println
            println("finding best performance for : " + benchName)

            val fpuParams = params.filter(!partition(_))
            val fpgaParams = params.filter(partition(_))

            val fpuPerf = getBestPerformance(fpuParams)
            val fpgaPerf = getBestPerformance(fpgaParams)

            println("fpu : " + fpuPerf)
            println("fpga: " + fpgaPerf)

            if (fpuPerf.isEmpty || fpgaPerf.isEmpty) {
              None
            } else {
              Some((fpuPerf.get, fpgaPerf.get))
            }
          }
      }
      .flatten

    val header = "Benchmark, Area, Critical Path"
    val rows = normalizedPerf
      .map {
        (fpuPerf, fpgaPerf) =>
          {
            assert(fpuPerf.bench == fpgaPerf.bench)

            // println
            // println("best fpu -> " + fpuPerf)
            // println("best fpga -> " + fpgaPerf)

            val bench = fpuPerf.bench
            val areaRatio = fpuPerf.area / fpgaPerf.area
            val perfRatio = fpuPerf.perf() / fpgaPerf.perf()

            if ((fpuPerf.perf() > 0) && (fpgaPerf.perf() > 0)) {
              f"$bench%s, $areaRatio%2.2f, $perfRatio%2.3f"
            } else {
              ""
            }
          }
      }
      .filter(_.nonEmpty)
      .mkString("\n")

    val csv = (header :: rows :: Nil).mkString("\n")

    val archName = allParams.filter(!_.fpga).head.scArch.name
    val csvDir = GlobalParams.defaultBaseDir(archName) + "/csv/"

    ("mkdir -p " + csvDir).!!
    ("cp " + GlobalParams.root + "/scripts/plot_performance.py " + csvDir).!!

    val fName = csvDir + "performance" + p0 + "_" + p1 + ".csv"
    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()

    val headerAll = f"Benchmark, CP $p0, CP $p1, IIs $p0, IIs $p1, II x CP Ratio"
    val rowsAll = normalizedPerf.toList.sortWith {
      (p0, p1) => {
        val perf0 = p0._1.perf() / p0._2.perf()
        val perf1 = p1._1.perf() / p1._2.perf()

        perf0 < perf1
      }
    }.map {
        (fpuPerf, fpgaPerf) =>
          {
            assert(fpuPerf.bench == fpgaPerf.bench)

            val bench = fpuPerf.bench
            val fpuArea = fpuPerf.area.toInt
            val fpgaArea = fpgaPerf.area.toInt
            val fpuCP = fpuPerf.cp
            val fpgaCP = fpgaPerf.cp
            val fpuIICP = fpuPerf.perf()
            val fpgaIICP = fpgaPerf.perf()
            val fpuIIs = fpuPerf.iis.mkString(" / ")
            val fpgaIIs = fpgaPerf.iis.mkString(" / ")

            val ratio = fpuIICP / fpgaIICP

            if ((fpuPerf.perf() > 0) && (fpgaPerf.perf() > 0)) {
              f"$bench%s, $fpuCP%2.2f, $fpgaCP%2.2f, $fpuIIs%s, $fpgaIIs%s, $ratio%2.3f"
            } else {
              ""
            }
          }
      }
      .filter(_.nonEmpty)
      .mkString("\n")

    val csvAll = (headerAll :: rowsAll :: Nil).mkString("\n")

    val fNameAll = csvDir + "performance_all" + p0 + "_" + p1 + ".csv"
    val fAll = Util.writeOpen(fNameAll)
    fAll.write(csvAll)
    fAll.close()
  }

  def getEdgeInfo(params: GlobalParamsInst, buf: Boolean): Map[String, Int] = {
    val fName = if (buf) {
      params.buildDir + "/csv/ext_edges_buf.csv"
    } else {
      params.buildDir + "/csv/ext_edges_pre_buf.csv"
    }

    Source
      .fromFile(fName)
      .getLines()
      .toList
      .tail
      .map {
        s =>
          {
            val edge = s.split(",")(0).trim()
            val count = s.split(",")(1).trim().toInt

            (edge -> count)
          }
      }
      .toMap
  }

  def plotExternalEdges(allParams: Seq[GlobalParamsInst], buf: Boolean): Unit = {
    val header = "Edge, Count"
    val rows = allParams
      .filter(!_.fpga)
      .map {
        params =>
          {
            getEdgeInfo(params, buf)
          }
      }
      .flatten
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2).sum)
      )
      .toList
      .sortBy(-_._2)

    val maxValue = rows.map(_._2).max

    val rowsStrs = rows.map {
      (edge, count) =>
        {
          val countNorm = (count / maxValue.toDouble)
          edge + ", " + f"$countNorm%2.2f"
        }
    }

    val csv = (header :: rowsStrs).mkString("\n")

    val archName = allParams.filter(!_.fpga).head.scArch.name
    val csvDir = GlobalParams.defaultBaseDir(archName) + "/csv/"

    ("mkdir -p " + csvDir).!!
    ("cp " + GlobalParams.root + "/scripts/plot_ext_edges.py " + csvDir).!!

    val fName = if (buf) {
      csvDir + "ext_edges_buf.csv"
    } else {
      csvDir + "ext_edges.csv"
    }

    // println(CYAN + "Printing: " + fName + RESET)

    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  def getBestFPGAIIandCP(allParams: Seq[GlobalParamsInst]): Unit = {
    allParams.filter(_.fpga).groupBy(_.bench).toList.sortBy(_._1).map {
      (bench, cpParams) =>
        {
          val minII = cpParams
            .filter {
              cpParam =>
                {
                  assert(cpParam.cp.size == 1)
                  val bufPlacementLog = cpParam.fpgaBuild + "/buffer-placement/" + cpParam.bench + "/placement.log"
                  val iis = getInnermostCFDFCsIIs(bufPlacementLog)

                  iis.isDefined
                }
            }
            .map {
              cpParam =>
                {
                  assert(cpParam.cp.size == 1)
                  val bufPlacementLog = cpParam.fpgaBuild + "/buffer-placement/" + cpParam.bench + "/placement.log"
                  val iis = getInnermostCFDFCsIIs(bufPlacementLog).get.map(_._2)

                  (iis, cpParam.cp.head)
                }
            }
            .reduce {
              (a, b) =>
                {
                  if (a._1 == b._1) {
                    if (a._2 < b._2) {
                      a
                    } else {
                      b
                    }
                  } else if (a._1.sum < b._1.sum) {
                    a
                  } else {
                    b
                  }
                }
            }

          println(f"$bench%15s" + " achieved a min II of " + minII._1 + " with " + minII._2)
        }
    }
  }

  def flattenParams(allParams: Seq[GlobalParamsInst]): Seq[GlobalParamsInst] = {
    allParams.map {
      params =>
        {
          params.cp.map {
            cp =>
              {
                val (nBuildDir, nAnalysisDir) = if (params.fpga) {
                  (params.buildDir, params.analysisDir)
                } else {
                  val cpStr = f"_cp$cp%1.2f"
                  val seedStr = "seed_" + params.seed

                  val nBuild = params.buildDir.replace("base", "") + seedStr + cpStr + "/"
                  val nAnalysis = nBuild + "/csv/"

                  (nBuild, nAnalysis)
                }

                val nParams = GlobalParamsInst(
                  params.scArch,
                  params.bench,
                  nBuildDir,
                  params.mlirDir,
                  nAnalysisDir,
                  params.withBuffers,
                  params.enableDisplay,
                  params.timings,
                  params.seed,
                  cp :: Nil,
                  params.vprSettings,
                  params.forcePack,
                  params.forceRun,
                  params.parallel,
                  params.fpga,
                  params.fromDot,
                  params.mode,
                  params.config
                )

                nParams
              }
          }
        }
    }.flatten
  }

  def getBBCycle(s: String): List[Int] = {
    s.split(":")(1).trim().split("->").toList.map(_.trim().toInt)
  }

  def getCFDFCLoc(log: List[String], num: Int): (Int, Int) = {
    var found = false
    var loc = 0
    var unionNum = -1

    log.foreach {
      line =>
        {
          if (line.contains("CFDFC Union")) {
            if (!found) {
              loc = 0
              unionNum = unionNum + 1
            }
          }

          if (line.contains("- CFDFC #")) {
            if (line.contains("- CFDFC #" + num)) {
              found = true
            }

            if (!found) {
              loc = loc + 1
            }
          }
        }
    }

    (unionNum, loc)
  }

  // CFDFC Number to CFDFC location in the union
  def getInnermostCFDFCs(placementLog: List[String]): List[(Int, (Int, Int))] = {
    val startLoc = placementLog.indexWhere(_.contains("Disjoint CFDFCs Unions"))
    val endLoc = placementLog.indexWhere(_.contains("Buffer Placement Decisions"))

    val unionLog = placementLog.drop(startLoc).take(endLoc - startLoc)

    val cfdfcs = unionLog.filter(_.contains("- CFDFC #"))
    val cfdfcsBBCycles = cfdfcs.map {
      cfdfc =>
        {
          val num = cfdfc.split(":")(0).split("#")(1).toInt
          val bbInfo = getBBCycle(cfdfc)

          (num, bbInfo)
        }
    }

    // TODO FIXME
    // assumes that the index 0 is always the innermost CFDFC
    // If the innermost is not strictly within the above, then we pick it as an if then else
    val innermostCFDFCs = cfdfcsBBCycles.filter {
      (num, bbInfo) =>
        {
          val (union, loc) = getCFDFCLoc(unionLog, num)

          !cfdfcsBBCycles.filter(_._1 != num).exists {
            (oNum, oBBInfo) =>
              {
                val (oUnion, oLoc) = getCFDFCLoc(unionLog, oNum)

                if ((oUnion == union) && (oLoc < loc)) {
                  oBBInfo.forall(bbInfo.contains(_))
                } else {
                  false
                }
              }
          }
        }
    }

    innermostCFDFCs
      .map {
        (num, _) =>
          {
            val (union, loc) = getCFDFCLoc(unionLog, num)

            (union, (num, loc))
          }
      }
      .sortBy(_._1)
  }

  def getThroughputs(placementLog: List[String]): List[List[String]] = {
    var keep = false

    val allThroughputs = collection.mutable.ListBuffer[List[String]]()
    val current = collection.mutable.ListBuffer[String]()

    placementLog.foreach {
      line =>
        {
          if (line.contains("CFDFC Throughputs")) {
            keep = true
          } else if (line.contains("Channel Throughputs")) {
            if (current.nonEmpty) {
              allThroughputs += current.toList
            }

            current.clear()
            keep = false
          }

          if (keep && line.contains("Throughput of CFDFC")) {
            current += line
          }
        }
    }

    allThroughputs.toList
  }

  def getInnermostCFDFCsIIs(placementLog: String): Option[List[(Int, Int)]] = {
    if (new java.io.File(placementLog).exists) {
      val placementLogContent = Source.fromFile(placementLog).getLines().toList

      val innermostCFDFC = getInnermostCFDFCs(placementLogContent)
      val throughputs = getThroughputs(placementLogContent).map(_.filter(_.trim().nonEmpty)).filter(_.nonEmpty)

      val iis = throughputs.zipWithIndex.map {
        (throughputsStrs, unionNum) =>
          {
            innermostCFDFC.filter(_._1 == unionNum).map(_._2).map {
              (num, loc) =>
                {
                  val iiStr = throughputsStrs.filter(_.contains("#" + loc)).head
                  val ii = math.round(1 / (iiStr.split(":")(1).trim().toDouble)).toInt

                  (num, ii)
                }
            }
          }
      }.flatten

      Some(iis)
    } else {
      None
    }
  }

  // TODO this assumes the ordering is preserved in the LOG, would probably be better to change the placement.log
  // output to not print CFDFC indices, but keep the original numbering...
  def printInnermostCFDFCsIIs(
      csvDir: String,
      placementLog: String
  ): Unit = {
    val iis = getInnermostCFDFCsIIs(placementLog)

    if (iis.isDefined) {
      val header = "Cfdfc, II"
      val rows = iis.get
        .sortBy(_._1)
        .map(
          (k, v) => "" + k + ", " + v
        )

      val csv = (header :: rows).mkString("\n")

      val fName = csvDir + "/ii.csv"
      // println(CYAN + "Printing: " + fName + RESET)

      val f = Util.writeOpen(fName)
      f.write(csv)
      f.close()
    }
  }

  def saveIIs(allParams: Seq[GlobalParamsInst]): Unit = {
    allParams.map {
      params =>
        {
          if (params.fpga) {
            val bufPlacementLog = params.fpgaBuild + "/buffer-placement/" + params.bench + "/placement.log"
            val csvDir = params.fpgaBuild + "/csv/"

            printInnermostCFDFCsIIs(csvDir, bufPlacementLog)
          } else {
            val bufPlacementLog = params.buildMlirDir + "/buffer-placement/" + params.bench + "/placement.log"
            val csvDir = params.buildDir + "/csv/"

            printInnermostCFDFCsIIs(csvDir, bufPlacementLog)
          }
        }
    }
  }

  def savePerformance(allParams: Seq[GlobalParamsInst]): Unit = {
    allParams.map {
      params =>
        {
          if (params.fpga) {
            val bufPlacementLog = params.fpgaBuild + "/buffer-placement/" + params.bench + "/placement.log"
            val csvDir = params.fpgaBuild + "/csv/"

            printInnermostCFDFCsIIs(csvDir, bufPlacementLog)
          } else {
            val bufPlacementLog = params.buildMlirDir + "/buffer-placement/" + params.bench + "/placement.log"
            val csvDir = params.buildDir + "/csv/"

            printInnermostCFDFCsIIs(csvDir, bufPlacementLog)
          }
        }
    }
  }

  def parseAreaBreakdownReport(params: GlobalParamsInst): Map[String, Int] = {
    if (params.fpga) {
      val areaInfo = FPGAReportReader(params).areaInfo

      (("CLB" -> areaInfo.slices) :: ("DSP" -> areaInfo.dsps) :: Nil).toMap
    } else {
      val areaCsv = params.buildDir + "/csv/tileUsage.csv"

      Source
        .fromFile(areaCsv)
        .getLines()
        .toList
        .tail
        .map {
          line =>
            {
              val tName = line.split(",")(0).trim()
              val count = line.split(",")(1).trim().toInt

              (tName, count)
            }
        }
        .toMap // .filter(_._1 != "ioTile")
    }
  }

  def plotAreaComparison(allParams: Seq[GlobalParamsInst]): Unit = {
    val areas = allParams
      .groupBy(_.bench)
      .map {
        (benchName, params) =>
          {
            val fpuParams = params.filter(!_.fpga)
            val fpgaParams = params.filter(_.fpga)

            val fpuPerf = getBestPerformance(fpuParams)
            val fpgaPerf = getBestPerformance(fpgaParams)

            if (fpuPerf.isEmpty || fpgaPerf.isEmpty) {
              None
            } else {
              Some((fpuPerf.get, fpgaPerf.get))
            }
          }
      }
      .flatten

    val fpuTiles = areas.head._1.tileAreas.map(_._1).toList.sorted.mkString(", ")
    val fpgaTiles = areas.head._2.tileAreas.map(_._1).toList.sorted.mkString(", ")

    val header = "Benchmark, " + fpuTiles + ", Area FPU, " + fpgaTiles + ", Area FPGA, Area Ratio"
    val rows = areas
      .filter {
        (fpuPerf, fpgaPerf) =>
          {
            (fpuPerf.perf() > 0) && (fpgaPerf.perf() > 0)
          }
      }
      .toList
      .sortBy {
        (fpuPerf, fpgaPerf) =>
          {
            fpuPerf.area / fpgaPerf.area
          }
      }
      .map {
        (fpuPerf, fpgaPerf) =>
          {
            val bench = fpuPerf.bench

            val fpuTiles = fpuPerf.tileAreas.toList
              .sortBy(_._1)
              .map(
                (_, count) => f"$count%2d"
              )
              .mkString(", ")
            val fpgaTiles = fpgaPerf.tileAreas.toList
              .sortBy(_._1)
              .map(
                (_, count) => f"$count%2d"
              )
              .mkString(", ")

            val areaRatio = fpuPerf.area / fpgaPerf.area
            val areaRatioStr = f"$areaRatio%2.2f"

            val fpuArea = fpuPerf.area.toInt
            val fpgaArea = fpgaPerf.area.toInt

            bench + ", " + fpuTiles + ", " + f"$fpuArea%7d, " + fpgaTiles + ", " + f"$fpgaArea%7d, " + areaRatioStr
          }
      }

    val csv = (header :: rows).mkString("\n")

    val archName = allParams.filter(!_.fpga).head.scArch.name
    val csvDir = GlobalParams.defaultBaseDir(archName) + "/csv/"

    val fName = csvDir + "area_detailed.csv"

    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  def parseAreaReport(params: GlobalParamsInst): Option[DetailedAreaContribInfo] = {
    val fName = params.analysisDir + "/" + "area_contrib_detailed.csv"
    if (new java.io.File(fName).exists) {
      val report = Source.fromFile(fName).getLines().toList

      val header = report.head.split(",").toList
      val content = report.tail.head.split(",").toList

      val bench = content.head.trim()

      val tileAreas = header.tail
        .zip(content.tail)
        .filter(!_._1.contains("SB Count"))
        .map {
          (tName, area) =>
            {
              (tName.trim(), area.trim().toInt)
            }
        }
        .toMap

      Some(DetailedAreaContribInfo(params, tileAreas))
    } else {
      None
    }
  }

  def plotAreaDist(allParams: Seq[GlobalParamsInst]): Unit = {
    val areas = allParams
      .groupBy(_.bench)
      .map {
        (benchName, params) =>
          {
            val perf = getBestPerformance(params)

            if (perf.nonEmpty) {
              parseAreaReport(perf.get.params)
            } else {
              None
            }
          }
      }
      .flatten

    val header = "Benchmark, " + areas.head.tileAreas.map(_._1).toList.sorted.mkString(", ")
    val rows = areas.map {
      areaInfo =>
        {
          areaInfo.params.bench + ", " + areaInfo.tileAreas.toList.sortBy(_._1).map(_._2).mkString(", ")
        }
    }.toList

    val csv = (header :: rows).mkString("\n")

    val archName = allParams.filter(!_.fpga).head.scArch.name
    val csvDir = GlobalParams.defaultBaseDir(archName) + "/csv/"

    val fName = csvDir + "area_contrib.csv"
    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  def parseUtilizationReport(params: GlobalParamsInst): Option[DetailedUtilizationInfo] = {
    val fName = params.analysisDir + "/" + "utilization_prims.csv"
    if (new java.io.File(fName).exists) {
      val report = Source.fromFile(fName).getLines().toList

      val header = report.head.split(",").toList
      val content = report.tail.head.split(",").toList

      val bench = content.head.trim()

      val tileAreas = header.tail
        .zip(content.tail)
        .filter(!_._1.contains("SB Count"))
        .map {
          (tName, utilization) =>
            {
              (tName.trim(), utilization.trim().toDouble)
            }
        }
        .toMap

      Some(DetailedUtilizationInfo(params, tileAreas))
    } else {
      None
    }
  }

  def plotUtilization(allParams: Seq[GlobalParamsInst]): Unit = {
    val utilization = allParams
      .groupBy(_.bench)
      .map {
        (benchName, params) =>
          {
            val perf = getBestPerformance(params)

            if (perf.nonEmpty) {
              parseUtilizationReport(perf.get.params)
            } else {
              None
            }
          }
      }
      .flatten
      .toList

    val header = "Benchmark, " + utilization.head.tileAreas.map(_._1).toList.sorted.mkString(", ")
    val rows = utilization
      .sortBy(-_.tileAreas("Total"))
      .map {
        areaInfo =>
          {
            areaInfo.params.bench + ", " + areaInfo.tileAreas.toList.sortBy(_._1).map(_._2).mkString(", ")
          }
      }
      .toList

    val csv = (header :: rows).mkString("\n")

    val archName = allParams.filter(!_.fpga).head.scArch.name
    val csvDir = GlobalParams.defaultBaseDir(archName) + "/csv/"

    val fName = csvDir + "utilization.csv"
    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  def aggregate(params: Seq[GlobalParamsInst]): Unit = {
    val allParams = flattenParams(params)

    val fpgaParams = allParams.filter(_.fpga)

    allParams.filter(!_.fpga).groupBy(_.scArch.name).map {
      (archName, archParams) =>
        {

          getBestFPGAIIandCP(archParams ++ fpgaParams)
          saveIIs(archParams ++ fpgaParams)
          plotPerformanceComparison(archParams ++ fpgaParams, (p: GlobalParamsInst) => p.fpga, "FPU", "FPGA")
          plotAreaComparison(archParams ++ fpgaParams)

          if (archName == "decoupled") {
            plotAreaDist(archParams)
            plotUtilization(archParams)
          }
        }
    }

    val nonFPGA = allParams.filter(!_.fpga)
    val archNames = nonFPGA.map(_.scArch.name).toSet.toList

    if (archNames.size == 2) {
      val a0 = archNames.head
      val a1 = archNames.tail.head

      plotPerformanceComparison(nonFPGA, (p: GlobalParamsInst) => p.scArch.name == a0, a0, a1)
    }

    // plotExternalEdges(allParams, true)
    // plotExternalEdges(allParams, false)
  }
}
