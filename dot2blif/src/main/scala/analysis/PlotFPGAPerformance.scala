package analysis

import frontend.GlobalParamsInst
import io.Source
import util.Util
import arch.TBlock
import frontend.Benchmark
import frontend.GlobalParams
import crkt.ElasticGraph
import readers.FPGAReport
import frontend.Plot

import sys.process._
import io.AnsiColor._
import math.ceil

object FPGAReports {
  def savePerformance(params: GlobalParamsInst, fpgaReport: FPGAReport, csvDir: String): Unit = {
    val header = "Benchmark, Area, Critical Path"
    val rows = params.bench + ", " + fpgaReport.area + ", " + fpgaReport.cp
    val csv = (header :: rows :: Nil).mkString("\n")

    val fName = csvDir + "/performance.csv"
    println(CYAN + "Printing: " + fName + RESET)

    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  def apply(params: GlobalParamsInst, fpgaReport: FPGAReport): Unit = {
    val csvDir = params.fpgaBuild + "/csv/"

    savePerformance(params, fpgaReport, csvDir)

    val bufPlacementLog = params.fpgaBuild + "/buffer-placement/" + params.bench + "/placement.log"
    Plot.saveIIs(params :: Nil)
  }
}
