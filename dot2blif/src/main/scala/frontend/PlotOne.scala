package frontend

import util.Util

import io.Source
import sys.process._
import io.AnsiColor._

object PlotOne extends Tool {
  val name = "plotOne"

  def parseArguments(args: String*): List[GlobalParams] = {
    val archName = getArgVal(args, "arch=")
    val dotName = getArgValOpt(args, "dot=")
    val cp = getCompositeArgValOpt(args, "cp=").fold(2.0 :: Nil)(_.map(_.toDouble))

    val baseDir = GlobalParams.defaultBaseDir(archName)

    val fpuParams = GlobalParams(
      archName,
      dotName.get :: Nil,
      baseDir,
      cp
    )

    fpuParams :: Nil
  }

  def run(params: GlobalParamsInst): Unit = {}

  def plotCps(allParams: Seq[GlobalParamsInst]): Unit = {
    allParams.groupBy(_.bench).map {
      (benchName, benchParams) =>
        {
          val header = "Objective, Area, CP, II"
          // val benchRows = benchParams.map {
          //   params => {
          //     // val perf = Plot.getFPUPerformanceCsv(params)
          //     // val ii = Plot.getFPUIICsv(params: GlobalParamsInst)
          //     // val cpObj = params.cp

          //     // (cpObj, perf.area, perf.cp, ii)

          //     ???
          //   }
          // }.sortBy(_._1).map {
          //   (obj, area, cp, ii) => {
          //     "" + obj + ", " + area + ", " + cp + ", " + ii
          //   }
          // }.toList

          val csv: String = ??? // (header :: benchRows).mkString("\n")

          val archName = allParams.filter(!_.fpga).head.scArch.name
          val csvDir = GlobalParams.root + "/build/" + archName + "/Default/" + benchName + "/csv/"

          ("mkdir -p " + csvDir).!!
          ("cp " + GlobalParams.root + "/scripts/plot_bench.py " + csvDir).!!

          val fName = csvDir + "bench.csv"
          println(CYAN + "Printing: " + fName + RESET)

          val f = Util.writeOpen(fName)
          f.write(csv)
          f.close()
        }
    }
  }

  def aggregate(params: Seq[GlobalParamsInst]): Unit = {
    plotCps(params)
  }
}
