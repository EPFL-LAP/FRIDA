package frontend

import analysis.ParsePerformanceMetrics

object PerfReports extends Tool {
  val name = "PerfReports"

  def parseArguments(args: String*): List[GlobalParams] = {
    val archName = getArgVal(args, "arch=")
    val baseDir = GlobalParams.defaultBaseDir(archName)

    // GlobalParams(archName, Benchmark.benchList, baseDir) :: Nil

    ???
  }

  def run(params: GlobalParamsInst): Unit = {
    println("mapping " + params.bench + " onto " + params.scArch.name)
    ParsePerformanceMetrics(params)
  }

  def aggregate(params: Seq[GlobalParamsInst]): Unit = {}
}
