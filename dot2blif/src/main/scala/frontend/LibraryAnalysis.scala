package frontend

import analysis.LibraryAnalysis

object ReportLibraryStatistics extends Tool {
  val name = "LibraryAnalysis"

  def parseArguments(args: Seq[String]): Seq[GlobalParams] = {
    val archName = getArgVal(args, "arch=")
    val baseDir = GlobalParams.analysisBaseLoc + "/lib/"

    // GlobalParams(archName, "fir" :: Nil, baseDir) :: Nil
    ???
  }

  def run(params: GlobalParamsInst): Unit = {
    LibraryAnalysis(params)
  }

  def aggregate(params: Seq[GlobalParamsInst]): Unit = {}
}
