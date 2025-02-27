package frontend

import core.CompilerState

object ArchGen extends Tool {
  val name = "ArchGen"

  def parseArguments(args: String*): List[GlobalParams] = {
    val archName = getArgVal(args, "arch=")
    val baseDir = GlobalParams.analysisBaseLoc + "/" + archName + "/genSimple/"

    // GlobalParams(archName, "fir" :: Nil, baseDir) :: Nil
    ???
  }

  def run(params: GlobalParamsInst): Unit = {
    CompilerState.genArch(params)
  }

  def aggregate(params: Seq[GlobalParamsInst]): Unit = {}
}
