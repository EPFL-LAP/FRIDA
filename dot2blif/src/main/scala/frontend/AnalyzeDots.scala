package frontend

import analysis._
import core.CompilerState
import collection.mutable.{Map => MMap}

object Analyze extends Tool {
  val name = "Analyze"

  def parseArguments(args: String*): List[GlobalParams] = {
    val archName = getArgVal(args, "arch=")
    // GlobalParams(archName, Benchmark.benchList, GlobalParams.analysisBaseLoc + "/crkt/") :: Nil
    ???
  }

  def run(params: GlobalParamsInst): Unit = {
    val analysis = FindForkSizes(MMap[Int, Int]()) :: AnalysisType(MMap[String, Int]()) :: Nil

    val parsedCs = CompilerState.frontEnd(params)
    // val cs = CompilerState.defaultPipeline(params, None, Nil, true, false)
    val cs = CompilerState.defaultPipeline(params).get

    analysis.foreach(
      an => an(cs.crkt, params)
    )
  }

  def aggregate(params: Seq[GlobalParamsInst]): Unit = {}
}
