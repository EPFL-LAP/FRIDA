package core

import crkt._

import frontend.GlobalParamsInst

// TODO this is super ugly and needs to be refactored....

trait Analysis {
  def aggregate(g: ElasticGraph): Unit
  def print()(implicit params: GlobalParamsInst): Unit

  // TODO remove this function
  def getFName()(implicit params: GlobalParamsInst): String = {
    // val dotName = params.dot.split("/").last.replace(".dot", "")
    // val analysisName = this.getClass().getName().replace("$", "").replace("analysis.", "")

    // params.analysisDir + "/crkts/" + dotName + "/" + analysisName + ".csv"

    ???
  }

  def apply(g: ElasticGraph, params: GlobalParamsInst): Unit = {
    aggregate(g)
  }
}
