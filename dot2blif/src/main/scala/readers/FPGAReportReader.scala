package readers

import frontend.FPGABenchmark
import frontend.GlobalParamsInst

import io.Source
import math.ceil

case class FPGAAreaInfo(slices: Int, dsps: Int, slicesArea: Int, dspsArea: Int) {
  lazy val totalArea = slicesArea + dspsArea
}

case class FPGAReport(cp: Double, areaInfo: FPGAAreaInfo) {
  lazy val area = areaInfo.totalArea
}

object FPGAReportReader {
  val clbRefArea = ceil(921).toInt // ceil(921 / 2.0).toInt // 2 CLBs are 921, per Jason
  val dspRefArea = ceil(8064).toInt // ceil(8064 / 2.0).toInt // 2 DSPs are 8064, per Jason
  val ramRefArea = 16237 / 2 // 16237 // 36kB RAM, per Jason

  def parseTimingReport(params: GlobalParamsInst, fName: String): Double = {
    val slack = Source
      .fromFile(fName)
      .getLines
      .toList
      .filter(_.contains("Slack"))
      .head
      .split("""\s+""")(3)
      .replace("ns", "")
      .toDouble

    assert(params.cp.size == 1)

    params.cp.head - slack
  }

  def parseAreaReport(params: GlobalParamsInst, fName: String): FPGAAreaInfo = {
    val rptStr = Source.fromFile(fName).getLines.toList

    val slices = rptStr
      .filter(_.contains("Slice"))
      .filter(!_.contains("Logic"))
      .filter(!_.contains("LUT"))
      .filter(!_.contains("Register"))
      .head
      .split('|')(2)
      .trim()
      .toInt

    val dsps = rptStr.filter(_.contains("DSPs")).head.split('|')(2).trim().toInt

    val sliceArea = slices * clbRefArea
    val dspArea = dsps * dspRefArea

    FPGAAreaInfo(slices, dsps, sliceArea, dspArea)
  }

  def apply(params: GlobalParamsInst): FPGAReport = {
    val synthDir = params.fpgaBuild + "/synth/"

    val timingReport = synthDir + "/timing_post_pr.rpt"
    val areaReport = synthDir + "/utilization_post_pr.rpt"

    val cp = parseTimingReport(params, timingReport)
    val areaInfo = parseAreaReport(params, areaReport)

    FPGAReport(cp, areaInfo)
  }
}
