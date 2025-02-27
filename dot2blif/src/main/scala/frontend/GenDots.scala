package frontend

import archs.ArchDef
import analysis.AnalysisArch

import io.AnsiColor._
import printers.DynamaticTimingPrinter
import core.CompilerState
import printers.BlifPrinter
import printers.PackPrinter
import analysis.GridInfoExtractor
import rrggen.ArchitectureLegalizer
import printers.ArchPrinter

object GenDots extends Tool {
  val name = "GenDots"

  def parseArguments(args: String*): List[GlobalParams] = {
    GlobalParams (
      "",
      "dummy" :: Nil,
      GlobalParams.root + "/build/benchmarks/dot/",
      10.0 :: Nil,
      fpga=true,
    ) :: Nil
  }

  def run(params: GlobalParamsInst): Unit = {
    println(YELLOW + "Generating CGRA-ME dots. " + RESET)

    generators.SimpleChoice(params)
    generators.AsymChoice(params)
  }

  def aggregate(params: Seq[GlobalParamsInst]): Unit = {}
}


