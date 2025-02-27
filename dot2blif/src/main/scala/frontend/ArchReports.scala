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

object ArchReports extends Tool {
  val name = "ArchReports"

  def parseArguments(args: String*): List[GlobalParams] = {
    val archName = getArgVal(args, "arch=")
    val baseDir = GlobalParams.analysisBaseLoc + "/" + archName + "/"

    GlobalParams(archName, "fir" :: Nil, baseDir, Benchmark.bufDefaultObj :: Nil, parallel = true) :: Nil
  }

  def run(params: GlobalParamsInst): Unit = {
    println(
      YELLOW + " -> " + RESET
        + "mapping " + GREEN + params.bench + RESET
        + " onto " + CYAN + params.scArch.name + RESET
        + " with seed " + MAGENTA + params.seed + RESET + "."
    )

    // TODO it would be nice to have one option in the frontend to generate the dots and the timing
    // TODO representation
    DynamaticTimingPrinter

    if (!Benchmark.cachedRRG(params)) {
      Benchmark.cacheRRG(params)
    }

    Benchmark.moveRRG(params)

    val parsedCs = CompilerState.frontEnd(params)
    val cs = CompilerState.defaultPipeline(params).get

    // ArchPrinter(params, cs.tiles, cs.archMasks, None)
    BlifPrinter(params, cs.crkt, cs.archMasks, cs.mols, cs.tiles.map(_._2).toList)
    PackPrinter(params, cs.mols, cs.archMasks, cs.crkt, cs.tiles.map(_._2).toList)

    val gridInfo = GridInfoExtractor(params, cs.archMasks, cs.tiles)
    ArchPrinter(params, cs.tiles, cs.archMasks, Some(gridInfo))
    ArchitectureLegalizer(cs.archMasks, cs.tiles, params, Some(gridInfo))

    AnalysisArch(params, cs.tiles, cs.archMasks)
  }

  def aggregate(params: Seq[GlobalParamsInst]): Unit = {}
}
