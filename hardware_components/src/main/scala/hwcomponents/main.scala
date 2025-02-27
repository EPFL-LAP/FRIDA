package components

import java.io.File
import java.io.FileWriter
import scala.io.AnsiColor._
import scala.util.Random
import archs.{Constant, Params, ForkParams, Fork => Frk}
import frontend.GlobalParams
import printers.DynamaticTimingPrinter.compNameToDynCompName
import readers.TimingReader
import components.TimingParser.translator
import archs.Blocks$package.{/*typeNameToParamsConfig,*/ typeNameToStaticInfo}
import components.ChiselComponent.primToChiselComp
import util.Util
import components.Generator.removeBlackBoxFileNames
import archs.ForkVariants
import scala.collection.parallel.CollectionConverters._
import scala.io.{Source => FileSource}
import frontend.GlobalParams.hwLib

object Main {
  val rawData = GlobalParams.root + "/build/hardware/raw_data"
  val verilog = "./build/hardware/verilog"
  val constraints = "./build/hardware/constraints"
  val dynamaticTimings = "./build/hardware/dyn_timings"

  def printHelp = {
    println("hwGen utilization (one of):")
    println(
      "hwGen " + BLUE + "gen" + RESET + ": Generates the verilog corresponding to the parameters "
        + "in Generator.scala"
    )
    println(
      "hwGen " + BLUE + "model" + RESET + s": Reads raw physical data from Synopsys/Innovus reports in $rawData and outputs canonicalised dot files in $hwLib"
    )
    println(
      "hwGen " + BLUE + "genTop" + RESET + s": Output the verilog corresponding to the Toplevel module"
    )
    println("hwGen " + BLUE + "help" + RESET + ": prints this help message")
  }

  def genVerilog() = {
    Generator(GlobalParams.verilog, GlobalParams.constraints)
  }

  def genWireComponents() = {
    Cst.wantedConfigurations.foreach { p =>
      {
        val compName = Constant(p).libName
        val fName = GlobalParams.hwLib + "/" + compName + ".cfg"

        println("Gen wire component delay: " + fName)

        val file = new File(fName)
        file.getParentFile().mkdirs()
        val writer = new FileWriter(file)

        writer.write(Cst.defaultDelays(p))
        writer.close()
      }
    }

    val wireForkP = ForkParams(0, 1, archs.EagerFork)
    val forkName = Frk(wireForkP).libName

    val fName = GlobalParams.hwLib + "/" + forkName + ".cfg"
    println("Gen wire component delay: " + fName)

    val file = new File(fName)
    file.getParentFile().mkdirs()
    val writer = new FileWriter(file)

    writer.write(Fork.defaultDelays(wireForkP))
    writer.close()
  }

  def genLib() = {
    val timingGraphs = (new File(rawData)).listFiles
      .filter(_.isDirectory)
      .map { dir =>
        {
          println("Handling " + dir.getName() + "...")

          val g =
            TimingParser(dir.getName(), dir.toString() + "/timing_full.rpt")
          DotPrinter(dir.toString() + "/full_timing_pre.dot", false)(g)
          val legalized = Legalizer(g)
          DotPrinter(dir.toString() + "/full_timing_post.dot", false)(legalized)

          val area = AreaReader(dir.getName(), dir.toString() + "/area.rpt")

          (dir.getName(), (legalized, area))
        }
      }
      .toMap

    // print timing in VPR format
    timingGraphs.foreach {
      case (compName, (g, area)) => {
        TimingPrinter(
          compName,
          g,
          area,
          GlobalParams.hwLib + "/" + compName + ".cfg"
        )
      }
    }

    // Some components are wire components and make the EDA tools fail, such as the constant
    // We generate a zero delay timing model for these components
    genWireComponents()

    // print timing in dynamatic format
    // val timingsGroupped = timingGraphs.toList.map {
    //   case (compName, (g, area)) => (compName, g)
    // }.groupBy(_._1.split("_")(0))

    // val dynDelay = dynamaticTimings + "/" + "delays.dat"
    // println(timingsGroupped.map(_._1).mkString("\n"))

    // val dynLat = dynamaticTimings + "/" + "latencys.dat"
    // println(timingsGroupped.map(_._1).mkString("\n"))

    // DynamaticTiming(timingsGroupped, dynDelay, dynLat)
  }

  def genRTLConfig(): Unit = {
    RTLConfig.generateRTLConfig("vhdl").writeToFile
    RTLConfig.generateRTLConfig("verilog").writeToFile
  }

  def genRTLComponent(args: Array[String]): Unit = {
    val outFP = args(1)
    val componentName = args(2)

    println(YELLOW + "Printing: " + outFP + RESET)

    val libRepOpt = if (args.length == 4) Some(args(3)) else None
    val libRep = libRepOpt.getOrElse("")
    println(
      s"Generating component $componentName with library representation $libRep"
    )
    val primStaticInfo = typeNameToStaticInfo(componentName)
    val unpackedParams = primStaticInfo.unpackLibRep(libRep)
    println(s"unpackedParams $unpackedParams")

    // now want chisel component
    val paramType = primStaticInfo.params // typeNameToParamsConfig(componentName)
    val chiselComp = primToChiselComp(primStaticInfo.typeString)
    val verilog = removeBlackBoxFileNames (
      ChiselComponent.emitVerilog(unpackedParams)._1
    )

    // want primitive to also have a "get equivalent chisel component"?

    val writer: FileWriter = Util.writeOpen(outFP)
    writer.write(verilog)
    writer.close()
    println(s"Finished writing component $componentName")
  }

  def main(args: Array[String]): Unit = {
    if (args.contains("help")) {
      printHelp
    } else if (args.contains("gen")) {
      genVerilog()
    } else if (args.contains("model")) {
      genLib()
    } else if (args.contains("genrtlconfig")) {
      genRTLConfig()
    } else if (args.contains("genrtlcomponent")) {
      genRTLComponent(args)
    } else {
      printHelp
    }
  }
}
