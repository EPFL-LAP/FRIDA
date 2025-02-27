package frontend

import core._
import arch.Arch
import arch.PbType
import arch.Tile
import arch.Hs
import printers._
import routegen._
import sys.process._
import rrggen._
import readers._
import util.Util
import archs.ConfigurationBits
import archs.ConfParams

import io.AnsiColor._
import java.io.File
import java.io.FileOutputStream
import scala.util.{Failure, Success}
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import analysis._
import archs.Architectures
import collection.mutable.{Map => MMap}
import io.Source
import archs.Architectures.decoupled

object FrontEnd {
  def blue(s: String): String = BLUE + s + RESET
  def yellow(s: String): String = YELLOW + s + RESET
  def green(s: String): String = GREEN + s + RESET
  def red(s: String): String = RED + s + RESET

  // TODO Should be part of tools and the help is a mkString of the tools
  def printHelp = {
    println("dotToBlif utilization (one of):")
    println
    println(
      "dotToBlif (" + blue("pack")
        + "|" + blue("place")
        + "|" + blue("all")
        + "|" + blue("dynamatic")
        + "|" + blue("analyze")
        + ") " + "dot=" + green("dotName") + " arch=" + green("archName") + " "
        + "[" + yellow("withoutBuffers") + "] "
        + "[" + yellow("withoutDisplay") + "] "
        + "[" + yellow("timings") + "] "
        + "[" + yellow("scArch") + "]"
    )
    println("  " + blue("pack") + ": pack $" + green("dotName") + " onto $" + green("archName"))
    println("  " + blue("place") + ": pack and place $" + green("dotName") + " onto $" + green("archName"))
    println("  " + blue("all") + ": pack place and route $" + green("dotName") + " onto $" + green("archName"))
    println("  " + blue("dynamatic") + ": ???")
    println("  " + blue("analyze") + ": runs analysis passes on dotName when mapped onto archName")
    println("  " + yellow("withoutBuffers") + ": remove buffers from the netlist and architecture, if any")
    println("  " + yellow("withoutDisplay") + ": runs VPR without display")
    println(
      "  " + yellow("timings") + ": print timing information in VPR architecture. If not there, put a "
        + "dummy register on all ports from all primitives"
    )
    println("  " + yellow("scArch") + ": use the scala architecture description")
    println
    println(
      "dotToBlif " + blue("bench") + " arch=" + green("archName") + " "
        + "[" + yellow("withoutBuffers") + "] "
        + "[" + yellow("withoutDisplay") + "] "
        + "[" + yellow("timings") + "] "
        + "[" + yellow("scArch") + "]"
    )
    println("  " + blue("bench") + ": pack place and route all benchmarks onto $" + green("archName"))
    println
    println(
      "dotToBlif " + blue("allAnalyze") + " arch=" + green("archName") + " "
        + "[" + yellow("withoutBuffers") + "] "
        + "[" + yellow("timings") + "] "
        + "[" + yellow("scArch") + "]"
    )
    println("  " + blue("allAnalyze") + ": run all analysis passes on all benchmarks mapped onto $" + green("archName"))
    println
    println(
      "dotToBlif " + blue("genArch") + " arch=" + green("archName") + " "
        + "[" + yellow("timings") + "] "
        + "[" + yellow("scArch") + "]"
    )
    println("  " + blue("genArch") + ": runs the architecture generation only for $" + green("archName"))
    println
    println("dotToBlif " + blue("help"))
    println("  " + blue("help") + ": print this help message")
  }

  object test {
    val env = sys.env("ELASTIC_ROOT")
  }

  @main
  def apply(args: String*): Unit = {
    if (args.isEmpty) {
      printHelp
    } else {
      var ranSomething = false

      Tool.tools.foreach {
        tool =>
          {
            if (args.contains(tool.name)) {
              try {
                tool(args)
              } catch {
                case e => {
                  val errorStr = "[" + red("error") + "]"
                  val spacer = errorStr + "    at "

                  println(errorStr + " " + e)

                  e.getStackTrace().map(_.toString()).foreach {
                    case s"$pref($file:$loc)" => println(spacer + pref + "(" + green(file) + ":" + blue(loc) + ")")
                  }

                  if (e.getCause() != null) {
                    println(errorStr + " Caused by: " + e.getCause())
                  }
                }
              }

              ranSomething = true
            }
          }
      }

      if (!ranSomething) {
        printHelp
      }
    }
  }
}
