package frontend

import archs.ArchDef
import archs.Architectures

import io.AnsiColor._
import scala.util.{Failure, Success}
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.FileOutputStream
import java.io.File

object Tool {
  val tools = List(
    ArchReports,
    Benchmark,
    Explore,
    ReportLibraryStatistics,
    ArchGen,
    Analyze,
    FPGABenchmark,
    Plot,
    PlotOne,
    GenDots
  )
}

trait Tool {
  def getArgValOrDef(args: Seq[String], argType: String, default: String): String = {
    args
      .filter(
        arg => arg.contains(argType)
      )
      .map(_.replace(argType, ""))
      .headOption
      .fold(default)(
        s => s
      )
  }

  def getArgVal(args: Seq[String], argType: String): String = {
    args
      .filter(
        arg => arg.contains(argType)
      )
      .map(_.replace(argType, ""))
      .headOption
      .fold(scala.sys.error("Expected argument: " + argType))(
        s => s
      )
  }

  def getArgValOpt(args: Seq[String], argType: String): Option[String] = {
    args
      .filter(
        arg => arg.contains(argType)
      )
      .map(_.replace(argType, ""))
      .headOption
  }

  def getCompositeArgValOpt(args: Seq[String], argType: String): Option[List[String]] = {
    args
      .filter(
        arg => arg.contains(argType)
      )
      .map(_.replace(argType, ""))
      .headOption
      .map(
        s => s.split(",").toList
      )
  }

  def name: String
  def parseArguments(args: Seq[String]): Seq[GlobalParams]
  def run(params: GlobalParamsInst): Unit

  // TODO change the name of this function
  def aggregate(params: Seq[GlobalParamsInst]): Unit

  def apply(args: Seq[String]): Unit = {
    val params = parseArguments(args)
    val insts = params.map(_.getInstances).flatten

    insts.map(run(_))
    // parallelRun(insts)
    aggregate(insts)
  }

  def parallelRun(params: Seq[GlobalParamsInst]): Unit = {
    def wrapper(pi: GlobalParamsInst): GlobalParamsInst = {
      run(pi)
      pi
    }

    val runs = params.map {
      pi =>
        {
          Future {
            val log = new FileOutputStream(new File("/dev/null"))
            Console.withOut(log) {
              wrapper(pi)
            }
          }
        }
    }

    runs.foreach {
      f =>
        {
          Await.ready(f, Duration.Inf).value.get match {
            case Success(pi) => println(GREEN + pi.buildDir + " suceeded" + RESET)
            case Failure(e) => {
              throw e
            }
          }
        }
    }

    aggregate(params)
  }
}
