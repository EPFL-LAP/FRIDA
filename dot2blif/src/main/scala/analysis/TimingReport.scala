package analysis

import frontend.GlobalParamsInst
import readers.VprTimingReportReader
import readers.TimingInfo
import util.Util
import arch.Tile
import frontend.GlobalParamsInst
import frontend.GlobalParams

import sys.process._
import java.io.File
import readers.GeneralInt
import readers.LocalInt
import readers.ConfigInt
import readers.Compute
import math.max
import io.AnsiColor._

object GlobalTimingReports {
  def saveCriticalPaths(csvDir: String, pathsInfos: Map[GlobalParamsInst, TimingInfo]): Unit = {
    val critPaths = pathsInfos.map(
      (p, ts) => (p, ts.criticalPath.delay)
    )

    val header = "architecture, benchmark, config, cp"

    val rows = critPaths
      .map {
        (params, cp) =>
          {
            params.scArch.name + ", " + params.bench + ", " + params.config() + ", " + cp
          }
      }
      .mkString("\n")

    val fName = csvDir + "/critical_paths.csv"
    println(CYAN + "Printing: " + fName + RESET)

    val f = Util.writeOpen(fName)
    f.write(header + "\n")
    f.write(rows + "\n")
    f.close()
  }

  // Find for this seed and this set of paths
  // what is the new critical path if we increase a primitive delay by X.
  // Go over all primitives names
  //   Go over all paths, bump each contributioin of the primitive by X
  //   reccord the increase in CP normalized to the CP
  // Average over all seeds
  // Average across all benchmarks
  def saveCriticalPathsDetailed(csvDir: String, pathsInfos: Map[GlobalParamsInst, TimingInfo]): Unit = {
    val pathsScaled = (0 to 20 by 2).map {
      scaleInt =>
        {
          val scale = scaleInt / 10.0

          val configsInfo = pathsInfos.map {
            (params, ts) =>
              {
                // Critical path delay of each config
                val critDelay = ts.criticalPath.delay

                // Primitives in the critical path
                val primitives = ts.pathInfos.map(_.delaysFineGrained.toSeq).flatten.map(_._1).toSet

                // Increase in CP if primitive delay is relaxed by the scale
                val contribs = primitives.toSeq.map {
                  pName =>
                    {
                      // update the delays by the scale on the primtive
                      val nDelays = ts.pathInfos.map {
                        pi =>
                          {
                            pi.delaysFineGrained.map {
                              (pContrib, d) =>
                                {
                                  if (pName == pContrib) {
                                    d + d * scale
                                  } else {
                                    d
                                  }
                                }
                            }.sum
                          }
                      }

                      val nCp = nDelays.max
                      (pName, max(nCp / critDelay, 1.0))
                    }
                }

                contribs
              }
          }

          val contribInfo = configsInfo.flatten
            .groupBy(_._1)
            .map(
              (k, v) => (k, v.map(_._2))
            )
            .map {
              (pName, configContribs) =>
                {
                  (pName, configContribs.sum / configContribs.size)
                }
            }
            .toSeq

          // Return the contributions
          contribInfo.toSeq.map(
            (pName, contrib) => (pName, (contrib, scaleInt))
          )
        }
    }

    val csv = pathsScaled.flatten
      .groupBy(_._1)
      .map {
        (pName, paths) =>
          {
            pName + ", " + paths
              .map(_._2)
              .sortBy(_._2)
              .map(_._1)
              .map(
                d => f"$d%1.2f"
              )
              .mkString(", ")
          }
      }
      .mkString("\n")

    val header = "primitive, " + (0 to 20 by 2)
      .map(
        c => "crit " + (c / 10.0)
      )
      .mkString(", ") + "\n"

    val fName = csvDir + "/timing_sensitivity_cp.csv"
    println(CYAN + "Printing: " + fName + RESET)

    val f = Util.writeOpen(fName)

    f.write(header)
    f.write(csv + "\n")
    f.close()
  }

  def savePlacementCriticalPaths(csvDir: String, pathsInfos: Map[GlobalParamsInst, TimingInfo]): Unit = {
    val critPaths = pathsInfos.map {
      (params, ti) =>
        {
          (params, ti.placeInfo.costInfo.cp)
        }
    }

    val header = "experiment, benchmark, config\n"
    val critPathsStr = critPaths
      .map {
        (params, cp) =>
          {
            params.scArch.name + ", " + params.bench + ", " + params.config() + ", " + cp
          }
      }
      .flatten
      .mkString("\n")

    val fName = csvDir + "/placement_critical_paths.csv"
    println(CYAN + "Printing: " + fName + RESET)

    val f = Util.writeOpen(fName)
    f.write(header)
    f.write(critPathsStr + "\n")
    f.close()
  }

  def savePlacementCriticalPathsNorm(csvDir: String, pathsInfos: Map[GlobalParamsInst, TimingInfo]): Unit = {
    val critPaths = pathsInfos.map {
      (params, ti) =>
        {
          (params, ti.criticalPath.delay / ti.placeInfo.costInfo.cp)
        }
    }

    val header = "experiment, benchmark, config\n"
    val critPathsStr = critPaths
      .map {
        (params, cp) =>
          {
            params.scArch.name + ", " + params.bench + ", " + params.config() + ", " + cp
          }
      }
      .flatten
      .mkString("\n")

    val fName = csvDir + "/placement_critical_paths_norm.csv"
    println(CYAN + "Printing: " + fName + RESET)

    val f = Util.writeOpen(fName)
    f.write(header)
    f.write(critPathsStr + "\n")
    f.close()
  }

  def copyScripts(csvDir: String): Unit = {
    Seq("cp", GlobalParams.root + "/scripts/plot_cp.py", csvDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_cp_est.py", csvDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_cp_norm.py", csvDir).!!
    Seq("cp", GlobalParams.root + "/scripts/plot_var.py", csvDir).!!
  }

  def apply(params: Seq[GlobalParamsInst]): Unit = {
    // val tInfos = LocalTimingReports.getTimingInfo(params)

    // tInfos.groupBy(_._1.baseRun).map {
    //   (_, configs) => {
    //     // Configs here are unique
    //     val configTInfo = configs.toMap

    //     // Analysis Dir is before the configurations
    //     val csvDir = configs.head._1.analysisDir
    //     println(csvDir)

    //     Seq("mkdir", "-p", csvDir).!!
    //     copyScripts(csvDir)

    //     saveCriticalPaths(csvDir, configTInfo)
    //     saveCriticalPathsDetailed(csvDir, configTInfo)
    //     savePlacementCriticalPaths(csvDir, configTInfo)
    //     savePlacementCriticalPathsNorm(csvDir, configTInfo)
    //   }
    // }

    ???
  }
}
