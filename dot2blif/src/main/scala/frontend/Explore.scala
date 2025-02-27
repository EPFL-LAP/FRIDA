package frontend

import archs._
import arch.Arch
import arch.PbType
import core._
import util.Util

import sys.process._
import java.io.FileOutputStream
import java.io.File
import analysis.RRGConnectivityExtractor
import analysis.AreaExtractor
import analysis.AreaInfo

// TODO is this actually used??
case class RoutingConfig(archs: List[Arch], biggestTileInfo: Option[AreaInfo]) {
  override def toString(): String = {
    archs
      .map {
        arch =>
          {
            arch.name + ": (" + arch.fcIn + ", " + arch.fcOut + ") -> " + arch.chanWidth
          }
      }
      .mkString("\n")
  }
}

object Explore extends Tool {
  val name = "explore"

  // For now, same for both architectures
  val fcInRange = (5 until 50 by 10).map(_ / 100.0).map(VPRFC(Frac, _))
  val fcOutRange = ((5 until 50 by 10).map(_ / 100.0).map(VPRFC(Frac, _)))

  def archParamsStringRep(fcIn: VPRFC, fcOut: VPRFC): String = {
    "fcin" + fcIn + "_fcout" + fcOut
  }

  def findFcArchConfigParams(archName: String, fcIn: VPRFC, fcOut: VPRFC): GlobalParams = {
    val arch = Architectures(archName)

    val masks = arch.archs.filter(_.place).head :: arch.archs.filter(!_.place).map {
      a =>
        {
          Arch(a.name, a.width, a.pbs, a.place, -1, fcIn, fcOut)
        }
    }

    val archDef = ArchDef(
      arch.name,
      arch.tiles,
      arch.layout,
      arch.switchLengths,
      masks
    )

    val archInstName = archParamsStringRep(fcIn, fcOut)
    val baseDir = GlobalParams.root + "/build/explorer/" + archName + "/" + archInstName + "/"
    val vprParams = VPRParams.default :: Nil

    // TODO not sure what to put in the benchmark buffering objective here....
    GlobalParams(
      archDef,
      Benchmark.benchList,
      baseDir,
      true,
      false,
      true,
      1 :: Nil,
      ???,
      vprParams,
      false,
      false,
      false,
      false,
      false,
      ???
    )
  }

  def findAllConfigParams(archName: String): List[GlobalParams] = {
    fcInRange
      .map {
        fcIn =>
          {
            fcOutRange.map {
              fcOut =>
                {
                  findFcArchConfigParams(archName, fcIn, fcOut)
                }
            }
          }
      }
      .flatten
      .toList
  }

  def parseArguments(args: String*): List[GlobalParams] = {
    val archName = getArgVal(args, "arch=")
    findAllConfigParams(archName)
  }

  def run(params: GlobalParamsInst): Unit = {
    Benchmark.run(params)
  }

  def parseRoutingWidth(params: GlobalParamsInst): Seq[Arch] = {
    val fullArchs = params.scArch.archs.filter(!_.place).map {
      arch =>
        {
          val logFile = params.buildDir + "/route_" + arch.name + ".log"
          val chanWidth = io.Source
            .fromFile(logFile)
            .getLines()
            .toList
            .filter(_ contains "Circuit successfully routed with a channel width factor of")
            .head
            .filter(_.isDigit)
            .toInt

          val fName = params.buildDir + "/routing" + arch.name + ".csv"
          val f = Util.writeOpen(fName)

          f.write("archName, chanWidth, fcIn, fcOut\n")
          f.write(arch.name + ", " + arch.chanWidth + ", " + arch.fcIn + ", " + arch.fcOut)
          f.close()

          Arch(arch.name, arch.width, arch.pbs, arch.place, chanWidth, arch.fcIn, arch.fcOut)
        }
    }

    fullArchs
  }

  def getWorstArchPerFCs(archs: Seq[Arch]): Seq[Arch] = {
    archs
      .groupBy(_.name)
      .map {
        case (archName, archs) => {
          archs
            .groupBy(
              arch => (arch.fcIn, arch.fcOut)
            )
            .map {
              case ((fcIn, fcOut), archs) => {
                archs.reduce {
                  (a0, a1) =>
                    {
                      if (a0.chanWidth > a1.chanWidth) {
                        a0
                      } else {
                        a1
                      }
                    }
                }
              }
            }
        }
      }
      .flatten
      .toSeq
  }

  def printChanInfo(archs: Seq[Arch], archName: String): Unit = {
    archs.groupBy(_.name).foreach {
      case (routeName, configs) => {
        val rows = configs
          .map(
            arch => "" + arch.fcIn + ", " + arch.fcOut + ", " + arch.chanWidth
          )
          .mkString("\n")

        val fName = GlobalParams.root + "/build/explorer/" + archName + "/csv/routing_configs_" + routeName + ".csv"
        val f = Util.writeOpen(fName)
        f.write("fcIn, fcOut, chanWidth\n")
        f.write(rows)
        f.close()
      }
    }

    Seq(
      "cp",
      GlobalParams.root + "/scripts/plot_channels.py",
      GlobalParams.root + "/build/explorer/" + archName + "/csv/"
    ).!!
  }

  def relaxArchs(archs: Seq[Arch], factor: Double): Seq[Arch] = {
    archs.map {
      arch =>
        {
          val proposedChans = (arch.chanWidth * factor).toInt
          val nChans = if (proposedChans % 2 == 1) proposedChans + 1 else proposedChans
          Arch(arch.name, arch.width, arch.pbs, arch.place, nChans, arch.fcIn, arch.fcOut)
        }
    }
  }

  // For each architecture, finds a potential best candidate
  // A best candidate is one that has the minimum number of channels and less FCin / FCout
  // For two configurations with the same number of channels and inverted FCin / FCout values
  // Prefer the one with bigger FCout.
  def findMinChanBest(archs: Seq[Arch]): Map[String, Arch] = {
    archs.groupBy(_.name).map {
      (archName, archs) =>
        {
          val minChannels = archs
            .reduce(
              (a0, a1) => if (a0.chanWidth < a1.chanWidth) a0 else a1
            )
            .chanWidth
          val archsWithMinChans = archs.filter(_.chanWidth == minChannels)

          val bestArch = archsWithMinChans.foldLeft(archsWithMinChans.head) {
            case (acc, arch) => {
              val accFcSum = acc.fcIn.value + acc.fcOut.value
              val archFcSum = arch.fcIn.value + arch.fcOut.value

              if (accFcSum < archFcSum) {
                acc
              } else if (archFcSum < accFcSum) {
                arch
              } else {
                if (acc.fcOut.value > arch.fcOut.value) {
                  acc
                } else {
                  arch
                }
              }
            }
          }

          (archName, bestArch)
        }
    }
  }

  def printAreaInfo(archs: Seq[Arch], archName: String, relaxFactor: Double): Unit = {
    val relaxedArchs = relaxArchs(archs, relaxFactor)
    val bestArchs = findMinChanBest(relaxedArchs)

    relaxedArchs.groupBy(_.name).foreach {
      case (routeArchName, configs) => {
        val archConfig = bestArchs.filter(_._1 != routeArchName).map(_._2).toList

        val refArchDef = Architectures(archName)

        val rows = configs
          .map {
            config =>
              {
                val allArchs = config :: archConfig
                val archDef = ArchDef(
                  refArchDef.name,
                  refArchDef.tiles,
                  refArchDef.layout,
                  refArchDef.switchLengths,
                  Arch.place(100) :: allArchs
                )
                val archP = allArchs
                  .map {
                    arch => arch.name + archParamsStringRep(arch.fcIn, arch.fcOut)
                  }
                  .mkString("__") + "__relax" + relaxFactor

                println(archP)

                // Re-generate the architecture
                val bDir =
                  GlobalParams.root + "/build/explorer/" + archName + "/area/" + routeArchName + "/" + archP + "/"
                val aDir = bDir + "/csv/"
                val dot = GlobalParams.dotPath + "/" + "fir" + ".dot"
                val mlir: String = ??? // GlobalParams.mlirPath + "/" + "fir" + ".mlir" // TODO check this
                val vprParams = VPRParams.default

                val params = GlobalParamsInst(
                  archDef,
                  dot,
                  mlir,
                  bDir,
                  aDir,
                  true,
                  false,
                  false,
                  1,
                  ???, // TODO Not sure what to put here... Depends on what we chose to use as the best...
                  vprParams,
                  false,
                  false,
                  false,
                  false,
                  ???,
                  ???,
                  () => ""
                )

                val logFName = bDir + "log.log"
                val log = new FileOutputStream(new File(logFName))
                val cs = Console.withOut(log) {
                  Console.withErr(log) {
                    CompilerState.genArch(params)
                  }
                }

                // Get architecture connectivity information
                val conInfo = Console.withOut(log) {
                  Console.withErr(log) {
                    RRGConnectivityExtractor(params, cs.archMasks)
                  }
                }

                // Collect Area Information
                val areaInfo = AreaExtractor.getBiggestTileArea(params, cs.tiles, cs.archMasks, conInfo)

                val unitArea = areaInfo.area()
                val capArea = areaInfo.areaWithCap()
                val sbArea = areaInfo.sb.area()
                val cbAreaUnit = areaInfo.cb.area()
                val cbArea = areaInfo.cb.area() * areaInfo.tile.vprConfig.capacity

                val row = "" + config.fcIn + ", " + config.fcOut + ", "
                  + unitArea + ", " + capArea + ", " + cbAreaUnit + ", " + cbArea

                row
              }
          }
          .mkString("\n")

        val fName =
          GlobalParams.root + "/build/explorer/" + archName + "/csv/routing_configs_area_" + routeArchName + ".csv"

        val f = Util.writeOpen(fName)
        f.write("fcIn, fcOut, unitArea, capArea, sbArea, cbAreaUnit, cbArea\n")
        f.write(rows)
        f.close()
      }
    }
  }

  def aggregate(params: Seq[GlobalParamsInst]): Unit = {
    val archName = params.head.scArch.name
    val meaningfullConfigs = getWorstArchPerFCs(params.map(parseRoutingWidth(_)).flatten)

    printChanInfo(meaningfullConfigs, archName)
    printAreaInfo(meaningfullConfigs, archName, 1.6)
  }
}
