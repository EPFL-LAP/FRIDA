package readers

import crkt.PortNodeID
import crkt.ElasticGraph
import arch.BlockPortID.PortInstanceParser
import arch.HSType
import arch.Tile

import frontend.GlobalParamsInst

import collection.mutable.{Map => MMap}
import collection.mutable.{Set => MSet}
import collection.mutable.ListBuffer

import io.Source
import arch.HSValid
import arch.HSReady
import archs.AutoLayout
import archs.FixedLayout
import archs.ControlConfig.height
import arch.D
import arch.Vld
import arch.Rdy

trait DelayType(val str: String)
object GeneralInt extends DelayType("GInt")
object LocalInt extends DelayType("LInt")
object ConfigInt extends DelayType("Conf")
object Compute extends DelayType("Comp")

trait PathType(val str: String)
object Control extends PathType("Control")
object Data extends PathType("Data")

trait Side {
  def distTo(oSide: Side): Int
}

trait PathDir(val str: String)
// Only valid and data
object Forward extends PathDir("Forward")
// Only ready
object Backward extends PathDir("Backwatd")
// (Valid or data) and ready
object Mixed extends PathDir("Mixed")

object Left extends Side {
  def distTo(oSide: Side): Int = oSide match {
    case Left   => 1
    case Top    => 2
    case Bottom => 2
    case Right  => 3
  }
}

object Right extends Side {
  def distTo(oSide: Side): Int = oSide match {
    case Left   => 3
    case Top    => 2
    case Bottom => 2
    case Right  => 1
  }
}

object Top extends Side {
  def distTo(oSide: Side): Int = oSide match {
    case Left   => 2
    case Top    => 1
    case Bottom => 3
    case Right  => 2
  }
}

object Bottom extends Side {
  def distTo(oSide: Side): Int = oSide match {
    case Left   => 2
    case Top    => 3
    case Bottom => 1
    case Right  => 2
  }
}

import arch.PortInstance

case class GridLoc(x: Int, y: Int, side: Option[Side])
case class EndPoint(value: PortNodeID, loc: GridLoc)

object PathInfoMut {
  def empty =
    PathInfoMut(0, 0, None, None, None, 0, 0, 0, Forward, ListBuffer(), ListBuffer(), ListBuffer(), MMap(), MMap())
}

// TODO also put a delay contribution here

case class PathInfoMut(
    var crit: Int,
    var delay: Double,
    var src: Option[EndPoint],
    var dst: Option[EndPoint],
    var pathType: Option[PathType],
    var tileHops: Int,
    var segments: Int,
    var primitives: Int,
    var pathDir: PathDir,
    var dist: ListBuffer[Int],
    var manhattanDist: ListBuffer[Int],
    var primsPerTile: ListBuffer[Int],
    delays: MMap[DelayType, Double],
    delaysFineGrained: MMap[String, Double]
) {
  def build: PathInfo = {
    PathInfo(
      crit,
      delay,
      src.get,
      dst.get,
      pathType.get,
      tileHops,
      segments,
      primitives,
      pathDir,
      dist.toList,
      manhattanDist.toList,
      primsPerTile.toList,
      delays.toMap,
      delaysFineGrained.toMap
    )
  }
}

case class PathInfo(
    crit: Int,
    delay: Double,
    src: EndPoint,
    dst: EndPoint,
    pathType: PathType,
    tileHops: Int,
    segments: Int,
    primitives: Int,
    pathDir: PathDir,
    dist: List[Int],
    manhattanDist: List[Int],
    primsPerTile: List[Int],
    delays: Map[DelayType, Double],
    delaysFineGrained: Map[String, Double]
) {
  lazy val manhattanMean = ???
}

case class PlaceRessourceUsage(
    unitTileLocations: Int,
    locs: List[PlaceLocInfo],
    archInfo: Map[Tile, Int],
    netlistInfo: Map[Tile, Int]
)
case class PlacementCostInfo(cp: Double, placementCost: Double, placementBBCost: Double, placementTDCost: Double)

case class PlacerInfo(costInfo: PlacementCostInfo, utilInfo: PlaceRessourceUsage)

case class TimingInfo(pathInfos: List[PathInfo], placeInfo: PlacerInfo) {
  lazy val criticalPath = pathInfos.filter(_.crit == 0).head
}

case class PlaceLocInfo(blockName: String, x: Int, y: Int, xOffset: Int, yOffset: Int)

object VprTimingReportReader {
  def getPath(line: String): Int = {
    line.replace("#Path ", "").toInt
  }

  def getPrimName(line: String): Option[String] = {
    if (line.contains("Dummy") || line.contains("clk") || line.contains("identity")) {
      None
    } else {
      Some(line.split('.')(0).trim())
    }
  }

  def getEndPoint(line: String): EndPoint = {
    val cleaned = line.replace("Startpoint: ", "").replace("Endpoint  : ", "")

    val pStr = cleaned.split("""\s+""")(0)

    val isIo = pStr.contains("inpad") || pStr.contains("outpad")

    val (nodeName, pi) = if (isIo) {
      val piStr = pStr.split("__")(1).split('.')(0)

      (pStr.split("__")(0), PortInstanceParser(piStr))
    } else {
      (pStr.split('.')(0), PortInstanceParser(pStr.split('.')(1).split('[')(0)))
    }

    val value = PortNodeID(nodeName, pi.id, pi.loc)

    val gridLocStr = cleaned.split("""\s+""")(3)
    val xLoc = gridLocStr.replace("(", "").replace(")", "").trim().split(",")(0).toInt
    val yLoc = gridLocStr.replace("(", "").replace(")", "").trim().split(",")(1).toInt

    EndPoint(value, GridLoc(xLoc, yLoc, None))
  }

  def getSide(str: String): Side = {
    str match {
      case "RIGHT"  => Right
      case "LEFT"   => Left
      case "BOTTOM" => Bottom
      case "TOP"    => Top
      case other    => scala.sys.error("Unexpected side:" + str)
    }
  }

  def getManhattan(src: GridLoc, dst: GridLoc): Int = {
    val man = math.abs(src.x - dst.x) + math.abs(src.y - dst.y)
    val sideOffset = if (man == 0) {
      src.side.get.distTo(dst.side.get)
    } else {
      if (src.side == dst.side) {
        src.side.get.distTo(dst.side.get) + 1
      } else {
        src.side.get.distTo(dst.side.get)
      }
    }

    man + sideOffset
  }

  def getManhattanNoSides(src: GridLoc, dst: GridLoc): Int = {
    math.abs(src.x - dst.x) + math.abs(src.y - dst.y)
  }

  def readReport(rpt: String): List[PathInfo] = {
    var currPath = PathInfoMut.empty
    var inPath = false
    var inEpilogue = false
    var inGeneralInt = false
    var dist = 0
    var hops = 0
    var prevLineContainsIdentity = false
    var portsFound = MSet[Option[HSType]]()

    val prims = MSet[String]()

    var srcInterLoc = GridLoc(0, 0, None)
    var dstInterLoc = GridLoc(0, 0, None)

    val timings = ListBuffer[PathInfo]()

    Source.fromFile(rpt).getLines().foreach {
      line =>
        {
          if (line.contains("#Path")) {
            currPath.crit = getPath(line) - 1
          } else if (line.contains("Startpoint")) {
            val src = getEndPoint(line)

            currPath.src = Some(src)
            currPath.pathType = src.value.pId.pmw.pb match {
              case D     => Some(Data)
              case Vld   => Some(Control)
              case Rdy   => Some(Control)
              case other => scala.sys.error("Expected lowered id.")
            }
          } else if (line.contains("Endpoint")) {
            currPath.dst = Some(getEndPoint(line))
          } else if (line.contains("---------") && !inPath) {
            inPath = true
          } else if (line.contains("OPIN")) {
            val lineSize = line.split("""\s+""").size
            val locStr = line.split("""\s+""")(lineSize - 3).replace(")", "").replace("(", "")
            val sideStr = line.split("""\s+""")(lineSize - 4).replace(")", "").replace("(", "").replace(",", "")

            val xLoc = locStr.split(",")(0).toInt
            val yLoc = locStr.split(",")(1).toInt

            srcInterLoc = GridLoc(xLoc, yLoc, Some(getSide(sideStr)))
          } else if (line.contains("IPIN")) {
            val lineSize = line.split("""\s+""").size
            val locStr = line.split("""\s+""")(lineSize - 3).replace(")", "").replace("(", "")
            val sideStr = line.split("""\s+""")(lineSize - 4).replace(")", "").replace("(", "").replace(",", "")

            val xLoc = locStr.split(",")(0).toInt
            val yLoc = locStr.split(",")(1).toInt

            dstInterLoc = GridLoc(xLoc, yLoc, Some(getSide(sideStr)))

            // Append the intermediate path to the list of paths
            currPath.dist.append(dist)
            dist = 0

            currPath.manhattanDist.append(getManhattanNoSides(srcInterLoc, dstInterLoc))

            hops += 1

            currPath.primsPerTile.append(prims.size)
            prims.clear()
          } else if (line.contains("cell setup time")) {
            val lineSize = line.split("""\s+""").size
            val incr = -line.split("""\s+""")(lineSize - 1).toDouble

            currPath.delays(Compute) = currPath.delays.getOrElse(Compute, 0.0) + incr
            currPath.delaysFineGrained("Setup") = currPath.delaysFineGrained.getOrElse("Setup", 0.0) + incr
          } else if (line.contains("data arrival time")) {
            inEpilogue = true
          } else if (line.contains("slack")) {
            val lineSize = line.split("""\s+""").size
            val slack = math.abs(line.split("""\s+""")(lineSize - 1).toDouble)

            currPath.delay = slack

            currPath.tileHops = hops
            hops = 0

            val pathDir = if (portsFound.contains(Some(HSReady)) && portsFound.size == 1) {
              Backward
            } else if ((portsFound.size == 3) || ((portsFound.size == 2) && portsFound.contains(Some(HSReady)))) {
              Mixed
            } else {
              Forward
            }

            currPath.pathDir = pathDir
            portsFound.clear()

            // reset tracking variables
            inPath = false
            inEpilogue = false
            inGeneralInt = false
            dist = 0
            hops = 0
            srcInterLoc = GridLoc(0, 0, None)
            dstInterLoc = GridLoc(0, 0, None)

            // Remember path
            timings.append(currPath.build)

            currPath = PathInfoMut.empty
          }

          if (!inEpilogue && inPath && !line.contains("--------")) {
            // Count primitives per tiles
            if (!line.contains("|")) {
              getPrimName(line).foreach(
                primName => prims += primName
              )
            }

            // Update timng information
            val lineSize = line.split("""\s+""").size
            val incr = line.split("""\s+""")(lineSize - 2).toDouble

            if (incr != 0) {
              val (dt, primName) = if (line.contains("primitive")) {
                inGeneralInt = false

                if (!line.contains("Dummy") && !prevLineContainsIdentity) {
                  currPath.primitives += 1
                }

                val primNamePattern = "\'[0-9a-zA-Z-_ ]+\'".r
                val primName = primNamePattern.findFirstMatchIn(line) match {
                  case Some(s) => s.toString().replace("\'", "")
                  case None    => scala.sys.error("Did not parse primitive name in VPR timing report.")
                }

                (Compute, primName)
              } else if (line.contains("CHAN")) {
                inGeneralInt = true

                val length = line.split("length:")(1).split("""\s+""")(0).toInt
                dist += length
                currPath.segments += length

                (GeneralInt, GeneralInt.str + "_L" + length)
              } else {
                assert(line.contains("intra"))

                if (inGeneralInt) {
                  inGeneralInt = false
                  (LocalInt, LocalInt.str)
                } else {
                  inGeneralInt = false
                  (ConfigInt, ConfigInt.str)
                }
              }

              currPath.delays(dt) = currPath.delays.getOrElse(dt, 0.0) + incr
              currPath.delaysFineGrained(primName) = currPath.delaysFineGrained.getOrElse(primName, 0.0) + incr
            }
          }

          if (inPath && !line.contains('|') && line.contains("at")) {
            val isValid = line.contains("vld")
            val isReady = line.contains("rdy")
            val isData = !isValid && !isReady

            if (isValid) {
              portsFound += Some(HSValid)
            }

            if (isReady) {
              portsFound += Some(HSReady)
            }

            if (isData) {
              portsFound += None
            }
          }

          if (line.contains("identity")) {
            prevLineContainsIdentity = true
          } else if (prevLineContainsIdentity) {
            prevLineContainsIdentity = false
          }
        }
    }

    timings.toList
  }

  // Assumes the IO is on the perimeter of the tile
  def readPlacementFile(placeFileLog: String, params: GlobalParamsInst): List[PlaceLocInfo] = {
    var startParse = false

    val (maxX, maxY) = params.scArch.layout match {
      case AutoLayout => ???
      case FixedLayout(width, height) => {
        (width, height)
      }
    }

    val locs = Source
      .fromFile(placeFileLog)
      .getLines()
      .map {
        line =>
          {
            if (startParse) {
              val blockName = line.split("""\s+""")(0)
              val x = line.split("""\s+""")(1).toInt
              val y = line.split("""\s+""")(2).toInt
              val subx = line.split("""\s+""")(3).toInt
              val suby = line.split("""\s+""")(4).toInt

              Some(PlaceLocInfo(blockName, x, y, subx, suby))
            } else {
              if (line.contains("#----------")) {
                startParse = true
              }

              None
            }
          }
      }
      .flatten
      .filter { // TODO this should be handled by respective functions...
        case PlaceLocInfo(blockName, x, y, subx, suby) => {
          !blockName.contains("Clk") && !(x == 0) && !(y == 0) && !(x == (maxX - 1)) && !(y == (maxY - 1))
        }
      }
      .toList

    locs
  }

  def readPlacementRessourceUsage(
      params: GlobalParamsInst,
      placeLog: String,
      placeFile: String
  ): PlaceRessourceUsage = {
    var inRessource = false
    var wasNetlist = false
    var wasArch = false

    // TODO Why start from Tile here?
    val archInfo = MMap[Tile, Int]()
    val circuitInfo = MMap[Tile, Int]()

    val tiles = params.scArch.tiles
      .map(_())
      .map(
        t => (t.name, t)
      )
      .toMap

    Source.fromFile(placeLog).getLines().foreach {
      line =>
        {
          if (line.contains("Resource usage...")) {
            inRessource = true
          } else if (inRessource && line.contains("Netlist")) {
            wasNetlist = true
          } else if (inRessource && line.contains("Architecture")) {
            wasArch = true
          } else if (inRessource && wasNetlist && line.contains("blocks of type:")) {
            val tName = line.split("blocks of type:")(1).trim()
            val t = tiles(tName)
            val num = line.split("blocks of type:")(0).trim().toInt

            circuitInfo += ((t, num))

            wasNetlist = false
          } else if (inRessource && wasArch && line.contains("blocks of type:")) {
            val tName = line.split("blocks of type:")(1).trim()
            val t = tiles(tName)
            val num = line.split("blocks of type:")(0).trim().toInt

            archInfo += ((t, num))

            wasArch = false
          } else if (line.contains("Device Utilization:")) {
            inRessource = false
          }
        }
    }

    val locs = readPlacementFile(placeFile, params)

    PlaceRessourceUsage(locs.size, locs, archInfo.toMap, circuitInfo.toMap)
  }

  def readPlace(placeLog: String): PlacementCostInfo = {
    var cp: Double = 0
    var placementCost: Double = 0
    var placementBBCost: Double = 0
    var placementTDCost: Double = 0

    Source.fromFile(placeLog).getLines().foreach {
      line =>
        {
          if (line.contains("Placement estimated critical path delay (least slack):")) {
            cp = line.replace("Placement estimated critical path delay (least slack):", "").split("ns")(0).toDouble
          } else if (line.contains("Placement cost:")) {
            val lSplit = line.split(',')
            placementCost = lSplit(0).replace("Placement cost:", "").toDouble
            placementBBCost = lSplit(1).replace("bb_cost:", "").toDouble
            val td = lSplit(2).replace("td_cost:", "")
            placementTDCost = if (td.contains("nan")) {
              -1
            } else {
              td.toDouble
            }
          }
        }
    }

    PlacementCostInfo(cp, placementCost, placementBBCost, placementTDCost)
  }

  def apply(params: GlobalParamsInst, timingRpt: String, placeLog: String, placeFile: String): TimingInfo = {
    val timings = readReport(timingRpt)
    val placeCostInfo = readPlace(placeLog)
    val placeUtilInfo = readPlacementRessourceUsage(params, placeLog, placeFile)

    val placeInfo = PlacerInfo(placeCostInfo, placeUtilInfo)

    TimingInfo(timings, placeInfo)
  }
}
