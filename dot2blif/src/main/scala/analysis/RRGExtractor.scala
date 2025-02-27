package analysis

import arch.Arch
import arch.PbType
import frontend.GlobalParamsInst
import archs.Architectures
import io.Source
import io.AnsiColor._
import util.XMLParser
import java.io.File

import math.ceil

case class Grid(width: Int, height: Int)

case class Location(xlow: Int, xhigh: Int, ylow: Int, yhigh: Int)

sealed trait NType(val str: String)
case object IPin extends NType("IPIN")
case object OPin extends NType("OPIN")
case object CHANX extends NType("CHANX")
case object CHANY extends NType("CHANY")

sealed trait Side(val str: String)
case object Top extends Side("TOP")
case object Bottom extends Side("BOTTOM")
case object Left extends Side("LEFT")
case object Right extends Side("RIGHT")

sealed trait Direction(val str: String)
case object Inc extends Direction("INC_DIR")
case object Dec extends Direction("DEC_DIR")

sealed trait RRNode {
  def id: Int
  def t: NType
  def loc: Location
}

case class RRPin(id: Int, t: NType, loc: Location, side: Side) extends RRNode
case class RRChan(id: Int, t: NType, loc: Location, dir: Direction) extends RRNode

case class RREdge(srcId: Int, sinkId: Int, switchId: Int)

case class RRGraph(nodes: Set[RRNode], edges: Set[RREdge], grid: Grid, switches: Map[String, Int]) {
  lazy val edgesFromSource = edges.groupBy(_.srcId)
  lazy val edgesFromSink = edges.groupBy(_.sinkId)
}

type WireLength = Int

case class RRGConInfo(
    // Routing multiplexers number of inputs
    rMuxIns: Map[WireLength, Int],

    // Connection block multiplexers number of imputs
    cMuxIns: Int, // TODO should depend on the tile later

    // Out Degree of the wires in the RRG only counting routing muxs
    chanAvOutLoad: Map[WireLength, Int],

    // Out Degree of the wires in the RRG
    chanAvOutDegree: Map[WireLength, Int],
    routingNum: Map[WireLength, Int]
) {
  override def toString(): String = {
    "rMux input size:\n"
      + rMuxIns
        .map(
          (wl, s) => "  L" + wl + ": " + s
        )
        .mkString("\n")
      + "\ncMux input size: " + cMuxIns
      + "\naverage programmed multiplexer out degree:\n"
      + chanAvOutLoad
        .map(
          (wl, s) => "  L" + wl + ": " + s
        )
        .mkString("\n")
      + "\naverage out degree:\n"
      + chanAvOutDegree
        .map(
          (wl, s) => "  L" + wl + ": " + s
        )
        .mkString("\n")
      + "\nrouting muxs per tile:\n"
      + routingNum
        .map(
          (wl, s) => "  L" + wl + ": " + s
        )
        .mkString("\n")
  }
}

object RRGConnectivityExtractor {
  def readNodes(rrg: xml.Elem): Set[RRNode] = {
    (rrg \ "rr_nodes").head.child
      .filter(
        n => (n \@ "id").nonEmpty
      )
      .filter {
        n =>
          {
            ((n \@ "type") contains "PIN") || ((n \@ "type") contains "CHAN")
          }
      }
      .map {
        node =>
          {
            val id = (node \@ "id")
            val nTypeStr = (node \@ "type")

            val nType = nTypeStr match {
              case IPin.str  => IPin
              case OPin.str  => OPin
              case CHANX.str => CHANX
              case CHANY.str => CHANY
            }

            val locStr = (node \ "loc").head
            val xlow = (locStr \@ "xlow")
            val xhigh = (locStr \@ "xhigh")
            val ylow = (locStr \@ "ylow")
            val yhigh = (locStr \@ "yhigh")

            val loc = Location(xlow.toInt, xhigh.toInt, ylow.toInt, yhigh.toInt)

            if (nTypeStr contains "CHAN") {
              val dir = (node \@ "direction")

              val direction = dir match {
                case Inc.str => Inc
                case Dec.str => Dec
              }

              RRChan(id.toInt, nType, loc, direction)
            } else {
              val sideStr = (locStr \@ "side")

              val side = sideStr match {
                case Top.str    => Top
                case Bottom.str => Bottom
                case Left.str   => Left
                case Right.str  => Right
                case other      => Right // TODO fixme
              }

              RRPin(id.toInt, nType, loc, side)
            }
          }
      }
      .toSet
  }

  def readEdges(rrg: xml.Elem): Set[RREdge] = {
    (rrg \ "rr_edges").head.child
      .filter(
        n => (n \@ "sink_node").nonEmpty
      )
      .map {
        node =>
          {
            val srcId = (node \@ "src_node").toInt
            val sinkId = (node \@ "sink_node").toInt
            val switchId = (node \@ "switch_id").toInt

            RREdge(srcId, sinkId, switchId)
          }
      }
      .toSet
  }

  // For now we assume only one type of tile and compute the size of the grid only here
  // TODO In the future, might want to compute mux sizes per tiles
  def readGrid(rrg: xml.Elem): Grid = {
    val (xs, ys) = (rrg \ "grid").head.child
      .filter(
        n => (n \@ "block_type_id").nonEmpty
      )
      .map {
        node =>
          {
            val x = (node \@ "x").toInt
            val y = (node \@ "y").toInt

            (x, y)
          }
      }
      .unzip

    Grid(xs.max, ys.max)
  }

  def readMuxs(rrg: xml.Elem): Map[String, Int] = {
    (rrg \ "switches").head.child
      .filter(
        n => (n \@ "id").nonEmpty
      )
      .map {
        node =>
          {
            val id = (node \@ "id").toInt
            val name = (node \@ "name")

            (name, id.toInt)
          }
      }
      .toMap
  }

  def readRRG(rrg: xml.Elem): RRGraph = {
    RRGraph(readNodes(rrg), readEdges(rrg), readGrid(rrg), readMuxs(rrg))
  }

  def parseRoute(params: GlobalParamsInst) = ???

  def channelsAt(rrGraph: RRGraph, loc: Location): Set[RRChan] = {
    rrGraph.nodes
      .collect {
        case c: RRChan => c
      }
      .filter {
        case RRChan(_, _, cloc, _) => cloc == loc
      }
  }

  // All channels not neighbors to IO tiles
  def allCentralChannels(rrgraph: RRGraph, l: Int): Set[RRChan] = {
    val minX = 1
    val minY = 1
    val maxX = rrgraph.grid.width - (1 + (l - 1))
    val maxY = rrgraph.grid.height - (1 + (l - 1))

    val chans = (minX until maxX)
      .map {
        x =>
          {
            (minY until maxY).map {
              y =>
                {
                  val horizontalLoc = Location(x, x + (l - 1), y, y)
                  val verticalLoc = Location(x, x, y, y + (l - 1))

                  channelsAt(rrgraph, horizontalLoc)
                    ++ channelsAt(rrgraph, verticalLoc)
                }
            }.flatten
          }
      }
      .flatten
      .toSet

    chans
  }

  def average(l: Seq[Int]): Int = {
    ceil(l.sum / l.size.toDouble).toInt
  }

  def inDegree(rrgraph: RRGraph, node: RRNode): Int = {
    rrgraph.edgesFromSink.get(node.id).fold(0)(_.size)
  }

  def outDegreeWithSwitches(rrgraph: RRGraph, node: RRNode, muxIds: Set[Int]): Int = {
    rrgraph.edgesFromSource.get(node.id).fold(0)(_.filter(muxIds contains _.switchId).size)
  }

  def outDegree(rrgraph: RRGraph, node: RRNode): Int = {
    rrgraph.edgesFromSource.get(node.id).fold(0)(_.size)
  }

  def getRoutingMuxInputs(rrgraph: RRGraph, arch: Arch, params: GlobalParamsInst): Map[WireLength, Int] = {
    val minX = 1
    val minY = 1
    val maxX = rrgraph.grid.width - 2
    val maxY = rrgraph.grid.height - 2

    params.scArch.switchLengths.map {
      l =>
        {
          val chanSizes = allCentralChannels(rrgraph, l).toList.map(inDegree(rrgraph, _))

          (l -> average(chanSizes))
        }
    }.toMap
  }

  def pinsAt(rrgraph: RRGraph, loc: Location, nt: NType): Set[RRPin] = {
    rrgraph.nodes
      .collect {
        case p: RRPin => p
      }
      .filter {
        case RRPin(_, t, ploc, _) => (ploc == loc) && (nt == t)
      }
  }

  def getCBMuxInputs(rrgraph: RRGraph, arch: Arch, params: GlobalParamsInst): Int = {
    val minX = 1
    val minY = 1
    val maxX = rrgraph.grid.width - 1
    val maxY = rrgraph.grid.height - 1

    val pinInDegrees = (minX until maxX).map {
      x =>
        {
          (minY until maxY).map {
            y =>
              {
                val loc = Location(x, x, y, y)
                val pins = pinsAt(rrgraph, loc, IPin).toList

                pins.map(inDegree(rrgraph, _))
              }
          }.flatten
        }
    }.flatten

    average(pinInDegrees)
  }

  def averageChanOutDegree(rrgraph: RRGraph, params: GlobalParamsInst): Map[WireLength, Int] = {
    val minX = 1
    val minY = 1
    val maxX = rrgraph.grid.width - 2
    val maxY = rrgraph.grid.height - 2

    params.scArch.switchLengths.map {
      l =>
        {
          val chanSizes = allCentralChannels(rrgraph, l).toList.map(outDegree(rrgraph, _))

          (l -> average(chanSizes))
        }
    }.toMap
  }

  // Needs to be run as a statistics on all benchmarks
  // we store it in build/analysis/routing_load_stats.log
  // If the file is not there, we put the total load
  def averageChanOutLoad(rrgraph: RRGraph, params: GlobalParamsInst): Map[WireLength, Int] = {
    val loadStatsFName = params.analysisDir + "/routing_load_stats.csv"

    if (File(loadStatsFName).exists()) {
      ???
    } else {
      // We do not have the stats, take the worst case estimate
      averageChanOutDegree(rrgraph, params)
    }
  }

  def print(info: Map[Arch, RRGConInfo]): Unit = {
    info.map {
      (arch, i) =>
        {
          println
          println(arch.name + ":")
          println(i.toString())
        }
    }
  }

  def startingAt(params: GlobalParamsInst, rrg: RRGraph): Map[WireLength, Int] = {
    params.scArch.switchLengths.map {
      l =>
        {
          val routingNum = average(allCentralChannels(rrg, l).groupBy(_.loc).map(_._2.size).toSeq)

          (l -> routingNum)
        }
    }.toMap
  }

  def apply(params: GlobalParamsInst, archs: List[Arch]): Map[Arch, RRGConInfo] = {
    println(CYAN + "Extracting RRG connectivity info..." + RESET)

    val res = archs
      .filter(!_.place)
      .map {
        arch =>
          {
            val rrgFName = "rrg_" + arch.name + ".xml"
            val rrgXml = XMLParser.parseFile(params, rrgFName)
            val rrg = readRRG(rrgXml)

            val rMuxIns = getRoutingMuxInputs(rrg, arch, params)
            val cMuxIns = getCBMuxInputs(rrg, arch, params)
            val chanAvOutDegree = averageChanOutDegree(rrg, params)
            val chanAvRouteOutDegree = averageChanOutLoad(rrg, params)
            val startingPoints = startingAt(params, rrg)

            arch -> RRGConInfo(rMuxIns, cMuxIns, chanAvRouteOutDegree, chanAvOutDegree, startingPoints)
          }
      }
      .toMap

    // println(res.map((k,v) => (k.name + ":\n" + v)).mkString("\n"))

    res
  }
}
