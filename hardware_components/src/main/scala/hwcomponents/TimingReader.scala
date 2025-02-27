package components

import arch.{Mux => AMux, Pin => APin, _}
import archs.Params

import util._
import scala.io
import java.io.File
import math.max
import math.floor
import math.rint
import math.ceil

object TimingParser {
  val translator = Map[String, ChiselComponent[_ <: Params]](
    "Operator" -> ALU,
    "Branch" -> Branch,
    "Merge" -> Merge,
    "CntrlMerge" -> CntrlMerge,
    "EB" -> EB,
    "Fork" -> Fork,
    "Join" -> Join,
    "CMux" -> MuxConfig,
    "Mux" -> Mux,
    "OEHB" -> OEHB,
    "Sink" -> Sink,
    "Source" -> Source,
    "TEHB" -> TEHB,
    "Cst" -> Cst,
    "Mult" -> Multiplier,
    "ConfBits" -> ConfigurationBits,
    "Comparator" -> Icmp,
    "Select" -> Select
  )

  val widthToColor = Map[(Int, Option[HSType]), String](
    (0, None) -> "gold",
    (0, Some(HSValid)) -> "salmon2",
    (0, Some(HSReady)) -> "blue",
    (1, None) -> "forestgreen",
    (1, Some(HSValid)) -> "salmon2",
    (1, Some(HSReady)) -> "blue",
    (32, None) -> "mediumpurple1",
    (32, Some(HSValid)) -> "salmon2",
    (32, Some(HSReady)) -> "blue"
  )

  def compName(moduleName: String): String = {
    val candidates = translator.map(_._1).filter(moduleName.startsWith(_))
    assert(
      candidates.size == 1,
      "Did not find supported component for: " + moduleName
    )

    candidates.head
  }

  def parseTiming(
      moduleName: String,
      it: Iterator[String]
  ): Option[TimingEdge] = {
    var parsed = false
    var beginPoint = ""
    var endPoint = ""
    var setup: Option[Double] = None
    var clockToQ: Option[Double] = None
    var delay: Double = 0
    var clock: Double = 0
    var requiredTime: Double = 0
    var slack: Double = 0
    var arrivalTime: Double = 0

    val rawModule = compName(moduleName)

    while (!parsed) {
      if (!it.hasNext) {
        return None
      }

      val line = it.next()
      if (line contains "Beginpoint") {
        beginPoint = line.split("\\s+")(1)
      } else if (line contains "Endpoint") {
        endPoint = line.split("\\s+")(1)
      } else if (line contains "- Setup") {
        setup = Some(line.split("\\s+")(2).toDouble)
      } else if (line contains "+ Phase Shift") {
        clock = line.split("\\s+")(3).toDouble
      } else if (line contains "= Required Time") {
        requiredTime = line.split("\\s+")(3).toDouble
      } else if (line contains "= Slack Time") {
        slack = line.split("\\s+")(3).toDouble
        parsed = true
      } else if (line contains "- Arrival Time") {
        delay = line.split("\\s+")(3).toDouble
      }
    }

    parsed = false
    var inPath = false
    while (!parsed) {
      val line = it.next()
      if (line contains "|") {
        inPath = true
      } else {
        if (inPath == true) {
          parsed = true
        }

        inPath = false
      }

      val tableFields = line.replaceAll("\\s+", "").split("""\|""")
      if (
        inPath && (tableFields.size == 7) && (tableFields(
          1
        ) != "Instance") && (tableFields(1) != "")
      ) {
        if (tableFields(2).replace("^", "").replace("v", "") == "CK->Q") {
          clockToQ = Some(tableFields(4).toDouble)
        }
      }
    }

    delay = ceil3(clockToQ.fold(delay)(delay - _))
    val d = Delay(clock, requiredTime, slack, delay, setup, clockToQ)
    val source = translator(rawModule).translate(moduleName, beginPoint)
    val dest = translator(rawModule).translate(moduleName, endPoint)

    Some(TimingEdge(source, dest, d, Map()))
  }

  def truncate3(d: Double): Double = floor(d * 1000) / 1000
  def round3(d: Double): Double = rint(d * 1000) / 1000
  def ceil3(d: Double): Double = ceil(d * 1000) / 1000

  def removeRedundantEdges(g: TimingGraphBuilder): TimingGraph = {
    val nEdges = g.edges
      .map {
        case (source: TimingEntity, edgeList: List[TimingEdge]) => {
          val nEdges = edgeList.groupBy(_.dest).map {
            case (dest: TimingEntity, l: List[TimingEdge]) =>
              dest match {
                case Pin(_, port) => {
                  val minSlackDelay = l
                    .reduce((e0, e1) =>
                      if (e0.delay.slack > e1.delay.slack) e1 else e0
                    )
                    .delay

                  TimingEdge(
                    source,
                    dest,
                    minSlackDelay,
                    Map("color" -> widthToColor(port.id.width, port.hsType))
                  )
                }

                case Register(name) => {
                  val minSlackDelay = l
                    .reduce((e0, e1) =>
                      if (e0.delay.slack > e1.delay.slack) e1 else e0
                    )
                    .delay

                  val attr = if (source.isInstanceOf[Pin]) {
                    val port = source.asInstanceOf[Pin].port
                    Map("color" -> widthToColor(port.id.width, port.hsType))
                  } else {
                    Map[String, String]()
                  }

                  TimingEdge(source, dest, minSlackDelay, attr)
                }

                case ResetPin => scala.sys.error("Reset pin encountered")
              }
          }

          nEdges.toSet
        }
      }
      .reduceOption(_ ++ _)
      .getOrElse(Set())

    TimingGraph(g.nodes, nEdges, Map())
  }

  def removeConfiguration(g: TimingGraph): TimingGraph = {
    val nNodes = g.nodes.filter {
      case Pin(_, port) => !port.id.pmw.pm.isInstanceOf[PMConf]
      case other        => true
    }

    val nEdges = g.edges.filter {
      case TimingEdge(source, dest, _, _) => {
        val sourceOK = source match {
          case Pin(_, port) => !port.id.pmw.pm.isInstanceOf[PMConf]
          case other        => true
        }

        val destOK = dest match {
          case Pin(_, port) => !port.id.pmw.pm.isInstanceOf[PMConf]
          case other        => true
        }

        sourceOK && destOK
      }
    }

    TimingGraph(nNodes, nEdges, g.attrs)
  }

  def buildTimingGraph(moduleName: String, fname: String): TimingGraph = {
    val it = io.Source.fromFile(fname).getLines()

    def rec(
        it: Iterator[String],
        nodes: Set[TimingEntity],
        edges: Map[TimingEntity, List[TimingEdge]]
    ): TimingGraphBuilder = {
      if (!it.hasNext) {
        TimingGraphBuilder(nodes, edges)
      } else {
        val optEdge = parseTiming(moduleName, it)
        val nEdge =
          if (optEdge.isEmpty) return TimingGraphBuilder(nodes, edges)
          else optEdge.get

        val nNodes = nodes + nEdge.source + nEdge.dest
        val nEdges = if (edges.keySet contains nEdge.source) {
          val nL = nEdge :: edges(nEdge.source)
          edges.updated(nEdge.source, nL)
        } else {
          edges.updated(nEdge.source, (nEdge :: Nil))
        }

        rec(it, nNodes, nEdges)
      }
    }

    val redundant = rec(it, Set(), Map())
    removeConfiguration(removeRedundantEdges(redundant))
  }

  def apply(moduleName: String, fname: String): TimingGraph = {
    buildTimingGraph(moduleName, fname)
  }
}
