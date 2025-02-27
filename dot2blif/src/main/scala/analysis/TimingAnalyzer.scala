package analysis

import crkt._
import arch._

import math.pow

case class EdgeInfo(src: Port, dst: Port)

object TimingAnalyzer {
  def parseTimingString(tStr: String): Double = {
    val delay = tStr.split("e-")(0).toDouble
    val unit = tStr.split("e-")(1).toInt

    delay * pow(10, -unit)
  }

  def getTimingsFrom(block: TBlock, pi: PortInstance): List[Timing] = {
    block.physicalInfo.timings.filter {
      case CombTiming(src, dst, _, _) => (src == pi) || (dst == pi)
      case RegTiming(loc, _, _, _, _) => loc == pi
    }
  }

  def getTimingsFromNodePort(node: TNode, port: Port)(implicit blocks: Map[String, TBlock]): List[Timing] = {
    // val equBlock = node.equivalentBlock(blocks)

    // val portInstances = port.pb match {
    //   case Impl => {
    //     List(PortInstance(port.id, port.loc, None),
    //          PortInstance(port.id, port.loc, Some(HSValid)),
    //          PortInstance(port.id, port.loc, Some(HSReady)))
    //   }
    //   case D => PortInstance(port.id, port.loc, None) :: Nil
    //   case Hs => {
    //     PortInstance(port.id, port.loc, Some(HSValid)) :: PortInstance(port.id, port.loc, Some(HSReady)) :: Nil
    //   }
    // }

    // portInstances.map(getTimingsFrom(equBlock, _)).flatten
    ???
  }

  def exploreFromEdge(edge: EdgeInfo, g: ElasticGraph, blocks: Map[String, TBlock]): Double = {
    val srcNode = g.nodes(edge.src.thisNode)
    val dstNode = g.nodes(edge.dst.thisNode)

    ???
  }

  def getWorseTimingArc(edge: EdgeInfo, g: ElasticGraph, blocks: Map[String, TBlock]): Double = {
    ???
  }
}
