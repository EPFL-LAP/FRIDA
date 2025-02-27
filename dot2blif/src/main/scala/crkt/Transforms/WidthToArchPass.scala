package crkt

import arch._
import core._

import scala.collection.mutable.HashSet

// NOTE: treat if multiple ports with different data width of the same meaning need to be extended
// NOTE: then they can all be mapped to the same width, and treated uniformly
// NOTE: may want to throw a warning

object WidthToArchPass extends IRPass[ElasticGraph] {
  val encoutered = HashSet[(Int, Int)]()

  def findClosestWidth(p: Port, b: TBlock): Int = {
    def error = scala.sys.error("Cannot width extend port " + p + " to the architecture")
    // b.findClosestBlockPort(p).fold(error)(_.width)

    ???
  }

  def mapPorts(ports: List[Port], equBlock: TBlock): List[Port] = {
    ports.map {
      case p @ Port(BlockPortID(width, pt, pmw, dummy), name, attr, thisNode, distPorts, loc) => {
        val nWidth = findClosestWidth(p, equBlock)
        if (nWidth == width) {
          p
        } else {
          val nId = BlockPortID(nWidth, pt, pmw, dummy)
          val nP = Port(nId, name, attr, thisNode, distPorts, loc)
          assert(nWidth >= width)

          if (distPorts.nonEmpty) {
            Port.updateDistPorts(p, nP)
            // distPort.get.distPorts = Some(nP)
          }

          if ((nWidth != width) && !encoutered.contains((nWidth, width))) {
            // println("" + width + " -> " + nWidth)
            encoutered += ((nWidth, width))
          }

          nP
        }
      }
    }
  }

  // TODO this should get the Elastic graph and the Archs list
  def apply(graph: ElasticGraph, blocks: Map[String, TBlock]): ElasticGraph = {
    ???
  }

  def verifier(g: ElasticGraph, blocks: Map[String, TBlock]): Unit = {
    ???
  }
}
