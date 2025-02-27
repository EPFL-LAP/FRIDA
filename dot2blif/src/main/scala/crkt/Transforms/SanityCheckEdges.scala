package crkt

import core._
import arch.TBlock
import arch.PortType
import arch.PTInput
import arch.PTOutput
// TODO Probably implement some sanitycheck passes here

object SanityCheckEdgesPass extends IRPass[ElasticGraph] {
  def checkPort(port: Port): Unit = {
    if (port.distPorts.nonEmpty) {
      if ((port.distPorts.size > 1) && (port.pt == PTInput)) {
        scala.sys.error(
          "Input ports can only have one incoming edge: (" + port.distPorts.mkString(", ") + ") -> " + port.toString()
        )
      }

      if (!port.distPorts.forall(_._2.distPorts.contains(port.nodeID()))) {
        if (port.pt == PTInput) {
          scala.sys.error("Inconsistent edge: (" + port.distPorts.mkString(", ") + ") -> " + port.toString())
        } else {
          scala.sys.error("Inconsistent edge: " + port.toString() + " -> (" + port.distPorts.mkString(", ") + ")")
        }
      }
    }
  }

  def apply(graph: ElasticGraph, blocks: Map[String, TBlock]): ElasticGraph = {
    val nodes = graph.nodes.map(_._2)
    nodes.foreach(_.ports.values.flatten.foreach(checkPort))

    graph
  }
}
