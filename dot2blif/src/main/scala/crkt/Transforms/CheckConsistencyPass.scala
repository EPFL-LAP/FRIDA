package crkt

import core._
import arch.TBlock
import arch.PTOutput
import arch.BlockPortID

object CheckConsistencyPass extends IRPass[ElasticGraph] {
  def apply(graph: ElasticGraph, blocks: Map[String, TBlock]): ElasticGraph = {
    val nodes = graph.nodes

    val missmatchPorts = nodes
      .map(_._2)
      .map {
        case n @ Node(name, nType, _, attr, _, ports) => {
          n.inPorts()
            .map {
              (id: BlockPortID, ports: List[Port]) =>
                {
                  ports.map {
                    (p: Port) =>
                      {
                        if ((p.distPorts.size == 1) && (p.distPorts.head._2.width != p.width)) {
                          Some(p)
                        } else if (p.distPorts.size > 1) {
                          scala.sys.error("Check Consistency Pass must be called on implicit handshake dialect")
                          ???
                        } else {
                          None
                        }
                      }
                  }
                }
            }
            .flatten
        }
      }
      .flatten
      .filter(_.isDefined)
      .map(_.get)

    if (missmatchPorts.isEmpty) {
      println("No edge cross architecture grannularities!\n")
    } else {
      println("The missmatch edges are:")
      val edges = missmatchPorts.map {
        (p: Port) =>
          {
            if (p.pt == PTOutput) {
              p.thisNode + "." + p.id + "(" + p.id.pmw.pb + ")" + " -> "
                + p.distPorts.head._2.thisNode + "." + p.distPorts.head.toString()
            } else {
              val dp = p.distPorts.head._2
              dp.thisNode + "." + dp.id + "(" + dp.id.pmw.pb + ")" + " -> "
                + p.thisNode + "." + p.toString()
            }

          }
      }
      println(edges.mkString("\n"))
      println()
    }

    graph
  }
}
