package crkt

import arch._

object LowerToExplicitVldRdy {
  def convertId(id: BlockPortID): (BlockPortID, BlockPortID) = {
    assert(id.pmw.pb == Hs, id.pmw.pb)

    val vId = BlockPortID(id.width, id.pt, PortMeaningWrapper(id.pmw.pm, Vld), id.dummy)
    val rId = BlockPortID(id.width, id.pt.flipped, PortMeaningWrapper(id.pmw.pm, Rdy), id.dummy)

    (vId, rId)
  }

  def rewriteEdges(ports: List[Port]): List[Port] = {
    ports.map {
      p =>
        {
          p.pmw.pb match {
            case D => p :: Nil
            case Hs => {
              val (vId, rId) = convertId(p.id)

              val vPort = Port(vId, "", Map(), p.thisNode, Map(), p.loc)
              val rPort = Port(rId, "", Map(), p.thisNode, Map(), p.loc)

              val (vDPs, rDPs) = p.distPorts
                .map(_._2)
                .map {
                  dp =>
                    {
                      assert(dp.id.pmw.pb == Hs)

                      val (vDId, rDId) = convertId(dp.id)

                      val vDP = Port(vDId, "", Map(), dp.thisNode, Map((vPort.nodeID() -> vPort)), dp.loc)
                      val rDP = Port(rDId, "", Map(), dp.thisNode, Map((rPort.nodeID() -> rPort)), dp.loc)

                      (vDP, rDP)
                    }
                }
                .unzip

              vPort.distPorts = vDPs
                .map(
                  dp => (dp.nodeID() -> dp)
                )
                .toMap
              rPort.distPorts = rDPs
                .map(
                  dp => (dp.nodeID() -> dp)
                )
                .toMap

              vPort :: rPort :: (vDPs.toList ++ rDPs.toList)
            }

            case other => scala.sys.error("Unexpected PortBundle.")
          }
        }
    }.flatten
  }

  def apply(g: ElasticGraph): ElasticGraph = {
    val outPorts = g.nodes
      .map(_._2)
      .map(_.ports.map(_._2).flatten)
      .flatten
      .filter {
        p =>
          {
            (p.id.pmw.pb == D) || (p.id.pt == PTOutput)
          }
      }
      .toList

    val nEdges = rewriteEdges(outPorts)

    val nNodes = nEdges
      .groupBy(_.thisNode)
      .map {
        (nodeName, ports) =>
          {
            val node = g.nodes(nodeName)

            Node(node.name, node.nType, node.mlirAttr, node.attr, Set(), ports.groupBy(_.id))
          }
      }
      .map(
        n => (n.name, n)
      )
      .toMap

    ElasticGraph(nNodes)
  }
}
