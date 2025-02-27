package crkt

import arch._

object LowerToExplicitHandshakeCrkt {
  def convertId(id: BlockPortID): (BlockPortID, BlockPortID) = {
    val dId = BlockPortID(id.width, id.pt, PortMeaningWrapper(id.pmw.pm, D), id.dummy)
    val hsId = BlockPortID(id.width, id.pt, PortMeaningWrapper(id.pmw.pm, Hs), id.dummy)

    (dId, hsId)
  }

  def rewriteEdges(ports: List[Port]): List[Port] = {
    ports.map {
      p =>
        {
          assert(p.id.pmw.pb == Impl)

          val (pdId, phsId) = convertId(p.id)

          val dataPort = Port(pdId, "", Map(), p.thisNode, Map(), p.loc)
          val hsPort = Port(phsId, "", Map(), p.thisNode, Map(), p.loc)

          val (dataDPs, hsDPs) = p.distPorts
            .map(_._2)
            .map {
              dp =>
                {
                  assert(dp.id.pmw.pb == Impl)

                  val (dId, hsId) = convertId(dp.id)

                  val dataDP = Port(dId, "", Map(), dp.thisNode, Map((dataPort.nodeID() -> dataPort)), dp.loc)
                  val hsDP = Port(hsId, "", Map(), dp.thisNode, Map((hsPort.nodeID() -> hsPort)), dp.loc)

                  (dataDP, hsDP)
                }
            }
            .unzip

          dataPort.distPorts = dataDPs
            .map(
              dp => (dp.nodeID() -> dp)
            )
            .toMap
          hsPort.distPorts = hsDPs
            .map(
              dp => (dp.nodeID() -> dp)
            )
            .toMap

          dataPort :: hsPort :: (dataDPs.toList ++ hsDPs.toList)
        }
    }.flatten
  }

  def apply(g: ElasticGraph): ElasticGraph = {
    val outPorts = g.nodes.map(_._2).map(_.ports.map(_._2).flatten).flatten.filter(_.id.pt == PTOutput).toList
    val nEdges = rewriteEdges(outPorts)

    val nNodes = nEdges
      .groupBy(_.thisNode)
      .map {
        (nodeName, ports) =>
          {
            val node = g.nodes(nodeName)

            Node(node.name, node.nType, node.mlirAttr, node.attr, node.annos, ports.groupBy(_.id))
          }
      }
      .map(
        n => (n.name, n)
      )
      .toMap

    ElasticGraph(nNodes)
  }
}
