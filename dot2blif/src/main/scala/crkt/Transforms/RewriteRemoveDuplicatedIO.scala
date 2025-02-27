package crkt

import arch._
import archs.Params
import archs.Exit
import core.PatternRewriter
import collection.mutable.{Set => MSet}

// VPR does not accept nets that go to multiple IOs, so we remove those....

object RewriteRemoveDuplicatedIO extends PatternRewriter {
  def toMultipleIos(g: ElasticGraph, n: TNode): Boolean = {
    n.ports.map(_._2).flatten.exists {
      p =>
        {
          p.distPorts
            .map(_._2)
            .filter(
              dp => g(dp.thisNode).nType.isIo
            )
            .size > 1
        }
    }
  }

  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    toMultipleIos(g, n)
  }

  def removeExtraIoLinks(g: ElasticGraph, n: TNode): TNode = {
    val nPorts = n.ports
      .map(_._2)
      .flatten
      .map {
        p =>
          {
            val (ioDPs, nonIo) = p.distPorts
              .map(_._2)
              .toList
              .partition(
                dp => g(dp.thisNode).nType.isIo
              )

            if (ioDPs.nonEmpty) {
              val keptIO = ioDPs.head
              val droppedIOS = ioDPs.tail

              p.distPorts = (keptIO :: nonIo)
                .map(
                  dp => (dp.nodeID() -> dp)
                )
                .toMap

              droppedIOS.foreach {
                dp =>
                  {
                    dp.distPorts = Map()
                  }
              }
            }

            p
          }
      }
      .toList
      .groupBy(_.id)

    Node(n.name, n.nType, n.mlirAttr, n.attr, n.annos, nPorts)
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    removeExtraIoLinks(g, n) :: Nil
  }
}
