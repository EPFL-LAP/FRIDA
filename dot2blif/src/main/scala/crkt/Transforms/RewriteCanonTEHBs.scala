package crkt

import arch._
import archs.Params
import archs.OEHB
import archs.TEHBParams
import archs.OEHBParams
import archs.TEHB
import archs.Operator
import core.PatternRewriter

import collection.mutable.{Map => MMap}

// Temporary fix to make sure all benchmark pass.....
// Sometimes, the buffer placement does not respect the constraints...

object RewriteCanonTEHBs extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    lazy val linkedBuf = n.ports
      .map(_._2)
      .flatten
      .exists(
        p =>
          p.distPorts
            .map(_._2)
            .exists(
              dp => g(dp.thisNode).nType.isBuf
            )
      )

    n.nType match {
      case TEHB(p) => (p.depth > 1) && !linkedBuf
      case other   => false
    }
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    val oehbName = n.name + "_o"
    val slots = 1
    val oehbP = OEHBParams(n.nType.asInstanceOf[TEHB].p.width, slots)

    val oehbPorts = n.ports
      .map(_._2)
      .flatten
      .filter(_.id.pt == PTInput)
      .map {
        p =>
          {
            assert(p.id.pt == PTInput)
            val outOP = Port(p.id.flipped(), "", Map(), oehbName, Map((p.nodeID() -> p)), p.loc)
            val inOP = Port(p.id, "", Map(), oehbName, p.distPorts, p.loc)

            p.distPorts
              .map(_._2)
              .foreach(
                dp => dp.distPorts = Map((inOP.nodeID() -> inOP))
              )
            p.distPorts = Map((outOP.nodeID() -> outOP))

            outOP :: inOP :: Nil
          }
      }
      .flatten

    val oehb = Node(oehbName, OEHB(oehbP), n.mlirAttr, n.attr, n.annos, oehbPorts.toList.groupBy(_.id))

    val nWidth = n.nType.asInstanceOf[TEHB].p.width
    val nSlots = n.nType.asInstanceOf[TEHB].p.depth - 1

    val tehb = Node(n.name, TEHB(TEHBParams(nWidth, nSlots)), n.mlirAttr, n.attr, n.annos, n.ports)

    tehb :: oehb :: Nil
  }
}
