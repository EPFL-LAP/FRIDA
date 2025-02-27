package crkt

import arch._
import archs.Params
import archs.OEHB
import archs.TEHBParams
import archs.OEHBParams
import archs.TEHB
import archs.Operator
import core.PatternRewriter
import mlir.BufConfigAttr
import mlir.AttrString

import collection.mutable.{Map => MMap}

object RewriteCanonOEHBs extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    n.nType match {
      case OEHB(p) => p.depth > 1
      case other   => false
    }
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    val tehbName = n.name + "_t"
    val slots = n.nType.asInstanceOf[OEHB].p.depth - 1
    val tehbP = TEHBParams(n.nType.asInstanceOf[OEHB].p.width, slots)

    val mlirName = "handshake.name" -> AttrString(tehbName)
    val mlirParams = "hw.parameters" -> BufConfigAttr(slots, false, false, true)
    val mlirAttrs = (mlirName :: mlirParams :: Nil).toMap

    val tehbOuts = n.ports.filter(_._1.pt == PTOutput).map(_._2).map(_.map(_.withNodeName(tehbName))).flatten
    val tehbIns = tehbOuts.map {
      p =>
        {
          Port(p.id.flipped(), p.id.flipped().toString(), Map(), tehbName, Map(), p.loc)
        }
    }

    val tehbPorts = (tehbIns ++ tehbOuts).toList.groupBy(_.id)
    val tehb = Node(tehbName, TEHB(tehbP), mlirAttrs, n.attr, n.annos, tehbPorts)

    n.ports.filter(_._1.pt == PTOutput).map(_._2).foreach {
      ps =>
        {
          ps.foreach {
            p =>
              {
                val tp = tehbIns.filter(_.id == p.id.flipped()).filter(_.loc == p.loc).head
                p.distPorts = Map((tp.nodeID() -> tp))
                tp.distPorts = Map((p.nodeID() -> p))
              }
          }
        }
    }

    val oehbName = ("handshake.name" -> AttrString(n.name))
    val oehbParams = ("hw.parameters" -> BufConfigAttr(1, true, true, false))
    val oehbAttrs = (oehbName :: oehbParams :: Nil).toMap

    val oehbP = OEHBParams(n.nType.asInstanceOf[OEHB].p.width, 1)
    val oehb = Node(n.name, OEHB(oehbP), oehbAttrs, n.attr, n.annos, n.ports)

    tehb :: oehb :: Nil
  }
}
