package crkt

import arch._
import archs.Params
import archs.Exit
import core.PatternRewriter
import collection.mutable.{Set => MSet}

// VPR does not accept nets that go to multiple IOs, so we remove those....

object RewriteFixupExit extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    n.nType match {
      case Exit(p) => n.ports.map(_._2).flatten.exists(_.distPorts.isEmpty)
      case other   => false
    }
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    val nPorts = n.ports.map(_._2).flatten.filter(_.distPorts.nonEmpty).toList.groupBy(_.id)

    val nType = n.nType match {
      case Exit(p) => n.ports.map(_._2).flatten.exists(_.distPorts.isEmpty)
      case other   => ???
    }

    Node(n.name, n.nType, n.mlirAttr, n.attr, n.annos, nPorts) :: Nil
  }
}
