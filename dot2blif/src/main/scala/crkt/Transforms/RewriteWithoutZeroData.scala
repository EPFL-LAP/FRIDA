package crkt

import arch._
import core.PatternRewriter

object RewriteWithoutZeroData extends PatternRewriter {
  def keepPort(p: Port): Boolean = {
    (p.id.pmw.pb == Impl) || (p.id.pmw.pb == Hs) || (p.id.width > 0)
  }

  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    n.ports.map(_._2).flatten.exists(!keepPort(_))
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    val nPorts = n.ports.map(_._2).flatten.filter(keepPort(_)).toList.groupBy(_.id)

    val nNode = Node(n.name, n.nType, n.mlirAttr, n.attr, n.annos, nPorts)

    nNode :: Nil
  }
}
