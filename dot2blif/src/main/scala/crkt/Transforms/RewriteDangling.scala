package crkt

import arch._
import core.PatternRewriter

object RewriteDangling extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    n.ports.values.flatten.forall(_.distPorts.isEmpty)
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    Nil
  }
}
