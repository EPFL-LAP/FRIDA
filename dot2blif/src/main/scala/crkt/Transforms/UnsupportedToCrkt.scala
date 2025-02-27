package crkt

import core.PatternRewriter

object UnsupportedToCrkt extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    ???
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    ???
  }
}

