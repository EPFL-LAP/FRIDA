package core

import crkt._
import arch._
import io.AnsiColor._

trait PatternRewriter {
  def apply(g: ElasticGraph): ElasticGraph = {
    var applied = false

    val nNodes = g.nodes
      .map(_._2)
      .toList
      .sortBy(_.name)
      .map {
        node =>
          {
            if (pMatch(node, g)) {
              applied = true
              rewrite(node, g)
            } else {
              node :: Nil
            }
          }
      }
      .flatten
      .map(n => (n.name, n))
      .toMap

    val nG = ElasticGraph(nNodes, g.properties)

    if (applied) {
      apply(nG)
    } else {
      nG
    }
  }

  def pMatch(n: TNode, g: ElasticGraph): Boolean
  def rewrite(n: TNode, g: ElasticGraph): List[TNode]
}

object PatternRewriterPass {
  def apply(rewriters: List[PatternRewriter], ir: ElasticGraph): ElasticGraph = {
    if (rewriters.isEmpty) {
      ir
    } else {
      println(CYAN + rewriters.head.getClass.getName + RESET)
      apply(rewriters.tail, rewriters.head(ir))
    }
  }
}
