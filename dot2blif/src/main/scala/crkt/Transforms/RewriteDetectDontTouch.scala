package crkt

import core.PatternRewriter
import archs.ForkParams
import archs.Fork
import arch.PTInput
import archs.LazyFork
import core.ADontTouch
import archs.TEHB
import archs.OEHB
import archs.EB

// If an eager fork is of size 1 after a lazy fork, keep it to prevent combinational loops

case object RewriteDetectDontTouch extends PatternRewriter {
  def fromLazyFork(n: TNode, g: ElasticGraph): Boolean = {
    n.ports.map(_._2)
      .flatten
      .filter(_.id.pt == PTInput)
      .map(_.distPorts.map(_._2))
      .flatten
      .exists {
        dp => {
          g(dp.thisNode).nType match {
            case Fork(p) => (p.variant == LazyFork)
            case TEHB(_) => fromLazyFork(g(dp.thisNode), g)
            case OEHB(_) => fromLazyFork(g(dp.thisNode), g)
            case EB(_) => fromLazyFork(g(dp.thisNode), g)
            case others => false
          }
        }
      }
  }

  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    lazy val addDontTouch = n.nType match {
      case Fork(p) => (p.num == 1) && fromLazyFork(n, g)
      case others => false
    }

    !n.annos.contains(ADontTouch) && addDontTouch
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    // println(n.name)

    Node(n.name, n.nType, n.mlirAttr, n.attr, n.annos + ADontTouch, n.ports) :: Nil
  }
}


