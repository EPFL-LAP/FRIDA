package crkt

import core.PatternRewriter
import archs._
import arch.Arch
import arch.Impl
import arch.BlockPortID
import arch.PMCond
import math.max

object RewriteInconsistentWidth extends PatternRewriter {
  import RewriteWidthUtils._

  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    assert(n.ports.map(_._2).flatten.forall(_.id.pmw.pb == Impl))

    n.ports
      .map(_._2)
      .flatten
      .exists(
        p => p.distPorts.exists(_._2.id.width > p.width)
      )
  }

  def findNewWidth(n: TNode): Option[Int] = {
    n.ports
      .map(_._2)
      .flatten
      .map(
        p =>
          p.distPorts
            .map(_._2)
            .map(
              dp => (p.width, dp.width)
            )
      )
      .flatten
      .filter(_ != _)
      .map(
        (w0, w1) => max(w0, w1).toInt
      )
      .reduceOption(max)
  }

  def updateNode(n: TNode, width: Int, instPrim: (Int => Primitive)): TNode = {
    val nWidth = findNewWidth(n)

    if (nWidth.isEmpty || (nWidth.get == width)) {
      n
    } else {
      val nPorts = n.nType match {
        case Constant(p) => {
          val outPorts = mapPorts(n.outPortsList(), nWidth.get)
          val inPorts = n.inPortsList()

          (inPorts ++ outPorts).groupBy(_.id)
        }

        case other => mapPorts(n.ports.map(_._2).flatten.toList, nWidth.get).groupBy(_.id)
      }
      val nPrim = instPrim(nWidth.get)

      Node(n.name, nPrim, RewriteWidthUtils.updateAttrs(n.mlirAttr, nWidth.get), n.attr, n.annos, nPorts)
    }
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    mapNode(n, updateNode) :: Nil
  }
}
