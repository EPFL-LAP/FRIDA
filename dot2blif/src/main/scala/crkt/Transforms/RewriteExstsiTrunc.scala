package crkt

import core.PatternRewriter
import archs._
import arch.Arch
import arch.Impl
import arch.BlockPortID
import arch.PMCond
import math.max

// Only there to ensure width operations are removed since we do not allow these yet...

object RewriteExstsiTrunc extends PatternRewriter {
  import RewriteWidthUtils._

  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    assert(n.ports.map(_._2).flatten.forall(_.id.pmw.pb == Impl))

    n.nType match {
      case Extsi(p) => p.inWidth != p.outWidth
      case Trunc(p) => p.inWidth != p.outWidth
      case other    => false
    }
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

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    n.nType match {
      case Extsi(p) => {
        if (p.inWidth == p.outWidth) {
          n :: Nil
        } else {
          val nWidth = max(p.inWidth, p.outWidth)
          val nPorts = mapPorts(n.ports.map(_._2).flatten.toList, nWidth).groupBy(_.id)

          Node(n.name, Extsi(ExtsiParams(nWidth, nWidth)), n.mlirAttr, n.attr, n.annos, nPorts) :: Nil
        }
      }

      case Extui(p) => {
        if (p.inWidth == p.outWidth) {
          n :: Nil
        } else {
          val nWidth = max(p.inWidth, p.outWidth)
          val nPorts = mapPorts(n.ports.map(_._2).flatten.toList, nWidth).groupBy(_.id)

          Node(n.name, Extui(ExtuiParams(nWidth, nWidth)), n.mlirAttr, n.attr, n.annos, nPorts) :: Nil
        }
      }

      case Trunc(p) => {
        if (p.inWidth == p.outWidth) {
          n :: Nil
        } else {
          val nWidth = max(p.inWidth, p.outWidth)
          val nPorts = mapPorts(n.ports.map(_._2).flatten.toList, nWidth).groupBy(_.id)

          Node(n.name, Extsi(ExtsiParams(nWidth, nWidth)), n.mlirAttr, n.attr, n.annos, nPorts) :: Nil
        }
      }

      case other => ???
    }
  }
}
