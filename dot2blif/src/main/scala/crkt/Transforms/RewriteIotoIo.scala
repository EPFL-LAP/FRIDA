package crkt

import arch._
import core.PatternRewriter
import archs.Entry
import archs.Exit
import archs.Primitive
import archs.Params
import archs.Operator
import archs.BlackBox
import archs.BlackBoxParams

case object RewriteIotoIo extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    val ioOnly = n.ports.map(_._2).flatten.forall {
      p =>
        {
          p.distPorts.map(_._2).forall {
            dp =>
              {
                g(dp.thisNode).nType.isIo || g(dp.thisNode).nType.isInstanceOf[BlackBox]
              }
          }
        }
    }

    n.nType match {
      case Entry(_) => n.nType.isIo && ioOnly
      case Exit(_)  => n.nType.isIo && ioOnly
      case BlackBox(p) => {
        n.ports
          .map(_._2)
          .flatten
          .exists(
            _.distPorts.exists(
              (_, dp) => g(dp.thisNode).nType.isIo
            )
          )
      }
      case others => false
    }
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    n.nType match {
      case BlackBox(p) => {
        val nPorts = n.ports
          .map(_._2)
          .flatten
          .map {
            p =>
              {
                val nDPs = p.distPorts.filter(
                  (_, dp) => !g(dp.thisNode).nType.isIo
                )

                if (nDPs.nonEmpty) {
                  Some(Port(p.id, p.name, p.attr, p.thisNode, nDPs, p.loc))
                } else {
                  None
                }
              }
          }
          .flatten
          .toList
          .groupBy(_.id)
          .map {
            (id, ps) =>
              {
                (
                  id,
                  ps.zipWithIndex.map(
                    (p, i) => Port(p.id, p.name, p.attr, p.thisNode, p.distPorts, i)
                  )
                )
              }
          }

        val nIns = nPorts
          .map(_._2)
          .flatten
          .filter(_.id.pt == PTInput)
          .map(_.id.width)
          .groupBy(
            w => w
          )
          .map(
            (k, v) => (k, v.size)
          )
          .toList

        val nOuts = nPorts
          .map(_._2)
          .flatten
          .filter(_.id.pt == PTOutput)
          .map(_.id.width)
          .groupBy(
            w => w
          )
          .map(
            (k, v) => (k, v.size)
          )
          .toList

        val nType = BlackBox(BlackBoxParams(nIns, nOuts, p.name, p.dontTouch))

        Node(n.name, nType, n.mlirAttr, n.attr, n.annos, nPorts) :: Nil
      }

      case other => {
        n.ports.map(_._2).flatten.foreach {
          p =>
            {
              p.distPorts.map(_._2).foreach {
                dp =>
                  {
                    dp.distPorts = dp.distPorts.filter(_._1 != p.nodeID())
                  }
              }
            }
        }

        Nil
      }
    }
  }
}
