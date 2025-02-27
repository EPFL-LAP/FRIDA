package crkt

import arch._
import core.PatternRewriter
import archs.Entry
import archs.Exit
import archs.Primitive
import archs.Params
import archs.Operator
import archs.BlackBox

import collection.mutable.{Map => MMap}
import archs.BlackBoxParams

case object RewriteOutputToEnd extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    n.nType match {
      case BlackBox(p) => {
        p.name.contains("end") && g.ios().filter(_.nType.isInstanceOf[Exit]).nonEmpty
      }

      case Exit(p) => true

      case other => false
    }
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    n.nType match {
      case BlackBox(p) => {

        val fromPorts = g.ios().filter(_.nType.isInstanceOf[Exit]).map {
          n =>
            {
              assert(n.ports.forall(_._1.pmw.pb == Impl))
              assert(n.ports.size == 1)
              assert(n.ports.head._2.size == 1)

              val outP = n.ports.head._2.head
              assert(outP.distPorts.size == 1)

              outP.distPorts.head._2
            }
        }

        val locs = MMap() ++= n.ports
          .map(
            (id, ps) => (id, ps.size)
          )
          .toMap

        val nPorts = fromPorts.map {
          p =>
            {
              val nP =
                Port(p.id.flipped(), "", Map(), n.name, Map((p.nodeID() -> p)), locs.getOrElse(p.id.flipped(), 0))
              p.distPorts = Map((nP.nodeID() -> nP))

              locs(nP.id) = locs.getOrElse(nP.id, 0) + 1

              nP
            }
        }

        val allPorts = (nPorts ++ n.ports.map(_._2).flatten).toList.groupBy(_.id)

        val nIns = allPorts
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

        val nOuts = allPorts
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

        Node(n.name, nType, n.mlirAttr, n.attr, n.annos, allPorts) :: Nil
      }

      case Exit(p) => Nil
      case other   => scala.sys.error("Unexpected type.")
    }
  }
}
