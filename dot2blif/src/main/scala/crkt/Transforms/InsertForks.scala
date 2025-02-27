package crkt

import arch._
import archs.Params
import archs.SrcConstant
import core.PatternRewriter
import archs.ForkParams
import archs.EagerFork
import archs.Fork

import collection.mutable.{Map => MMap}

// Since CGRA-ME has implicit forks in the interconnect, make them implicit since we require it

case class RewriteInsertForks(var num: Int = 0) extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    assert(n.ports.forall(_._1.pmw.pb == Impl))

    n.ports.filter(_._1.pt == PTOutput).exists(_._2.exists(_.distPorts.size > 1))
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    val outPortForks = n.ports.filter(_._1.pt == PTOutput).map(_._2).flatten.filter(_.distPorts.size > 1)

    n :: outPortForks.map {
      p => {
        val dps = p.distPorts

        val fName = "fork" + num
        num = num + 1

        val inP = Port(p.id.flipped(), "", Map(), fName, Map((p.nodeID() -> p)), 0)
        p.distPorts = Map((inP.nodeID() -> inP))

        val outPs = dps.map(_._2).map {
          dp => {
            val fOP = Port(dp.id.flipped(), "", Map(), fName, Map((dp.nodeID() -> dp)), 0)
            dp.distPorts = Map((fOP.nodeID() -> fOP))

            fOP
          }
        }.toList

        val fPorts = (inP :: outPs).groupBy(_.id)

        val fp = ForkParams(p.id.width, dps.size, EagerFork)

        Node(fName, Fork(fp), Map(), Map(), Set(), fPorts)
      }
    }.toList
  }
}


