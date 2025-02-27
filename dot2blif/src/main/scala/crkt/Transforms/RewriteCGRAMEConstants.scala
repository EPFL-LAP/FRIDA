package crkt

import arch._
import archs.Params
import archs.SrcConstant
import core.PatternRewriter

import collection.mutable.{Map => MMap}

object RewriteCGRAMEConstants extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    n.nType match {
      case SrcConstant(p) => {
        assert(n.ports.forall(_._1.pmw.pb == Impl) && (n.ports.size == 1) && (n.ports.head._2.size == 1))

        n.ports.head._2.head.distPorts.size > 1
      }
      case other => false
    }
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    val p = n.ports.head._2.head

    p.distPorts
      .map(_._2)
      .zipWithIndex
      .map {
        (dp, i) =>
          {
            val nOut = Port(p.id, p.name, p.attr, n.name + "_" + i, Map((dp.nodeID() -> dp)), p.loc)
            val nPorts = Map((nOut.id -> List(nOut)))
            val nCst = Node(n.name + "_" + i, n.nType, n.mlirAttr, n.attr, n.annos, nPorts)

            dp.distPorts = Map((nOut.nodeID() -> nOut))

            nCst
          }
      }
      .toList
  }
}
