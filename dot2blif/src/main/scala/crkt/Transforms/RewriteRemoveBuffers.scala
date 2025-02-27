package crkt

import arch._
import archs.Params
import archs.EB
import archs.TEHB
import archs.OEHB
import core.PatternRewriter

object RewriteRemoveBuffers extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    n.nType.isBuf
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    val inPorts = n.ports.filter(_._1.pt == PTInput)
    val outPorts = n.ports.filter(_._1.pt == PTOutput)

    assert(inPorts.size == 1)
    assert(outPorts.size == 1)

    val predP = inPorts.values.head.head.distPorts.head._2
    val succP = outPorts.values.head.head.distPorts.head._2

    predP.distPorts = outPorts.head._2.head.distPorts
    succP.distPorts = inPorts.head._2.head.distPorts

    Nil
  }
}
