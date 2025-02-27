package crkt

import arch._
import core.PatternRewriter
import archs.Branch
import archs.Fork
import archs.Mux
import archs.Select
import archs.Merge
import archs.Extsi
import archs.Trunc
import archs.Primitive
import archs.Params
import archs.IdentityPrimitive
import core.ACanIdentity
import archs.Constant
import archs.Extui
import archs.Source
import archs.Sink
import core.ADontTouch

case object RewriteIdentities extends PatternRewriter {
  def isCondLinkedToCst(n: TNode, g: ElasticGraph): Boolean = {
    val condPorts = n.ports.map(_._2).flatten
      .filter(_.id.pmw.pm.isInstanceOf[PMCond])
      .filter(_.id.pt == PTInput)
      .filter {
        _.id.pmw.pb match {
          case D => true
          case Impl => true
          case other => false
        }
      }

    if(condPorts.isEmpty) {
      false
    } else {
      assert(condPorts.size == 1)
      val condPort = condPorts.head

      g(condPort.distPorts.map(_._2).head.thisNode).nType.isInstanceOf[Constant]
    }
  }

  // TODO this should be moved to blocks on the Identity trait
  def isIdentity(n: TNode, g: ElasticGraph): Boolean = {
    lazy val isId = n.nType match {
      case Fork(p) => (p.num == 1)
      case Mux(p) => p.num == 1 // TODO double check this
      case Merge(p) => p.num == 1
      case Select(p) => p.num == 1
      case Branch(p) => isCondLinkedToCst(n, g)
      case Extsi(p) => p.inWidth == p.outWidth
      case Extui(p) => p.inWidth == p.outWidth
      case Trunc(p) => p.inWidth == p.outWidth
      case Constant(p) => n.name.contains("identity")
      case Source(p) => n.name.contains("identity")
      case Sink(p) => n.name.contains("identity")
      case other => false
    }

    !n.annos.contains(ADontTouch) && isId
  }

  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    isIdentity(n, g)
  }

  def rewriteRemoveCond(n: TNode): TNode = {
    val nPorts = n.ports.map(_._2).flatten.filter(!_.id.pmw.pm.isInstanceOf[PMCond]).toList

    Node(n.name, n.nType, n.mlirAttr, n.attr, n.annos, nPorts.groupBy(_.id))
  }

  def rewriteRemoveSink(n: TNode, g: ElasticGraph): TNode = {
    assert(n.nType.isInstanceOf[Branch])

    val nPorts = n.ports.map(_._2).flatten.filter {
      p => {
        !p.distPorts.map(_._2.thisNode).exists(g(_).nType.isInstanceOf[Sink])
      }
    }.toList

    Node(n.name, n.nType, n.mlirAttr, n.attr, n.annos, nPorts.groupBy(_.id))
  }

  def rewriteNoCond(n: TNode): Unit = {
    assert(n.inPorts().size == 1)
    assert(n.outPorts().size == 1)

    val inPort = n.inPorts().head._2.head
    val outPort = n.outPorts().head._2.head

    val succ = outPort.distPorts.head._2
    val pred = inPort.distPorts.head._2

    succ.distPorts = Map((pred.nodeID(), pred))
    pred.distPorts = pred.distPorts.map {
      (nId, p) =>
        {
          if (nId == inPort.nodeID()) {
            (succ.nodeID(), succ)
          } else {
            (nId, p)
          }
        }
    }
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    n.nType match {
      case Fork(p) => rewriteNoCond(n)
      case Mux(p) => rewriteNoCond(rewriteRemoveCond(n))
      case Merge(p) => rewriteNoCond(n)
      case Select(p) => rewriteNoCond(rewriteRemoveCond(n))
      case Branch(p) => rewriteNoCond(rewriteRemoveSink(rewriteRemoveCond(n), g))
      case Extsi(p) => rewriteNoCond(n)
      case Extui(p) => rewriteNoCond(n)
      case Trunc(p) => rewriteNoCond(n)

      // Nothing to rewrite since linked prim will be removed too
      case Constant(p) => Nil 
      case Source(p) => Nil
      case Sink(p) => Nil
      case other => scala.sys.error("Unexpected type.")
    }

    Nil
  }
}
