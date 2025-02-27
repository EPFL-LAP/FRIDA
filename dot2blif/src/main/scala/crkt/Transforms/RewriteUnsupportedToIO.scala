package crkt

import arch._
import core.PatternRewriter
import archs.Entry
import archs.EntryParams
import archs.Exit
import archs.ExitParams
import archs.Primitive
import archs.Params
import archs.Operator
import archs.BlackBox

case class RewriteUnsupportedToIO(var entryCount: Int, var exitCount: Int) extends PatternRewriter {
  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    n.nType match {
      case BlackBox(p) => true && !p.dontTouch
      case other       => false
    }
  }

  def entry(n: TNode, p: Port): TNode = {
    assert(p.pt == PTOutput)

    val width = p.width
    val nPrim = Entry(EntryParams(width, Set(p.pmw.pb)))

    val nodeName = "in_" + entryCount
    entryCount = entryCount + 1

    val pmw = PortMeaningWrapper(PMData(None), p.pmw.pb)
    val outID = BlockPortID(width, PTOutput, pmw, p.dummy)
    val outPort = Port(outID, "", Map(), nodeName, p.distPorts, 0)

    p.distPorts.foreach {
      (id, dp: Port) =>
        {
          dp.distPorts = Map(outPort.nodeID() -> outPort)
        }
    }

    val nPorts = Map[BlockPortID, List[Port]](
      outID -> (outPort :: Nil)
    )

    Node(nodeName, nPrim, n.mlirAttr, n.attr, n.annos, nPorts)
  }

  def exit(n: TNode, p: Port): TNode = {
    assert(p.pt == PTInput)

    val width = p.width
    val nPrim = Exit(ExitParams(width, Set(p.pmw.pb)))

    val nodeName = "out_" + exitCount
    exitCount = exitCount + 1

    val pmw = PortMeaningWrapper(PMData(None), p.pmw.pb)
    val inID = BlockPortID(width, PTInput, pmw, p.dummy)
    val inPort = Port(inID, "", Map(), nodeName, p.distPorts, 0)

    p.distPorts.foreach {
      (id, dp: Port) =>
        {
          dp.distPorts = Map(inPort.nodeID() -> inPort)
        }
    }

    val nPorts = Map[BlockPortID, List[Port]](
      inID -> (inPort :: Nil)
    )

    Node(nodeName, nPrim, n.mlirAttr, n.attr, n.annos, nPorts)
  }

  // Edges between unhandled componets are also removed
  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    assert(pMatch(n, g))

    val nNodes = n.ports.values.flatten
      .map {
        (p: Port) =>
          {
            if (p.pt == PTOutput) {
              val handledDists = p.distPorts.filter(
                (id, dP) => !pMatch(g.nodes(dP.thisNode), g)
              )
              if (handledDists.isEmpty) {
                None
              } else {
                // println(p.toString() + " -> Input")
                p.distPorts = handledDists
                Some(entry(n, p))
              }

            } else {
              if (pMatch(g.nodes(p.distPorts.head._2.thisNode), g)) {
                None
              } else {
                // println(p.toString() + " -> Output")
                Some(exit(n, p))
              }

            }
          }
      }
      .filter(_.isDefined)
      .map(_.get)
      .toList

    CrktChecks.sanityCheck(nNodes)

    nNodes
  }
}
