package crkt

import core.PatternRewriter
import archs.Branch
import archs.Mux
import arch.PTOutput
import arch.PMData
import arch.Hs
import arch.Impl
import arch.Vld
import arch.PTInput
import arch.PortType
import archs.SinkParams
import archs.Sink
import archs.Source
import archs.SourceParams
import arch.BlockPortID
import arch.PortMeaningWrapper
import arch.Regular
import mlir.AttrString

// Only work at Impl for now

object RewriteFixupIdentities extends PatternRewriter {
  def getPorts(n: TNode, dir: PortType): List[Port] = {
    n.ports.map(_._2).flatten
      .filter(_.pt == dir)
      .filter(_.pmw.pm.isInstanceOf[PMData])
      .filter {
        _.pmw.pb match {
          case Impl => true
          case others => scala.sys.error("Only works at Impl.")
        }
      }.toList
  }

  def getOutputPorts(n: TNode): List[Port] = {
    getPorts(n, PTOutput)
  }

  def getInputPorts(n: TNode): List[Port] = {
    getPorts(n, PTInput)
  }

  def pMatch(n: TNode, g: ElasticGraph): Boolean = {
    n.nType match {
      case Branch(p) => getOutputPorts(n).size == 1
      case Mux(p) => false && (getInputPorts(n).size == 1)
      case other => false
    }
  }

  def insertSink(n: TNode, p: Port): List[TNode] = {
    val sinkParams = SinkParams(p.id.width)
    val sinkPrim = Sink(sinkParams)

    val sinkName = n.name + "_sink"

    val sinkPID = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val sinkP = Port(sinkPID, "", Map(), sinkName, Map((p.nodeID() -> p)), 0)

    p.distPorts = Map((sinkP.nodeID() -> sinkP))

    val mlirAttrs = Map("handshake.name" -> AttrString(sinkName))

    val sink = Node(sinkName, sinkPrim, mlirAttrs, Map(), Set(), Map((sinkPID -> List(sinkP))))

    n :: sink :: Nil
  }

  def insertSource(n: TNode, p: Port): List[TNode] = {
    val sourceParams = SourceParams()
    val sourcePrim = Source(sourceParams)

    val sourceName = n.name + "_source"

    val sourcePID = BlockPortID(0, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val sourceP = Port(sourcePID, "", Map(), sourceName, Map((p.nodeID() -> p)), 0)

    p.distPorts = Map((sourceP.nodeID() -> sourceP))

    val mlirAttrs = Map("handshake.name" -> AttrString(sourceName))

    val source = Node(sourceName, sourcePrim, mlirAttrs, Map(), Set(), Map((sourcePID -> List(sourceP))))

    n :: source :: Nil
  }

  def rewrite(n: TNode, g: ElasticGraph): List[TNode] = {
    n.nType match {
      case Branch(p) => {
        val outPs = getOutputPorts(n)
        assert(outPs.size == 1)

        val outP0 = outPs.head
        val outP1 = Port(outP0.id, "", Map(), n.name, Map(), 1)

        val nPorts = (outP1 :: n.ports.map(_._2).flatten.toList).groupBy(_.id)
        val nNode = Node(n.name, n.nType, n.mlirAttr, n.attr, n.annos, nPorts)

        insertSink(nNode, outP1)
      }

      case Mux(p) => {
        val inPs = getInputPorts(n)
        assert(inPs.size == 1)

        val inP0 = inPs.head
        val inP1 = Port(inP0.id, "", Map(), n.name, Map(), 1)

        val nPorts = (inP1 :: n.ports.map(_._2).flatten.toList).groupBy(_.id)
        val nNode = Node(n.name, n.nType, n.mlirAttr, n.attr, n.annos, nPorts)

        // insertSink(nNode, inP1)
        n :: Nil
      }

      case other => scala.sys.error("Unexpected type.")
    }
  }
}

