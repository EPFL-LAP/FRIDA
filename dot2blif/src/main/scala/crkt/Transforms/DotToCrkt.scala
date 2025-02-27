package crkt

import readers.GenericDotGraph
import archs._
import arch.PTInput
import arch.PTOutput
import arch.Impl
import readers.GenericDotEdge
import arch.BlockPortID
import arch.PortMeaningWrapper
import arch.PMData
import arch.Regular
import arch.PortType

import collection.mutable.{Map => MMap}

// Is only used for CGRA-ME kernels
// Interpret all FP operations as integer
// Assume all edges are 32-bit edges with associated implicit handshake signals

object DotToCrkt {
  def apply(dot: GenericDotGraph): ElasticGraph = {
    val inId = BlockPortID(32, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outId = BlockPortID(32, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val portAlloc = MMap[(String, PortType), Int]()

    // TODO probably only works if no one to many but that should be fine..
    val portMap = dot.edges.map {
      case GenericDotEdge(src, dst, attrs) => {
        val srcLoc = portAlloc.getOrElse((src.name, PTOutput), 0)

        // TODO for CGRA-ME we assume all ops have only one output... No forks yet.
        // TODO just implement a proper parsing at some point...
        // portAlloc((src.name, PTOutput)) = portAlloc.getOrElse((src.name, PTOutput), 0) + 1

        val dstLoc = portAlloc.getOrElse((dst.name, PTInput), 0)
        portAlloc((dst.name, PTInput)) = portAlloc.getOrElse((dst.name, PTInput), 0) + 1

        val srcP = Port(outId, "", Map(), src.name, Map(), srcLoc)
        val dstP = Port(inId, "", Map(), dst.name, Map(), dstLoc)

        srcP.distPorts = Map((dstP.nodeID() -> dstP))
        dstP.distPorts = Map((srcP.nodeID() -> srcP))

        ((src, dst) -> (srcP, dstP))
      }
    }.toMap

    val nodes = dot.nodes.map(_._2).map {
      gnode =>
        {
          val prim = gnode.attr("opcode").toLowerCase() match {
            case "fadd"   => Operator(OperatorParams(32, ALUOperation.add))
            case "fsub"   => Operator(OperatorParams(32, ALUOperation.sub))
            case "fmul"   => Mult(MultParams(32))
            case "fdiv"   => Div(DivParams(32))
            case "const"  => SrcConstant(ConstantParams(32))
            case "input"  => Entry(EntryParams(32, Set(Impl)))
            case "output" => Exit(ExitParams(32, Set(Impl)))
            case other    => scala.sys.error("Unsupported operation: " + other)
          }

          val locEdges = dot.edges.filter(
            e => (e.src.name == gnode.name) || (e.dst.name == gnode.name)
          )

          val ports = prim.instantiate(prim.p).blockInterface.ports.map {
            (id, bp) =>
              {
                val edgesForId = id.pt match {
                  case PTInput  => locEdges.filter(_.dst.name == gnode.name)
                  case PTOutput => locEdges.filter(_.src.name == gnode.name)
                  case other    => scala.sys.error("Expected defined port direction.")
                }

                val ps = if (bp.toPins().size == edgesForId.size) {
                  bp.toPins().zip(edgesForId).map {
                    (pin, e) =>
                      {
                        val (srcP, dstP) = portMap((e.src, e.dst))

                        pin.id.pt match {
                          case PTInput  => dstP
                          case PTOutput => srcP
                          case other    => scala.sys.error("Expected defined port direction.")
                        }
                      }
                  }
                } else if (bp.toPins().size == 1) {
                  assert(
                    edgesForId
                      .map(
                        e => portMap((e.src, e.dst))._1
                      )
                      .toSet
                      .size == 1
                  )

                  val srcP = edgesForId
                    .map(
                      e => portMap((e.src, e.dst))._1
                    )
                    .toSet
                    .head
                  val dps = edgesForId.map(
                    e => portMap((e.src, e.dst))._2
                  )

                  srcP.distPorts = dps
                    .map(
                      p => (p.nodeID() -> p)
                    )
                    .toMap

                  srcP :: Nil
                } else {
                  scala.sys.error("Unexpected connectivity.")
                }

                (id, ps)
              }
          }

          Node(gnode.name, prim, Map(), Map(), Set(), ports)
        }
    }

    ElasticGraph(nodes.toList)
  }
}
