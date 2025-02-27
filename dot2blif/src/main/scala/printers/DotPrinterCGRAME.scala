package printers

import arch.PTInput
import util.Util
import crkt._
import archs._

import io.AnsiColor._

object DotPrinterCGRAME {
  def printNodes(g: ElasticGraph): List[String] = {
    g.nodes.values.map {
      n => {
        val opcodeStr = {
          n.nType match {
            case Operator(p) => {
              assert(p.op == ALUOperation.add)
              "FADD"
            }

            case Mult(_) => "FMUL"
            case Div(_) => "FDIV"
            case Entry(_) => "input"
            case Exit(_) => "output"
            case other => scala.sys.error("Unsupported operation.")
          }
        }

        n.name + " [shape=record, opcode=" + opcodeStr + ", label=\"{" + n.name + "}\"];\n"
      }
    }.toList
  }

  def printEdges(g: ElasticGraph): List[String] = {
    g.nodes.values.map(_.ports.values.flatten).flatten.filter(_.id.pt == PTInput).map {
      p => {
        assert(p.distPorts.size == 1)

        val srcName = p.distPorts.head._2.thisNode
        val dstName = p.thisNode

        srcName + " -> " + dstName + "[operand=any2input];"
      }
    }.toList
  }

  def apply(fName: String, g: ElasticGraph): Unit = {
    val nodesStr = printNodes(g)
    val edgesStr = printEdges(g)

    val writer = Util.writeOpen(fName)
    println(YELLOW + "Printing: " + fName + RESET)

    writer.write("digraph G {\n")
    writer.write(nodesStr.mkString("\n"))

    writer.write("\n\n")

    writer.write(edgesStr.mkString("\n"))
    writer.write("\n}\n")

    writer.close()
  }
}

