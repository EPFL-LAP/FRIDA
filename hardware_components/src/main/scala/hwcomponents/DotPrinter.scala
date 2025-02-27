package components

import arch.{Mux => AMux, Pin => APin, _}

import java.io.File
import java.io.FileWriter

// TODO maybe make moroe generic and work at an abstract graph level

object DotPrinter {
  def printTimingEntity(
      entity: TimingEntity
  )(implicit useIDs: Boolean): String = {
    entity match {
      case p @ Pin(name, port)  => printPin(p)
      case reg @ Register(name) => printReg(reg)
      case ResetPin             => printReset()
    }
  }

  def namePin(p: Pin)(implicit useIDs: Boolean): String = {
    val name = if (useIDs) p.port.toString() else p.name
    val nameSuf =
      if (useIDs) ""
      else
        p.port.hsType.fold("") {
          case HSValid => "_vld"; case HSReady => "_rdy"
        }
    val word = if (useIDs) "" else "_" + p.port.word.toString

    name + word + nameSuf
  }

  def printPin(p: Pin)(implicit useIDs: Boolean): String = {
    val pinName = namePin(p)

    "\"" + pinName + "\"" + "[shape=square, color=black, style=filled, fillcolor=aquamarine]"
  }

  def printReg(r: Register): String = {
    "\"" + r.name + "\"" + " [shape=oval, color=black, style=filled, fillcolor=coral]"
  }

  def printReset(): String = {
    "reset [shape=triangle, style=filled, fillcolor=cornsilk]"
  }

  def printEdge(e: TimingEdge)(implicit useIDs: Boolean): String = {
    def toName(entity: TimingEntity) = entity match {
      case p @ Pin(name, port) => {
        val pinName = namePin(p)
        if (useIDs) "\"" + port.toString() + "\"" else "\"" + pinName + "\""
      }
      case Register(name) => "\"" + name + "\""
      case ResetPin       => "reset"
    }

    val source = toName(e.source)
    val dest = toName(e.dest)

    val params = e.attrs
      .map { case (key: String, value: String) => key + "=" + value }
      .mkString(",")

    source + " -> " + dest + " [" + params + "]"
  }

  def apply(fname: String, useIds: Boolean): (TimingGraph) => Unit = {
    (g: TimingGraph) =>
      {

        println("printing timing graph... " + fname)

        implicit val useIDs = useIds

        val file = new File(fname)
        file.getParentFile().mkdirs()
        val writer = new FileWriter(file)

        writer.write("digraph G {\n")

        writer.write(
          g.nodes
            .map(n => "  " + printTimingEntity(n))
            .mkString(";\n".stripMargin)
        )
        writer.write(";\n")

        writer.write(g.edges.map(e => "  " + printEdge(e)).mkString(";\n"))

        writer.write("\n}\n")
        writer.close()
      }
  }
}
