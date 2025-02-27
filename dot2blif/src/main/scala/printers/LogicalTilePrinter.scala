package printers

import arch._
import util.Util
import frontend.GlobalParamsInst
import core.Namer
import io.AnsiColor._

object LogicalTilePrinter {
  def dfs(aMode: AbsMode, indent: String): String = {
    aMode match {
      case m: PrimMode => {
        val id = ("primID=" + m.name) :: Nil
        val annos = m.annotations.map(_.toString())
        val locConfig = m.locConfig.map(
          (lbp, loc) => lbp.toString() + " -> " + loc
        )
        val casts = m.castedPorts.map(
          (k, v) => "" + Namer(k) + " as " + Namer(v)
        )

        val attrsL = (id :: annos :: locConfig :: casts :: Nil).flatten.filter(_.nonEmpty)

        val attrs = if (attrsL.size > 2) {
          "\n" + indent + "    " + attrsL.mkString(",\n" + indent + "    ") + "\n" + indent
        } else {
          attrsL.mkString(", ")
        }

        m.name + " {" + attrs + "}"
      }

      case m: Mode if (m.name.contains("bypass")) => m.name

      case m: Mode => {
        val recIndent = indent + "    "
        val subNames = m.modes.map(dfs(_, recIndent))
        val subComb = m.modes.head.combinator
        val subStr = recIndent + subNames.mkString("\n" + recIndent + subComb + " ") + "\n"

        val id = "" :: Nil
        val annos = m.annotations.map(_.toString())
        val locConfig = m.locConfig
          .map(
            (lbp, loc) => lbp.toString() + " -> " + loc
          )
          .toList

        val attrsL = (id :: annos :: locConfig :: Nil).flatten.filter(_.nonEmpty)

        val attrs = if (attrsL.size > 2) {
          "\n" + indent + "    " + attrsL.mkString(",\n" + indent + "    ") + "\n" + indent
        } else {
          attrsL.mkString(", ")
        }

        m.name + " {" + attrs + "}" + " (\n" + subStr + indent + ")"
      }
    }

  }

  def apply(tile: Tile, params: GlobalParamsInst): Unit = {
    val fName = params.buildDir + "/asts/" + tile.name + ".cfg"
    println(YELLOW + "Printing: " + fName + RESET)

    val writer = Util.writeOpen(fName)

    val m = tile.asMode

    writer.write(dfs(m, ""))
    writer.write(" {\n")
    writer.write("    " + tile.vprConfig.toString())
    writer.write("\n}\n\n")

    writer.close()
  }
}
