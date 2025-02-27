package components

import archs.Params
import util.Util
import sys.process._

import java.io.FileWriter
import java.io.File
import java.io.FileOutputStream
import scala.io.{Source => IoSource}
import scala.util.matching.Regex

object Generator {
  // we do not synthesize Csts since they are wires only components and make the tools crash
  val components: List[ChiselComponent[_ <: Params]] = (
    ALU :: Icmp :: Branch :: CntrlMerge :: EB :: Fork :: Join :: ConfigurationBits :: MuxConfig :: Icmp
      :: Select :: Merge :: Multiplier :: Mux :: Sink :: Source :: TEHB :: OEHB :: Nil
  )

  def print(fName: String, data: String): Unit = {
    val writer: FileWriter = Util.writeOpen(fName)
    writer.write(data)
    writer.close()
  }

  def cleanLoclalDir(): Unit = {
    val cleanScript = "bash -c \"" +
      "rm *.json 2>/dev/null" +
      "\"" // 2>errs_place.log

    cleanScript.!
  }

  // TODO Did not find other way to remove the verilog file at the end.....
  def removeBlackBoxFileNames(verilogFile: String): String = {
    verilogFile.linesIterator
      .filter { line =>
        {
          val r = """Mux.+to1.v""".r
          r.findFirstMatchIn(line).isEmpty
        }
      }
      .mkString("\n")
  }
  def apply(verilogDir: String, constraintsDir: String): Unit = {
    def genComponent[P <: Params](c: ChiselComponent[P]): Unit = {
      c.wantedConfigurations.map { conf =>
        {
          val (verilog, moduleName) = c.emitVerilog(conf)
          val clocked = if (c.clocked) "_clk" else ""
          val module = verilogDir + "/" + moduleName + clocked + ".v"

          println(module)
          print(module, verilog)
        }
      }
    }

    VerilogMuxGenerator((2 until 64).toList)
    components.foreach(genComponent(_))
    cleanLoclalDir()
    removeBlackBoxFileNames(verilogDir)
  }
}
