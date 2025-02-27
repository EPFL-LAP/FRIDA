package routegen

import arch._
import util._
import frontend.GlobalParamsInst

import scala.io.Source
import java.io.FileWriter

object ToFixedLayout {
  def getFPGASize(placeFName: String): (Int, Int) = {
    val placedFile = Source.fromFile(placeFName).getLines().toList
    val line = placedFile
      .filter(
        (line: String) => line.contains("Array size")
      )
      .head

    val elems = line.split("\\s+")
    val index = elems.indexOf("x")
    (elems(index - 1).toInt, elems(index + 1).toInt)
  }

  def updateArchs(params: GlobalParamsInst, archs: List[Arch], height: Int, width: Int): Unit = {
    archs.foreach {
      (arch: Arch) =>
        {
          val archFile = params.buildDir + "/vpr/" + params.archPref + "_" + arch.name + ".xml"
          val archXML = Source.fromFile(archFile).getLines.toList

          val nXML = archXML.map {
            (line: String) =>
              {
                if (line.contains("auto_layout aspect_ratio")) {
                  val indent = line.indexOf("<")
                  val nLine = " ".repeat(indent) + "<fixed_layout name=\"fixed\" height=\"" + height
                    + "\" width=\"" + width + "\">"

                  nLine
                } else if (line.contains("/auto_layout")) {
                  line.replace("auto_layout", "fixed_layout")
                } else {
                  line
                }
              }
          }

          val writer: FileWriter = Util.writeOpen(archFile)
          writer.write(nXML.mkString("\n"))
          writer.close()
        }
    }
  }

  def apply(params: GlobalParamsInst, archs: List[Arch]): Unit = {
    val placeFName = params.buildDir + "/vpr/" + params.circuitPref + ".place"
    val (height, width) = getFPGASize(placeFName)

    updateArchs(params, archs, height, width)
  }
}
