package rrggen

import frontend.GlobalParamsInst
import arch.Arch
import arch.RootPb
import arch.TBlock
import io.AnsiColor._
import util.XMLParser
import analysis.GridInfo

case class RRGInfo(rrg: xml.Elem, arch: Arch)

object RRGUnifier {
  val RRG = "rrg_all.xml"

  def apply(
      params: GlobalParamsInst,
      archs: List[Arch],
      tiles: Map[String, RootPb],
      gridInfo: Option[GridInfo]
  ): Unit = {
    val widthArchs = archs.filter(!_.place)
    val rrgsFNames = widthArchs.map(
      arch => "rrg_" + arch.name + ".xml"
    )

    println(CYAN + "Loading routing resource graphs..." + RESET)
    val rrgsXML = rrgsFNames.map {
      fileName =>
        {
          val xml = XMLParser.parseFile(params, fileName)
          val archName = fileName.replace("rrg_", "").replace(".xml", "")
          val arch = archs.filter(_.name == archName).head

          RRGInfo(xml, arch)
        }
    }

    println(CYAN + "Merging routing resource graphs..." + RESET)
    val rrg = MergeRRG(tiles, rrgsXML)

    println(CYAN + "Legalize architecture for unique RRG..." + RESET)
    ArchitectureLegalizer(archs, tiles, params, gridInfo)

    println(CYAN + "Saving unified routing resource graph..." + RESET)
    XMLParser.writeFile(params, RRG, rrg)
  }
}
