package rrggen

import arch.Arch
import arch.PbType
import arch.TBlock
import frontend.GlobalParamsInst
import printers.ArchPrinter
import analysis._
import archs.Architectures
import archs.Architectures
import util.Util
import util.XMLParser

object ArchitectureLegalizer {
  def getSwitchList(
      archs: List[Arch],
      tiles: Map[String, PbType],
      params: GlobalParamsInst,
      gridInfo: Option[GridInfo]
  ): xml.Elem = {
    val switches = archs.filter(!_.place).map {
      arch =>
        {
          val switchLengths = params.scArch.switchLengths
          Architectures.getSwitchList(gridInfo, arch, params).child.filter(_ \@ "name" != "input_mux")
        }
    }

    val inputMux = <switch type="mux" name="input_mux" R="0" Cin="0" Cout="0" Tdel="0" mux_trans_size="0" buf_size="0"/>
    val allSwitches = inputMux :: switches

    <switchlist>
      {allSwitches}
    </switchlist>
  }

  def getSegmntList(
      archs: List[Arch],
      tiles: Map[String, PbType],
      params: GlobalParamsInst
  ): xml.Elem = {
    val allSegments = archs.filter(!_.place).map {
      arch =>
        {
          val switchLengths = params.scArch.switchLengths
          Architectures.getSegmentList(arch, params).child
        }
    }

    <segmentlist>
      {allSegments}
    </segmentlist>
  }

  def apply(
      archs: List[Arch],
      tiles: Map[String, PbType],
      params: GlobalParamsInst,
      gridInfo: Option[GridInfo]
  ): Unit = {
    val placeArch = archs.filter(_.place).head
    val placeArchFName = "arch_" + placeArch.name + ".xml"

    val uniqueArch = XMLParser.parseFile(params, placeArchFName)

    val nModels = uniqueArch \ "models"

    // TODO how about fc specified here? It is specific to an architecture, it is ignored then?
    val nTiles = uniqueArch \ "tiles"
    val nLayout = uniqueArch \ "layout"
    val nDevice = uniqueArch \ "device"
    val nSwitchList = getSwitchList(archs, tiles, params, gridInfo)
    val nSegmentList = getSegmntList(archs, tiles, params)
    val nDirectList = uniqueArch \ "directlist"
    val nComplexBlocks = uniqueArch \ "complexblocklist"

    // println(xml.PrettyPrinter(250, 4).format(nSwitchList))
    // println(xml.PrettyPrinter(250, 4).format(nSegmentList))

    val nArchXml = {
      <architecture>
        {nModels}
        {nTiles}
        {nLayout}
        {nDevice}
        {nSwitchList}
        {nSegmentList}
        {nDirectList}
        {nComplexBlocks}
      </architecture>
    }

    val archFName = params.buildDir + "/vpr/" + placeArchFName
    val archF = Util.writeOpen(archFName)
    archF.write(xml.PrettyPrinter(250, 4).format(nArchXml))
    archF.close()
  }
}
