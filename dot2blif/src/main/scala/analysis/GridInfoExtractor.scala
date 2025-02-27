package analysis

import arch.Arch
import arch.RootPb
import arch.TBlock
import frontend.GlobalParamsInst

// sbInfo is only used to print the right muxs in the architecture file
case class GridInfo(tileUnitArea: Double, conInfo: Map[Arch, RRGConInfo], sbInfo: Map[Arch, Map[WireLength, TBlock]])

object GridInfoExtractor {
  def apply(
      params: GlobalParamsInst,
      archs: List[Arch],
      tiles: Map[String, RootPb]
  ): GridInfo = {
    val conInfo = RRGConnectivityExtractor(params, archs)
    val area = AreaExtractor.getBiggestTileArea(params, tiles, archs, conInfo).areaWithCap()
    val sbInfo = AreaExtractor.getTileSB(tiles, archs, params, conInfo)

    GridInfo(area, conInfo, sbInfo)
  }
}
