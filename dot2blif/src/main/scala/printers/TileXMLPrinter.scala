package printers

import arch._
import core.Namer
import archs.Frac
import archs.ControlConfig.pinLocation

object TileXMLPrinter {
  def apply(indent: String, t: RootPb, blocks: Map[String, TBlock], arch: Arch): String = {
    if (t.nonEmptyUnder(arch)) {
      val theight = t.vprConfig.height.toString()
      val twidth = t.vprConfig.width.toString()
      val tcapacity = t.vprConfig.capacity.toString()

      val fc_in_type = arch.fcIn.t.str
      val fc_in_val = arch.fcIn.value.toString()
      val fc_out_type = arch.fcOut.t.str
      val fc_out_val = arch.fcOut.value.toString()

      val switchblock_pattern = t.vprConfig.switchblockPattern.str

      val interfacePorts = PrimitiveBlockXMLPrinter.printInterface(t, arch)
      val pinLocations = t.vprConfig.pinLocation.pinLocXML(t, arch)

      val tileXml = {
        <tile name={t.name} width={twidth} height={theight} area="0">
        <sub_tile name={t.name} capacity={tcapacity}>
        <equivalent_sites>
        <site pb_type={t.name} pin_mapping="direct"/>
        </equivalent_sites>
        {interfacePorts}
        <fc in_type={fc_in_type} in_val={fc_in_val} out_type={fc_out_type} out_val={fc_out_val}/>
        {pinLocations}
        <switchblock_locations pattern={switchblock_pattern}/>
        </sub_tile>
        </tile>
      }

      xml.PrettyPrinter(250, 4).format(tileXml)
    } else {
      ""
    }
  }
}
