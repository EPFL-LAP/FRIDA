package archs

import arch.Arch
import arch.D
import arch.Hs
import arch.Vld
import arch.Rdy
import arch.Impl
import analysis._
import frontend.GlobalParamsInst
import arch.AddInterconnectDelay
import arch.PTInput

sealed trait Layout
case object AutoLayout extends Layout
case class FixedLayout(width: Int, height: Int) extends Layout

case class ArchDef(
    name: String,
    tiles: List[TileGen],
    layout: Layout,
    switchLengths: List[Int],
    archs: List[Arch]
) {
  def numTracks(width: Int): Int = {
    val candidates = archs.filter(!_.place).filter(_.width == width)
    assert(candidates.size == 1)

    candidates.head.width
  }

  override def toString(): String = {
    val layoutStr = layout match {
      case AutoLayout                 => ???
      case FixedLayout(width, height) => "" + width + "x" + height
    }

    val switchLenghtsStr = switchLengths
      .map(
        l => "L" + l
      )
      .mkString("")
    val archStr = archs
      .filter(!_.place)
      .map(
        a => a.name + "fcin" + a.fcIn.toString().replace(".", "") + "_fcout" + a.fcOut.toString().replace(".", "")
      )
      .mkString("_")

    name + "_" + layoutStr + "_" + switchLenghtsStr + "__" + archStr
  }
}

object Architectures {
  // TODO The input mux shoud go eventually
  def defaultDevice: xml.Elem = {
    <device>
      <sizing R_minW_nmos="0" R_minW_pmos="0"/>
      <area grid_logic_tile_area="0"/>
      <chan_width_distr>
        <x distr="uniform" peak="1.00000"/>
        <y distr="uniform" peak="1.00000"/>
      </chan_width_distr>
      <switch_block type="wilton" fs="3"/>
      <connection_block input_switch_name="input_mux"/>
    </device>
  }

  def defaultDirectList: xml.Text = {
    xml.Text("")
  }

  def getSegmentList(arch: Arch, params: GlobalParamsInst): xml.Elem = {
    val switchLengths = params.scArch.switchLengths

    val segments = switchLengths.map {
      l =>
        {
          val swName = "L" + l + "_" + arch.name
          val segName = "seg_" + arch.name

          val length = l.toString()

          val sbLocs = (0 until (l + 1))
            .map(
              _ => 1
            )
            .mkString(" ")
          val cbLocs = (0 until (l))
            .map(
              _ => 1
            )
            .mkString(" ")

          <segment name={segName} freq="1" length={length} type="unidir" Rmetal="0" Cmetal="0">
          <mux name={swName}/>
          <sb type="pattern">{sbLocs}</sb>
          <cb type="pattern">{cbLocs}</cb>
        </segment>
        }
    }

    <segmentlist>
      {segments}
    </segmentlist>
  }

  // // VPR does not support this for CBlock muxes
  // // TODO deprecated
  // def genInputMux(arch: Arch, params: GlobalParamsInst): xml.Elem = {
  //   val cmuxWidth = if(arch.place) {
  //     32 // it is the slowest switch
  //   } else {
  //     arch.width
  //   }

  //   val delays = (2 until 33).map {
  //     i => {
  //       val del = AddInterconnectDelay.getMuxWorseDelay(cmuxWidth, i)(params).maxDelay
  //       <Tdel num_inputs={i.toString()} delay={del}/>
  //     }
  //   }

  //   val sName = "input_mux_" + arch.name

  //   <switch type="mux" name={sName} R="0" Cout="0" Cin="0" mux_trans_size="0" buf_size="0">
  //     {delays}
  //   </switch>
  // }

  def getSwitchList(
      grid: Option[GridInfo],
      arch: Arch,
      params: GlobalParamsInst
  ): xml.Elem = {
    val switchLengths = params.scArch.switchLengths

    val switchs = if (grid.isEmpty) {
      switchLengths.map {
        l =>
          {
            val sName = "L" + l + "_" + arch.name

            <switch type="mux" name={sName} R="0" Cin="0" Cout="0" Tdel="0" mux_trans_size="0" buf_size="0"/>
          }
      }
    } else {
      switchLengths.map {
        l =>
          {
            val sName = "L" + l + "_" + arch.name

            val delay = if (arch.place) {
              "" + grid.get.sbInfo
                .map(
                  (arch, info) => info(l).physicalInfo.criticalPath
                )
                .max + "e-9"
            } else {
              "" + grid.get.sbInfo(arch)(l).physicalInfo.criticalPath + "e-9"
            }

            <switch type="mux" name={sName} R="0" Cin="0" Cout="0" Tdel={delay} mux_trans_size="0" buf_size="0"/>
          }
      }
    }

    val inputMux = <switch type="mux" name="input_mux" R="0" Cin="0" Cout="0" Tdel="0" mux_trans_size="0" buf_size="0"/>

    val allSwitchs = inputMux :: switchs

    <switchlist>
      {allSwitchs}
    </switchlist>
  }

  def decoupled = ArchDef(
    "decoupled",
    (Second :: Control :: Third :: IoTile :: Nil),
    // (Third :: Nil),
    FixedLayout(12, 12), // FixedLayout(10, 10),
    1 :: 2 :: Nil,
    Arch.place(50)
      :: Arch("one", 1, Set(Vld, Rdy, D), false, 60, VPRFC(Frac, 0.25), VPRFC(Frac, 0.25))
      :: Arch("d", 32, Set(D), false, 10, VPRFC(Frac, 0.5), VPRFC(Frac, 0.05)) :: Nil
  )

  def simple = ArchDef(
    "simple",
    (Simple :: IoTile :: Nil),
    FixedLayout(12, 12),
    1 :: 2 :: Nil,
    Arch.place(50)
      :: Arch("one", 1, Set(Vld, Rdy, D), false, 150, VPRFC(Frac, 1), VPRFC(Frac, 1))
      :: Arch("d", 32, Set(D), false, 20, VPRFC(Frac, 1), VPRFC(Frac, 1)) :: Nil
  )

  def uniform = ArchDef(
    "uniform",
    (Second :: Third :: IoTile :: Nil),
    FixedLayout(15, 15), // FixedLayout(10, 10),
    1 :: 2 :: Nil,
    Arch.place(50)
      :: Arch("one", 1, Set(Vld, Rdy, D), false, 40, VPRFC(Frac, 0.25), VPRFC(Frac, 0.25))
      :: Arch("d", 32, Set(D), false, 10, VPRFC(Frac, 0.5), VPRFC(Frac, 0.05)) :: Nil
  )

  def packSimple = ArchDef(
    "packSimple",
    (PackSimple :: ForkTile :: IoTile :: Nil),
    FixedLayout(12, 12),
    1 :: Nil, // What comes after does not matter for the packing tests
    Arch.place(50)
      :: Arch("one", 1, Set(Vld, Rdy, D), false, 60, VPRFC(Frac, 0.25), VPRFC(Frac, 0.25))
      :: Arch("d", 32, Set(D), false, 10, VPRFC(Frac, 0.5), VPRFC(Frac, 0.05)) :: Nil
  )

  def packMac = ArchDef(
    "packMac",
    (PackMac :: ForkTile :: IoTile :: Nil),
    FixedLayout(12, 12),
    1 :: Nil, // What comes after does not matter for the packing tests
    Arch.place(50)
      :: Arch("one", 1, Set(Vld, Rdy, D), false, 60, VPRFC(Frac, 0.25), VPRFC(Frac, 0.25))
      :: Arch("d", 32, Set(D), false, 10, VPRFC(Frac, 0.5), VPRFC(Frac, 0.05)) :: Nil
  )

  def packTree = ArchDef(
    "packTree",
    (PackTree :: ForkTile :: IoTile :: Nil),
    FixedLayout(12, 12),
    1 :: Nil, // What comes after does not matter for the packing tests
    Arch.place(50)
      :: Arch("one", 1, Set(Vld, Rdy, D), false, 60, VPRFC(Frac, 0.25), VPRFC(Frac, 0.25))
      :: Arch("d", 32, Set(D), false, 10, VPRFC(Frac, 0.5), VPRFC(Frac, 0.05)) :: Nil
  )

  def bpArch = ArchDef(
    "bp",
    (BPArch :: ForkTile :: IoTile :: Nil),
    FixedLayout(12, 12),
    1 :: Nil, // What comes after does not matter for the packing tests
    Arch.place(50)
      :: Arch("one", 1, Set(Vld, Rdy, D), false, 60, VPRFC(Frac, 0.25), VPRFC(Frac, 0.25))
      :: Arch("d", 32, Set(D), false, 10, VPRFC(Frac, 0.5), VPRFC(Frac, 0.05)) :: Nil
  )


  def asymArch = ArchDef(
    "asym",
    (ASymArch :: ForkTile :: IoTile :: Nil),
    FixedLayout(12, 12),
    1 :: Nil, // What comes after does not matter for the packing tests
    Arch.place(50)
      :: Arch("one", 1, Set(Vld, Rdy, D), false, 60, VPRFC(Frac, 0.25), VPRFC(Frac, 0.25))
      :: Arch("d", 32, Set(D), false, 10, VPRFC(Frac, 0.5), VPRFC(Frac, 0.05)) :: Nil
  )


  val archs = Map[String, ArchDef](
    ("decoupled" -> decoupled),
    ("simple" -> simple),
    ("uniform" -> uniform),
    ("packSimple" -> packSimple),
    ("packMac" -> packMac),
    ("packTree" -> packTree),
    ("bp" -> bpArch),
    ("asym" -> asymArch)
  )

  def apply(archName: String): ArchDef = {
    archs(archName)
  }
}
