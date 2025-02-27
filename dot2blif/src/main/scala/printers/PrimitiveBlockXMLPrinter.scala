package printers

import arch._
import core.Namer
import core.NABlifModel
import frontend.GlobalParamsInst
import util.Util

import math.min
import math.floor

object PrimitiveBlockXMLPrinter {
  def printCombTiming(pb: PrimPb, c: CombTiming, arch: Arch): xml.Elem = {
    assert(arch.contains(c.source.toPin.id) && arch.contains(c.dest.toPin.id))

    val min = if (c.minDelay == "") c.maxDelay else c.minDelay
    val max = if (c.maxDelay == "") c.minDelay else c.maxDelay

    val srcLoc = PbLoc(pb.vprName, c.source.toPin)
    val dstLoc = PbLoc(pb.vprName, c.dest.toPin)

    val inPort = Namer(srcLoc)
    val outPort = Namer(dstLoc)

    <delay_constant max={max} min={min} in_port={inPort} out_port={outPort}/>
  }

  def printRegTiming(pb: PrimPb, r: RegTiming, arch: Arch): List[xml.Elem] = {
    if (!arch.contains(r.loc.toPin.id)) {
      Nil
    } else {
      val loc = PbLoc(pb.vprName, r.loc.toPin)
      val pin = Namer(loc)

      val tsetup = if (r.t_setup == "") "0.0e-12" else r.t_setup
      val tcq_min = if (r.t_clock_to_q_min == "") "0.0e-12" else r.t_clock_to_q_min
      val tcq_max = if (r.t_clock_to_q_max == "") "0.0e-12" else r.t_clock_to_q_max

      assert(r.t_clock_to_q_max == r.t_clock_to_q_min)

      val setup = <T_setup value={tsetup} port={pin} clock="clk"/>
      val clock_q = <T_clock_to_Q max={tcq_max} min={tcq_min} port={pin} clock="clk"/>

      setup :: clock_q :: Nil
    }
  }

  def printTimings(pb: PrimPb, arch: Arch): List[xml.Elem] = {
    pb.prim.physicalInfo.timings
      .filter {
        case c: CombTiming => {
          arch.contains(c.source.toPin.id) && arch.contains(c.dest.toPin.id)
        }

        case r: RegTiming => {
          arch.contains(r.loc.toPin.id)
        }
      }
      .map {
        case c: CombTiming => {
          printCombTiming(pb, c, arch) :: Nil
        }

        case r: RegTiming => {
          printRegTiming(pb, r, arch)
        }
      }
      .flatten
  }

  def printPin(pb: PbType, pin: Pin, arch: Arch): xml.Elem = {
    assert(arch.contains(pin.id))

    val pName = Namer(pin)

    if (pb.isInstanceOf[RootPb]) {
      pin.id.pt match {
        case PTInput => {
          <input name={pName} num_pins="1" equivalent="none"/>
        }

        case PTOutput => {
          <output name={pName} num_pins="1" equivalent="none"/>
        }

        case other => scala.sys.error("Expected defined pin direction.")
      }
    } else {
      pin.id.pt match {
        case PTInput => {
          <input name={pName} num_pins="1"/>
        }

        case PTOutput => {
          <output name={pName} num_pins="1"/>
        }

        case other => scala.sys.error("Expected defined pin direction.")
      }
    }
  }

  def printBlockPort(pb: PbType, bp: BlockPort, arch: Arch): List[xml.Elem] = {
    (0 until bp.words).map {
      w =>
        {
          printPin(pb, Pin(bp.id, w), arch)
        }
    }.toList
  }

  def printInterface(pb: PbType, arch: Arch): List[xml.Elem] = {
    val pins = pb.bi.ports
      .filter(
        (id, bp) => arch.contains(id)
      )
      .map(_._2)
      .map(_.toPins())
      .flatten

    val pinsXml = pins.toList
      .sortBy(Namer(_))
      .map {
        pin =>
          {
            printPin(pb, pin, arch)
          }
      }
      .toList

    if (pb.hasClockPin()) {
      val clkXml = <clock name="clk" num_pins="1"/>

      clkXml :: pinsXml
    } else {
      pinsXml
    }
  }

  def printCLK(pb: PbType, c: CLK, arch: Arch): xml.Elem = {
    val targetPb = pb.subBlocks.filter(_.name == c.dstPb).head
    assert(targetPb.nonEmptyUnder(arch))

    val (inC, outC, cName) = Namer(c)
    <direct name={cName} input={inC} output={outC}/>
  }

  def printMux(params: GlobalParamsInst, m: Mux, arch: Arch): xml.Elem = {
    assert(m.delay.nonEmpty)
    assert(m.delay.get.maxDelay == m.delay.get.minDelay)

    val (name, srcStr, dstStr) = Namer(m)

    val max = m.delay.get.maxDelay
    val min = m.delay.get.minDelay

    val srcNames = m.sources.map(Namer(_))
    val srcs = srcNames.mkString(" ")

    val delays = srcNames.map {
      src =>
        {
          <delay_constant max={max} min={min} in_port={src} out_port={dstStr}/>
        }
    }

    if(params.timings) {
      <mux name={name} input={srcs} output={dstStr}>
      {delays}
      </mux>
    } else {
      <mux name={name} input={srcs} output={dstStr}/>
    }
  }

  def printDirect(params: GlobalParamsInst, d: Direct, arch: Arch): xml.Elem = {
    assert(arch.contains(d.dstLoc.pin.id))

    val (name, srcStr, dstStr) = Namer(d)

    if (d.delay.isEmpty || !params.timings) {
      <direct name={name} input={srcStr} output={dstStr}/>
    } else {
      val max = d.delay.get.maxDelay
      val min = d.delay.get.minDelay

      val delay = {
        <delay_constant max={max} min={min} in_port={srcStr} out_port={dstStr}/>
      }

      <direct name={name} input={srcStr} output={dstStr}>
      {delay}
      </direct>
    }
  }

  def printLinks(params: GlobalParamsInst, pb: PbType, arch: Arch): List[xml.Elem] = {
    pb.links
      .filter {
        case d: Direct => arch.contains(d.srcLoc.pin.id) && arch.contains(d.dstLoc.pin.id)
        case m: Mux    => arch.contains(m.dstLoc.pin.id)
        case clk: CLK => {
          val targetPb = pb.subBlocks.filter(_.name == clk.dstPb)
          assert(targetPb.size == 1, clk)

          targetPb.head.nonEmptyUnder(arch)
        }
      }
      .map {
        case d: Direct => printDirect(params, d, arch)
        case m: Mux    => printMux(params, m, arch)
        case clk: CLK  => printCLK(pb, clk, arch)
      }
  }

  def printPrim(params: GlobalParamsInst, prim: PrimPb, arch: Arch, subBlocks: List[xml.Elem]): xml.Elem = {
    assert(subBlocks.isEmpty)

    val interfacePins = printInterface(prim, arch)

    val pbName = prim.vprName
    val modelName = prim.prim.vprName

    if(params.timings) {
      val timings = printTimings(prim, arch)

      <pb_type name={pbName} num_pb="1" blif_model={modelName}>
      {interfacePins}
      {timings}
      </pb_type>
    } else {
      <pb_type name={pbName} num_pb="1" blif_model={modelName}>
      {interfacePins}
      </pb_type>
    }
  }

  def printRoot(params: GlobalParamsInst, root: RootPb, arch: Arch, subBlocks: List[xml.Elem]): xml.Elem = {
    val interfacePins = printInterface(root, arch)
    val links = printLinks(params, root, arch)

    val pbName = root.vprName // root.prim.vprName

    <pb_type name={pbName}>
    {interfacePins}
    {subBlocks}
    <interconnect>
    {links}
    </interconnect>
    </pb_type>
  }

  def printInter(params: GlobalParamsInst, inter: InterPb, arch: Arch, subBlocks: List[xml.Elem]): xml.Elem = {
    val interfacePins = printInterface(inter, arch)
    val links = printLinks(params, inter, arch)

    val pbName = inter.vprName // inter.prim.vprName

    <pb_type name={pbName} num_pb="1">
    {interfacePins}
    {subBlocks}
    <interconnect>
    {links}
    </interconnect>
    </pb_type>
  }

  def apply(params: GlobalParamsInst, pb: PbType, arch: Arch): xml.Elem = {
    assert(pb.nonEmptyUnder(arch), pb.name)

    val subBlocks = pb.subBlocks
      .filter(_.nonEmptyUnder(arch))
      .map {
        recPb =>
          {
            PrimitiveBlockXMLPrinter(params, recPb, arch)
          }
      }
      .toList

    pb match {
      case prim: PrimPb => {
        printPrim(params, prim, arch, subBlocks)
      }

      case inter: InterPb => {
        printInter(params, inter, arch, subBlocks)
      }

      case root: RootPb => {
        printRoot(params, root, arch, subBlocks)
      }
    }
  }

  def apply(params: GlobalParamsInst, pb: RootPb, arch: Arch, blocks: Map[String, TBlock]): String = {
    val tileXml = PrimitiveBlockXMLPrinter(params, pb, arch)
    val legalized = LegalizeIO(tileXml, true, false, blocks, None)

    xml.PrettyPrinter(250, 4).format(legalized)
  }
}
