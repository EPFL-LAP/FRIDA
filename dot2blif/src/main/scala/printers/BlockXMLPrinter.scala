package printers

import arch._
import core.Namer
import core.AImpl
import archs.Select
import frontend.GlobalParamsInst

object BlockXMLPrinter {
  def clockedPin(pin: Pin, timings: List[Timing]): Boolean = {
    timings
      .collect {
        case reg: RegTiming => reg
      }
      .exists {
        case RegTiming(loc, _, _, _, _) => {
          pin == loc.toPin
        }
      }
  }

  // TODO should we filter according to ports that are pins here?
  def getCombSinks(pin: Pin, pi: PhysicalInfo): Seq[Pin] = {
    pi.timings
      .collect {
        case c: CombTiming => c
      }
      .filter {
        case CombTiming(source, dest, _, _) => {
          source.toPin == pin
        }
      }
      .map(_.dest)
      .map(_.toPin)
  }

  def getDefaultCombSinks(pin: Pin, b: TBlock): Seq[Pin] = {
    b.blockInterface.ports
      .map(_._2)
      .filter(_.id == pin.id.flipped())
      .map {
        bp =>
          {
            (0 until bp.words).map {
              w => {
                assert(bp.id.dummy == Regular)
                Pin(bp.id, w)
              }
            }
          }
      }
      .flatten
      .toSeq
  }

  def maskPins(pins: Seq[Pin], arch: Arch): Seq[Pin] = {
    pins.filter(
      pin => arch.contains(pin.id)
    )
  }

  def printPin(params: GlobalParamsInst, pin: Pin, b: TBlock, arch: Arch): xml.Elem = {
    val pi = b.physicalInfo
    val pName = Namer(pin)

    pin.id.pt match {
      case PTInput => {
        val combSinks = if (pi.timings.nonEmpty && params.timings) {
          maskPins(getCombSinks(pin, pi), arch)
        } else {
          maskPins(getDefaultCombSinks(pin, b), arch)
        }

        val validSinks = maskPins(combSinks, arch)
          .map(
            pin => Namer(pin)
          )
          .mkString(" ")

        if (clockedPin(pin, pi.timings)) {
          <port name={pName} combinational_sink_ports={validSinks} clock="clk"/>
        } else {
          <port name={pName} combinational_sink_ports={validSinks}/>
        }
      }

      case PTOutput => {
        if (clockedPin(pin, pi.timings)) {
          <port name={pName} clock="clk"/>
        } else {
          <port name={pName}/>
        }
      }

      case other => ???
    }
  }

  def printBlockPort(params: GlobalParamsInst, bp: BlockPort, b: TBlock, arch: Arch): List[xml.Elem] = {
    bp.toPins()
      .filter(
        pin => arch.contains(pin.id)
      )
      .map(printPin(params, _, b, arch))
      .toList
  }

  def apply(params: GlobalParamsInst, indent: String, b: TBlock, arch: Arch): String = {
    if (!b.annotations.contains(AImpl) && b.nonEmptyUnder(arch)) {
      val blockInt = b.blockInterface
      val bName = b.prim.vprNameString

      val inPorts = blockInt.ports
        .map(_._2)
        .filter(_.id.pt == PTInput)
        .map(printBlockPort(params, _, b, arch))

      val outPorts = blockInt.ports
        .map(_._2)
        .filter(_.id.pt == PTOutput)
        .map(printBlockPort(params, _, b, arch))

      val blockXml = {
        <model name={bName}>
        <input_ports>
        {inPorts}
        <port name="clk" is_clock="1"/>
        </input_ports>
        <output_ports>
        {outPorts}
        </output_ports>
        </model>
      }

      xml.PrettyPrinter(250, 4).format(blockXml)
    } else {
      ""
    }
  }
}
