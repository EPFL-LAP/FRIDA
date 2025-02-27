package components

import arch.{Mux => AMux, _}
import archs.{
  ConfigurationBits => ConfBits,
  ConfigurationBitsStaticInfo => ConfBitsi,
  ConfParams,
  Params
}

import chisel3._
import chisel3.util.RegEnable
import circt.stage.ChiselStage

object ConfigurationBits extends ChiselComponent[ConfParams] {
  def wantedConfigurations: List[ConfParams] = {
    (1 to 32).map(i => ConfParams(i)).toList
  }
  def apply(p: ConfParams): ConfigurationBits = {
    new ConfigurationBits(p)
  }

  val clocked = ConfBitsi.clocked

  def unpackParams(moduleName: String): ConfParams = {
    assert(moduleName.startsWith(ConfBitsi.typeString))

    val params =
      moduleName.replace(ConfBitsi.typeString, "").replace("_optimized", "")
    ConfBitsi.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: ConfParams,
      word: Int
  ): PortInstance = ???

  def emitVerilog(p: ConfParams): (String, String) = {
    val v = ChiselStage.emitSystemVerilog(new ConfigurationBits(p))
    val moduleName = ConfBits(p).libName

    (v, moduleName)
  }
}

class ConfigurationBits(p: ConfParams) extends Module {
  override val desiredName = ConfBits(p).libName

  val confIn = IO(Input(Bool()))
  val confOut = IO(Output(Bool()))
  val storeConfig = IO(Input(Bool()))

  val currentConfig = IO(Output(UInt(p.num.W)))

  val prevWires = (0 until (p.num - 1)).map(_ => Wire(Bool()))
  val configBits = (0 until p.num).map { i =>
    {
      val cReg =
        RegEnable(if (i == 0) confIn else prevWires(i - 1), 0.B, storeConfig)
      if (i == (p.num - 1)) {
        confOut := cReg
      } else {
        prevWires(i) := cReg
      }

      cReg
    }
  }

  currentConfig := VecInit(configBits).asUInt
}
