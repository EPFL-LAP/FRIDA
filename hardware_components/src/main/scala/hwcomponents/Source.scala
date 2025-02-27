package components

import arch.{Mux => AMux, _}
import archs.{Source => Src, SourceStaticInfo => Srci, SourceParams, Params}

import chisel3._
import chisel3.util.DecoupledIO
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import chisel3.util.MuxLookup
import circt.stage.ChiselStage

object Source extends ChiselComponent[SourceParams] {
  def wantedConfigurations: List[SourceParams] = {
    SourceParams() :: Nil
  }

  def unpackParams(moduleName: String): SourceParams = {
    assert(moduleName.startsWith(Srci.typeString))

    val params =
      moduleName.replace(Srci.typeString, "").replace("_optimized", "")
    Srci.unpackLibRep(params)
  }

  def apply(p: SourceParams): Source = {
    new Source(p)
  }

  val clocked = Srci.clocked

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: SourceParams,
      word: Int
  ): PortInstance = {
    portName match {
      case "dOut" =>
        PortInstance(BlockPortID(0, PTOutput, pmD, Regular), 0, hsType)
    }
  }

  def emitVerilog(p: SourceParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new Source(p))
    val moduleName = Src(p).libName

    (v, moduleName)
  }
}

class Source(p: SourceParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Src(p).libName

  val dOut = IO(Decoupled(UInt(0.W)))

  dOut.bits := DontCare
  dOut.valid := true.B
  dOut.ready := DontCare
}

object SourceGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Source(SourceParams()))
    println(v)
  }
}
