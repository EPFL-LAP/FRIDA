package components

import arch.{Mux => AMux, _}
import archs.{Sink => Snk, SinkStaticInfo => Snki, SinkParams, Params}

import chisel3._
import chisel3.util.DecoupledIO
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import circt.stage.ChiselStage

object Sink extends ChiselComponent[SinkParams] {
  def wantedConfigurations: List[SinkParams] = {
    SinkParams(0) :: SinkParams(1) :: SinkParams(32) :: Nil
  }

  def apply(p: SinkParams): Sink = {
    new Sink(p)
  }

  def unpackParams(moduleName: String): SinkParams = {
    assert(moduleName.startsWith(Snki.typeString))

    val params =
      moduleName.replace(Snki.typeString, "").replace("_optimized", "")
    Snki.unpackLibRep(params)
  }

  val clocked = Snki.clocked

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: SinkParams,
      word: Int
  ): PortInstance = {
    portName match {
      case "dIn" =>
        PortInstance(BlockPortID(0, PTInput, pmD, Regular), 0, hsType)
    }
  }

  def emitVerilog(p: SinkParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new Sink(p))
    val moduleName = Snk(p).libName

    (v, moduleName)
  }
}

class Sink(p: SinkParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Snk(p).libName

  val dIn = IO(Flipped(Decoupled(UInt(p.width.W))))

  dIn.bits := DontCare
  dIn.valid := DontCare
  dIn.ready := true.B
}

object SinkGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Sink(SinkParams(0)))
    println(v)
  }
}
