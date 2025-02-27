package components

import arch.{Mux => AMux, _}
import archs.{Mult, MultStaticInfo => Multi, MultParams, JoinParams, Params}

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.DecoupledIO
import chisel3.util.RegEnable
import circt.stage.ChiselStage

object Multiplier extends ChiselComponent[MultParams] {
  def wantedConfigurations: List[MultParams] = {
    MultParams(32) :: Nil
  }

  def apply(p: MultParams): Multiplier = {
    new Multiplier(p)
  }

  def unpackParams(moduleName: String): MultParams = {
    assert(moduleName.startsWith(Multi.typeString))

    val params =
      moduleName.replace(Multi.typeString, "").replace("_optimized", "")
    Multi.unpackLibRep(params)
  }

  val clocked = Multi.clocked

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: MultParams,
      word: Int
  ): PortInstance = {
    portName match {
      case "a" =>
        PortInstance(
          BlockPortID(p.width, flipped(PTInput, hsType), pmD, Regular),
          0,
          hsType
        )
      case "b" =>
        PortInstance(
          BlockPortID(p.width, flipped(PTInput, hsType), pmD, Regular),
          1,
          hsType
        )
      case "res" =>
        PortInstance(
          BlockPortID(p.width, flipped(PTOutput, hsType), pmD, Regular),
          0,
          hsType
        )
    }
  }

  def emitVerilog(p: MultParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new Multiplier(p))
    val moduleName = Mult(p).libName

    (v, moduleName)
  }
}

class Multiplier(p: MultParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Mult(p).libName

  val a = IO(Flipped(Decoupled(UInt(p.width.W))))
  val b = IO(Flipped(Decoupled(UInt(p.width.W))))
  val res = IO(Decoupled(UInt(p.width.W)))

  val join = Module(
    Join(
      JoinParams(2, recursivelyExtendPrefix = p.recursivelyExtendPrefix),
      Mult(p).libName + "_" + prefix
    )
  )

  join.dIn(0).valid := a.valid
  join.dIn(1).valid := b.valid

  a.ready := join.dIn(0).ready
  b.ready := join.dIn(1).ready

  res.valid := join.dOut.valid
  join.dOut.ready := res.ready

  res.bits := a.bits * b.bits

  join.dIn(0).bits := DontCare
  join.dIn(1).bits := DontCare
}

object MultGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Multiplier(MultParams(32)))
    println(v)
  }
}
