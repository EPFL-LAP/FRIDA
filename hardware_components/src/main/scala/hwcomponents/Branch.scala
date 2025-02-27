package components

import arch.{Mux => AMux, _}
import archs.{
  Branch => Br,
  BranchStaticInfo => Bri,
  BranchParams,
  JoinParams,
  Params
}

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.DecoupledIO
import circt.stage.ChiselStage

object Branch extends ChiselComponent[BranchParams] {
  def wantedConfigurations: List[BranchParams] =
    BranchParams(0) :: BranchParams(1) :: BranchParams(32) :: Nil
  val clocked = Bri.clocked

  def unpackParams(moduleName: String): BranchParams = {
    assert(moduleName.startsWith(Bri.typeString))

    val params =
      moduleName.replace(Bri.typeString, "").replace("_optimized", "")
    Bri.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: BranchParams,
      word: Int
  ): PortInstance = {
    portName match {
      case "dIn" =>
        PortInstance(
          BlockPortID(p.width, flipped(PTInput, hsType), pmD, Regular),
          0,
          hsType
        )
      case "condIn" =>
        PortInstance(
          BlockPortID(1, flipped(PTInput, hsType), pmC, Regular),
          0,
          hsType
        )
      case "dOutTrue" =>
        PortInstance(
          BlockPortID(p.width, flipped(PTOutput, hsType), pmD, Regular),
          1,
          hsType
        )
      case "dOutFalse" =>
        PortInstance(
          BlockPortID(p.width, flipped(PTOutput, hsType), pmD, Regular),
          0,
          hsType
        )
    }
  }

  def emitVerilog(p: BranchParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new Branch(p))
    val moduleName = Br(p).libName

    (v, moduleName)
  }
}

class Branch(p: BranchParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Br(p).libName

  val dIn = IO(Flipped(Decoupled(UInt(p.width.W))))
  val condIn = IO(Flipped(Decoupled(Bool())))

  val dOutTrue = IO(Decoupled(UInt(p.width.W)))
  val dOutFalse = IO(Decoupled(UInt(p.width.W)))

  val join = Module(
    Join(
      JoinParams(2, recursivelyExtendPrefix = p.recursivelyExtendPrefix),
      Br(p).libName + "_" + prefix
    )
  )

  join.dIn(0).valid := dIn.valid
  join.dIn(1).valid := condIn.valid

  dIn.ready := join.dIn(0).ready
  condIn.ready := join.dIn(1).ready

  join.dOut.ready := Mux(condIn.bits, dOutTrue.ready, dOutFalse.ready)

  dOutTrue.valid := condIn.bits & join.dOut.valid
  dOutFalse.valid := ~condIn.bits & join.dOut.valid

  dOutTrue.bits := dIn.bits
  dOutFalse.bits := dIn.bits

  join.dIn(0).bits := DontCare
  join.dIn(1).bits := DontCare
}

object BranchGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Branch(BranchParams(0)))
    println(v)
  }
}
