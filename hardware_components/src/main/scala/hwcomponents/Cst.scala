package components

import arch.{Mux => AMux, _}
import archs.{Constant, ConstantStaticInfo => Csti, ConstantParams, Params}

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.DecoupledIO
import chisel3.util.RegEnable
import circt.stage.ChiselStage

object Cst
    extends ChiselComponent[ConstantParams]
    with WireComponent[ConstantParams] {
  def wantedConfigurations: List[ConstantParams] =
    ConstantParams(1) :: ConstantParams(32) :: Nil

  val clocked = Csti.clocked

  def unpackParams(moduleName: String): ConstantParams = {
    assert(moduleName.startsWith(Csti.typeString))

    val params =
      moduleName.replace(Csti.typeString, "").replace("_optimized", "")
    Csti.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: ConstantParams,
      word: Int
  ): PortInstance = {
    val width = p.width

    portName match {
      case "tokenIn" =>
        PortInstance(
          BlockPortID(0, flipped(PTInput, hsType), pmD, Regular),
          0,
          hsType
        )
      case "dOut" =>
        PortInstance(
          BlockPortID(width, flipped(PTOutput, hsType), pmD, Regular),
          0,
          hsType
        )
      case "conf" =>
        PortInstance(BlockPortID(width, PTInput, pmConf, Regular), 0, None)
    }
  }

  def emitVerilog(p: ConstantParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new Cst(p))
    val moduleName = Constant(p).libName

    (v, moduleName)
  }

  def defaultDelays(p: ConstantParams): String = {
    val zeroDelay = "0.0e-12"

    Constant(p).libName + "_optimized(0) { } : [\n" +
      "(0, In, PMData, Reg)[0].v -> (" + p.width + ", Out, PMData, Reg)[0].v : " +
      "{ minDelay=\"" + zeroDelay + "\", maxDelay=\"" + zeroDelay + "\" },\n" +
      "(" + p.width + ", Out, PMData, Reg)[0].r -> (0, In, PMData, Reg)[0].r : " +
      "{ minDelay=\"" + zeroDelay + "\", maxDelay=\"" + zeroDelay + "\" }\n" +
      "]\n"
  }
}

class Cst(p: ConstantParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Constant(p).libName

  val tokenIn = IO(Flipped(Decoupled(UInt(0.W))))
  val dOut = IO(Decoupled(UInt(p.width.W)))
  val conf = IO(Input(UInt(p.width.W)))

  val cstVal = conf

  dOut.bits := cstVal
  dOut.valid := tokenIn.valid
  tokenIn.ready := dOut.ready
}

object CstGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Cst(ConstantParams(32)))
    println(v)
  }
}
