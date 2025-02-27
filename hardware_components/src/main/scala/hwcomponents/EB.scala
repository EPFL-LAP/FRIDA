package components

import arch.{Mux => AMux, _}
import archs.{
  EB => EBuf,
  EBStaticInfo => EBi,
  EBParams,
  TEHBParams,
  OEHBParams,
  Params
}

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.DecoupledIO
import chisel3.util.RegEnable
import circt.stage.ChiselStage

object EB extends ChiselComponent[EBParams] {
  def wantedConfigurations: List[EBParams] =
    EBParams(0, 1) :: EBParams(1, 1) :: EBParams(32, 1) :: Nil

  val clocked = EBi.clocked

  def unpackParams(moduleName: String): EBParams = {
    assert(moduleName.startsWith(EBi.typeString))

    val params =
      moduleName.replace(EBi.typeString, "").replace("_optimized", "")
    EBi.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: EBParams,
      word: Int
  ): PortInstance = {
    val width = p.width

    portName match {
      case "dIn" =>
        PortInstance(
          BlockPortID(width, flipped(PTInput, hsType), pmD, Regular),
          0,
          hsType
        )
      case "dOut" =>
        PortInstance(
          BlockPortID(width, flipped(PTOutput, hsType), pmD, Regular),
          0,
          hsType
        )
    }
  }

  def emitVerilog(p: EBParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new EB(p))
    val moduleName = EBuf(p).libName

    (v, moduleName)
  }
}

// TODO handle depth properly here
class EB(p: EBParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + EBuf(p).libName

  val dIn = IO(Flipped(Decoupled(UInt(p.width.W))))
  val dOut = IO(Decoupled(UInt(p.width.W)))

  val tehb = Module(
    TEHB(
      TEHBParams(
        p.width,
        p.depth,
        recursivelyExtendPrefix = p.recursivelyExtendPrefix
      ),
      EBuf(p).libName + "_" + prefix
    )
  )
  val oehb = Module(
    OEHB(
      OEHBParams(
        p.width,
        p.depth,
        recursivelyExtendPrefix = p.recursivelyExtendPrefix
      ),
      EBuf(p).libName + "_" + prefix
    )
  )

  dIn <> tehb.dIn
  tehb.dOut <> oehb.dIn
  oehb.dOut <> dOut
}

object EBGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new EB(EBParams(32, 1)))
    println(v)
  }
}
