package components

import arch.{Mux => AMux, _}
import archs.{OEHB => OB, OEHBStaticInfo => OEHBi, OEHBParams, Params}

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.DecoupledIO
import chisel3.util.RegEnable
import circt.stage.ChiselStage

object OEHB extends ChiselComponent[OEHBParams] {
  def wantedConfigurations: List[OEHBParams] = {
    OEHBParams(32, 1) :: OEHBParams(1, 1) :: OEHBParams(0, 1) ::
      OEHBParams(0, 10) :: OEHBParams(1, 10) :: OEHBParams(32, 10) :: Nil
  }

  def apply(p: OEHBParams, prefix: String = ""): OEHB = {
    new OEHB(p, prefix)
  }

  val clocked = OEHBi.clocked

  def unpackParams(moduleName: String): OEHBParams = {
    assert(moduleName.startsWith(OEHBi.typeString))

    val params =
      moduleName.replace(OEHBi.typeString, "").replace("_optimized", "")
    OEHBi.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: OEHBParams,
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

  def emitVerilog(p: OEHBParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new OEHB(p))
    val moduleName = OB(p).libName

    (v, moduleName)
  }
}

class OEHBRec(p: OEHBParams, prefix: String = "") extends Module {
  // oehb dataless
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + super.desiredName
  val dIn = IO(Flipped(Decoupled(UInt(p.width.W))))
  val dOut = IO(Decoupled(UInt(p.width.W)))

  val dataRegValid = Wire(Bool())
  val fullNextCycle = (dataRegValid & ~dOut.ready)
  dataRegValid := RegNext(fullNextCycle | dIn.valid, 0.B)

  if (p.width > 0) {
    val dataReg =
      RegEnable(dIn.bits, 0.U(p.width.W), ~fullNextCycle & dIn.valid)

    dOut.bits := dataReg
  } else {
    dOut.bits := DontCare
  }

  dOut.valid := dataRegValid
  dIn.ready := ~fullNextCycle

}

class OEHB(p: OEHBParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + OB(p).libName

  val dIn = IO(Flipped(Decoupled(UInt(p.width.W))))
  val dOut = IO(Decoupled(UInt(p.width.W)))

  val oehbs = (0 until p.depth).map(_ =>
    Module(new OEHBRec(p, OB(p).libName + "_" + prefix))
  )

  oehbs(0).dIn <> dIn
  oehbs(p.depth - 1).dOut <> dOut

  (0 until (p.depth - 1)).map { i =>
    {
      oehbs(i).dOut <> oehbs(i + 1).dIn
    }
  }
}

object OEHBGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new OEHB(OEHBParams(32, 4)))
    println(v)
  }
}
