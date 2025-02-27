package components

import arch.{Mux => AMux, _}
import archs.{TEHB => TB, TEHBStaticInfo => TEHBi, TEHBParams, Params}

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.DecoupledIO
import chisel3.util.RegEnable
import circt.stage.ChiselStage

object TEHB extends ChiselComponent[TEHBParams] {
  def wantedConfigurations: List[TEHBParams] = {
    TEHBParams(32, 1) :: TEHBParams(1, 1) :: TEHBParams(0, 1) ::
      TEHBParams(32, 3) :: TEHBParams(1, 3) :: TEHBParams(0, 3) ::
      TEHBParams(32, 2) :: TEHBParams(1, 2) :: TEHBParams(0, 2) ::
      TEHBParams(32, 7) :: TEHBParams(1, 7) :: TEHBParams(0, 7) ::
      TEHBParams(0, 10) :: TEHBParams(1, 10) :: TEHBParams(32, 10) :: Nil
  }

  def apply(p: TEHBParams, prefix: String = ""): TEHB = {
    new TEHB(p, prefix)
  }

  val clocked = TEHBi.clocked

  def unpackParams(moduleName: String): TEHBParams = {
    assert(moduleName.startsWith(TEHBi.typeString))

    val params =
      moduleName.replace(TEHBi.typeString, "").replace("_optimized", "")
    TEHBi.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: TEHBParams,
      word: Int
  ): PortInstance = {
    val width = p.width

    portName match {
      case "dIn" =>
        PortInstance(
          BlockPortID(width, flipped(PTInput, hsType), pmD, Regular),
          word,
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

  def emitVerilog(p: TEHBParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new TEHB(p))
    val moduleName = TB(p).libName

    (v, moduleName)
  }
}

class TEHBRec(p: TEHBParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + super.desiredName
  val dIn = IO(Flipped(Decoupled(UInt(p.width.W))))
  val dOut = IO(Decoupled(UInt(p.width.W)))

  val tehbFull = Wire(Bool())
  tehbFull := RegNext((dIn.valid | tehbFull) & ~dOut.ready, 0.B)

  if (p.width > 0) {
    val dataReg = RegEnable(dIn.bits, 0.U(p.width.W), ~tehbFull)

    when(tehbFull) {
      dOut.bits := dataReg
    }.otherwise {
      dOut.bits := dIn.bits
    }
  } else {
    dOut.bits := DontCare
  }

  dOut.valid := tehbFull | dIn.valid
  dIn.ready := ~tehbFull
}

class TEHB(p: TEHBParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + TB(p).libName

  val dIn = IO(Flipped(Decoupled(UInt(p.width.W))))
  val dOut = IO(Decoupled(UInt(p.width.W)))

  val tehbs = (0 until p.depth).map(_ =>
    Module(new TEHBRec(p, TB(p).libName + "_" + prefix))
  )

  tehbs(0).dIn <> dIn
  tehbs(p.depth - 1).dOut <> dOut

  (0 until (p.depth - 1)).map { i =>
    {
      tehbs(i).dOut <> tehbs(i + 1).dIn
    }
  }
}

object TEHBGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new TEHB(TEHBParams(32, 4)))
    println(v)
  }
}
