package components

import arch.{Mux => AMux, _}
import archs.{
  Select => Slct,
  SelectStaticInfo => Slcti,
  SelectParams,
  MuxConfigParams,
  Params
}

import chisel3._
import chisel3.util.DecoupledIO
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import chisel3.util.MuxLookup
import circt.stage.ChiselStage

object Select extends ChiselComponent[SelectParams] {
  def wantedConfigurations: List[SelectParams] = {
    SelectParams(32, 2) :: SelectParams(1, 2) :: SelectParams(0, 2) :: Nil
  }

  val clocked = Slcti.clocked

  def unpackParams(moduleName: String): SelectParams = {
    assert(moduleName.startsWith(Slcti.typeString))

    val params =
      moduleName.replace(Slcti.typeString, "").replace("_optimized", "")
    Slcti.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: SelectParams,
      word: Int
  ): PortInstance = {
    portName match {
      case "dIn" =>
        PortInstance(
          BlockPortID(p.width, flipped(PTInput, hsType), pmD, Regular),
          word,
          hsType
        )
      case "condIn" =>
        PortInstance(
          BlockPortID(1, flipped(PTInput, hsType), pmC, Regular),
          word,
          hsType
        )
      case "dOut" =>
        PortInstance(
          BlockPortID(p.width, flipped(PTOutput, hsType), pmD, Regular),
          0,
          hsType
        )
    }
  }

  def emitVerilog(p: SelectParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new Select(p))
    val moduleName = Slct(p).libName

    (v, moduleName)
  }
}

class AntiTokens(recursivelyExtendPrefix: Boolean = false, prefix: String = "")
    extends Module {
  override val desiredName = {
    if (recursivelyExtendPrefix) prefix else ""
  } + super.desiredName
  val inValids = IO(Input(Vec(2, Bool())))
  val inGenerates = IO(Input(Vec(2, Bool())))
  val outKill = IO(Output(Vec(2, Bool())))
  val stopValid = IO(Output(Bool()))

  val reg0 = RegInit(0.B)
  val reg1 = RegInit(0.B)

  reg0 := ~inValids(0) & (inGenerates(0) | reg0)
  reg1 := ~inValids(1) & (inGenerates(1) | reg1)

  stopValid := reg0 | reg1

  outKill(0) := inGenerates(0) | reg0
  outKill(1) := inGenerates(1) | reg1
}

class Select(p: SelectParams, prefix: String = "") extends Module {
  assert(p.num == 2) // TODO extend later for other sizes

  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Slct(p).libName
  val condSize = log2Ceil(p.num)

  val dIn = IO(Flipped(Vec(p.num, Decoupled(UInt(p.width.W)))))
  val condIn = IO(Flipped(Decoupled(UInt(condSize.W))))
  val dOut = IO(Decoupled(UInt(p.width.W)))

  if (p.width > 0) {
    val dataMux = Module(
      MuxConfig(
        MuxConfigParams(
          p.width,
          p.num,
          0,
          0,
          recursivelyExtendPrefix = p.recursivelyExtendPrefix
        ),
        Slct(p).libName + "_" + prefix
      )
    )
    dataMux.dIn := dIn.map(_.bits)
    dataMux.conf := condIn.bits
    dOut.bits := dataMux.dOut
  } else {
    dOut.bits := DontCare
  }

  val antiTokens = Module(
    new AntiTokens(p.recursivelyExtendPrefix, Slct(p).libName + "_" + prefix)
  )
  antiTokens.inValids(0) := dIn(0).valid
  antiTokens.inValids(1) := dIn(1).valid

  val ee =
    condIn.valid & ((~condIn.bits(0) & dIn(1).valid) | (condIn.bits(0) & dIn(
      0
    ).valid))
  val vInt = ee & ~antiTokens.stopValid

  val g0 = ~dIn(0).valid & vInt & dOut.ready
  val g1 = ~dIn(1).valid & vInt & dOut.ready
  antiTokens.inGenerates(0) := g0
  antiTokens.inGenerates(1) := g1

  dOut.valid := vInt
  dIn(0).ready := (~dIn(0).valid) | (vInt & dOut.ready) | antiTokens.outKill(0)
  dIn(1).ready := (~dIn(1).valid) | (vInt & dOut.ready) | antiTokens.outKill(1)
  condIn.ready := (~condIn.valid) | (vInt & dOut.ready)
}

object SelectGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Select(SelectParams(1, 2)))
    println(v)
  }
}
