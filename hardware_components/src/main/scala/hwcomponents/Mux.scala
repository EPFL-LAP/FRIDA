package components

import arch.{Mux => AMux, _}
import archs.{
  Mux => Mx,
  MuxStaticInfo => Mxi,
  MuxParams,
  MuxConfigParams,
  TEHBParams,
  Params
}

import chisel3._
import chisel3.util.DecoupledIO
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import chisel3.util.MuxLookup
import circt.stage.ChiselStage

object Mux extends ChiselComponent[MuxParams] {
  def wantedConfigurations: List[MuxParams] = {
    MuxParams(32, 2) :: MuxParams(1, 2) :: MuxParams(0, 2) :: Nil
  }

  val clocked = Mxi.clocked

  def unpackParams(moduleName: String): MuxParams = {
    assert(moduleName.startsWith(Mxi.typeString))

    val params =
      moduleName.replace(Mxi.typeString, "").replace("_optimized", "")
    Mxi.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: MuxParams,
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

  def emitVerilog(p: MuxParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new Mux(p))
    val moduleName = Mx(p).libName

    (v, moduleName)
  }
}

class Mux(p: MuxParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Mx(p).libName

  // To ensure correct simulation, since when N == 1 a constant is inserted on the condition port.
  val minNum = if(p.num == 1) 2 else p.num

  val condSize = log2Ceil(minNum)

  val dIn = IO(Flipped(Vec(minNum, Decoupled(UInt(p.width.W)))))
  val condIn = IO(Flipped(Decoupled(UInt(condSize.W))))
  val dOut = IO(Decoupled(UInt(p.width.W)))

  val dataOut = if (p.width > 0) {
    val dataMux = Module(
      MuxConfig(
        MuxConfigParams(
          p.width,
          minNum,
          0,
          0,
          recursivelyExtendPrefix = p.recursivelyExtendPrefix
        ),
        Mx(p).libName + "_" + prefix
      )
    )
    dataMux.dIn := dIn.map(_.bits)
    dataMux.conf := condIn.bits
    dataMux.dOut
  } else {
    DontCare
  }

  val validMux = Module(
    MuxConfig(
      MuxConfigParams(
        1,
        minNum,
        0,
        0,
        recursivelyExtendPrefix = p.recursivelyExtendPrefix
      ),
      Mx(p).libName + "_" + prefix
    )
  )
  validMux.dIn := dIn.map(_.valid & condIn.valid)
  validMux.conf := condIn.bits
  val validOut = validMux.dOut

  // val dataOut = MuxLookup(condIn.bits, dIn(0).bits)((0 until p.num).map(i => i.U -> dIn(i).bits))
  // val validOut =  MuxLookup(condIn.bits, dIn(0).valid)((0 until p.num).map(i => i.U -> (dIn(i).valid & condIn.valid)))

  val tehb = Module(
    TEHB(
      TEHBParams(
        p.width,
        1,
        recursivelyExtendPrefix = p.recursivelyExtendPrefix
      ),
      Mx(p).libName + "_" + prefix
    )
  )

  tehb.dOut <> dOut
  tehb.dIn.bits := dataOut
  tehb.dIn.valid := validOut

  val readyOut = Wire(Bool())
  readyOut := tehb.dIn.ready

  (0 until minNum).foreach(i =>
    dIn(i).ready := (condIn.bits === i.U) && condIn.valid && readyOut
  )
  condIn.ready := dOut.valid && readyOut
}

object MuxGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Mux(MuxParams(1, 4)))
    println(v)
  }
}
