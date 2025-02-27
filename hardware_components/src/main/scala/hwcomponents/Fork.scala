package components

import arch.{Mux => AMux, _}
import archs.{Fork => Frk, ForkStaticInfo => Frki, ForkParams, Params}

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.DecoupledIO
import circt.stage.ChiselStage
import archs.ForkVariants
import archs.{EagerFork => EF}
import archs.{LazyFork => LF}
import archs.{SwitchableFork => SF}

object Fork extends ChiselComponent[ForkParams] {
  def wantedConfigurations: List[ForkParams] = {
    Frki.defaultConfigs
  }

  def apply(p: ForkParams, prefix: String = ""): Fork = {
    new Fork(p, prefix)
  }

  val clocked = Frki.clocked

  def unpackParams(moduleName: String): ForkParams = {
    assert(moduleName.startsWith(Frki.typeString))

    val params =
      moduleName.replace(Frki.typeString, "").replace("_optimized", "")
    Frki.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: ForkParams,
      word: Int
  ): PortInstance = {
    portName match {
      case "dIn" =>
        PortInstance(
          BlockPortID(p.width, flipped(PTInput, hsType), pmD, Regular),
          0,
          hsType
        )
      case "dOut" =>
        PortInstance(
          BlockPortID(p.width, flipped(PTOutput, hsType), pmD, Regular),
          word,
          hsType
        )
    }
  }

  def emitVerilog(p: ForkParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new Fork(p))
    val moduleName = Frk(p).libName

    (v, moduleName)
  }

  def defaultDelays(p: ForkParams): String = {
    val zd = "0.0e-12"

    assert(p.width == 0)
    assert(p.num == 1)

    Frk(p).libName + "_optimized(0) { } : [\n" +
      "(0, In, PMData, Reg)[0].v -> (0, Out, PMData, Reg)[0].v : " + "{ minDelay=\"" + zd + "\", maxDelay=\"" + zd + "\" },\n" +
      "(0, Out, PMData, Reg)[0].r -> (0, In, PMData, Reg)[0].r : " + "{ minDelay=\"" + zd + "\", maxDelay=\"" + zd + "\" }" +
      "]\n"
  }
}

class EagerForkHSLogic(
  recursivelyExtendPrefix: Boolean = false,
  prefix: String = ""
) extends Module {
  override val desiredName = {
    if (recursivelyExtendPrefix) prefix else ""
  } + super.desiredName

  val notValidOrAllReady = IO(Input(Bool()))
  val dIn = IO(Flipped(Decoupled(UInt(0.W))))
  val dOut = IO(Decoupled(UInt(0.W)))

  val enToken = Wire(Bool())

  val stalled = ~dOut.ready & enToken
  enToken := RegNext(notValidOrAllReady | stalled, true.B)

  dOut.valid := dIn.valid & enToken
  dIn.ready := ~stalled

  dOut.bits := DontCare
  dIn.bits := DontCare
}

class EagerFork(
  num: Int,
  recursivelyExtendPrefix: Boolean = false,
  prefix: String = ""
) extends Module {
  override val desiredName = {
    if (recursivelyExtendPrefix) prefix else ""
  } + super.desiredName

  val dIn = IO(Flipped(Decoupled(UInt(0.W))))
  val dOut = IO(Vec(num, Decoupled(UInt(0.W))))

  dOut.foreach(_.bits := DontCare)

  val rememberLogic = (0 until num).map(_ =>
    Module(
      new EagerForkHSLogic(
        recursivelyExtendPrefix,
        "EagerFork_N" + num + "_" + prefix
      )
    )
  )

  val allReady = rememberLogic.map(_.dIn.ready).reduce(_ & _)
  dIn.ready := allReady

  rememberLogic.zipWithIndex.foreach {
    case (eagerForkHS: EagerForkHSLogic, i: Int) => {
      eagerForkHS.notValidOrAllReady := allReady | ~dIn.valid
      eagerForkHS.dIn.valid := dIn.valid
      eagerForkHS.dOut.ready := dOut(i).ready
      dOut(i).valid := eagerForkHS.dOut.valid

      eagerForkHS.dOut.bits := DontCare
      eagerForkHS.dIn.bits := DontCare
    }
  }
}

class LazyFork(
  num: Int,
  recursivelyExtendPrefix: Boolean = false,
  prefix: String = ""
) extends Module {
  override val desiredName = {
    if (recursivelyExtendPrefix) prefix else ""
  } + super.desiredName

  val dIn = IO(Flipped(Decoupled(UInt(0.W))))
  val dOut = IO(Vec(num, Decoupled(UInt(0.W))))

  val outReadies = dOut.map(_.ready)
  val allNReady = outReadies.reduce(_ & _)
  dIn.ready := allNReady

  val tmpReady = Wire(Vec(num, Bool()))

  tmpReady.zipWithIndex.map { case (tmpReadyI, idx) =>
    tmpReadyI := (0 until num)
      .filterNot(_ == idx)
      .map(outReadies(_))
      .reduceOption(_ & _)
      .fold(true.B)(i => i)
  }

  dOut.zipWithIndex.foreach { case (o, idx) =>
    o.valid := tmpReady(idx) & dIn.valid
  }

  dOut.foreach(_.bits := DontCare)
}

class Fork(p: ForkParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Frk(p).libName

  val dIn = IO(Flipped(Decoupled(UInt(p.width.W))))
  val dOut = IO(Vec(p.num, Decoupled(UInt(p.width.W))))

  val selectMode = if(p.variant == SF) {
    Some(IO(Input(Bool())))
  } else {
    None
  }

  val sel = Wire(Bool())
  sel := selectMode.fold((p.variant == LF).B)(i => i)

  when(sel) {
    val logic = Module(
      new LazyFork(
        p.num,
        p.recursivelyExtendPrefix,
        Frk(p).libName + "_" + prefix
      )
    )

    logic.dIn.valid := dIn.valid
    dIn.ready := logic.dIn.ready

    logic.dOut.zip(dOut).foreach{ case (lOuti, dOuti) => { lOuti.ready := dOuti.ready ; dOuti.valid := lOuti.valid }}

    logic.dIn.bits := DontCare
    logic.dOut.map(_.bits).foreach(_ := DontCare)
  } .otherwise {
    val logic = Module(
      new EagerFork(
        p.num,
        p.recursivelyExtendPrefix,
        Frk(p).libName + "_" + prefix
      )
    )

    logic.dIn.valid := dIn.valid
    dIn.ready := logic.dIn.ready

    logic.dOut.zip(dOut).foreach{ case (lOuti, dOuti) => { lOuti.ready := dOuti.ready ; dOuti.valid := lOuti.valid }}

    logic.dIn.bits := DontCare
    logic.dOut.map(_.bits).foreach(_ := DontCare)
  }

  dOut.map(_.bits).foreach(_ := dIn.bits)
}

object ForkGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Fork(ForkParams(0, 4, EF)))
    println(v)
  }
}
