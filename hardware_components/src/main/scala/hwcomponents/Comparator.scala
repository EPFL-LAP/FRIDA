package components

import java.io._

import arch.{Mux => AMux, _}
import archs.{
  Comparator,
  ComparatorOperation,
  ComparatorStaticInfo => Cmpi,
  ComparatorParams,
  JoinParams,
  Params,
  CmpUlt,
  CmpUle,
  CmpSlt,
  CmpSle,
  CmpEq,
  CmpNe,
  CmpUgt,
  CmpUge,
  CmpSgt,
  CmpSge,
  CmpAnyComp
}

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Reverse
import chisel3.util.log2Ceil
import chisel3.util.Cat
import chisel3.util.MuxCase
import circt.stage.ChiselStage

object Icmp extends ChiselComponent[ComparatorParams] {
  def wantedConfigurations: List[ComparatorParams] = Cmpi.defaultConfigs

  def apply(p: ComparatorParams): Icmp = {
    new Icmp(p)
  }

  def unpackParams(moduleName: String): ComparatorParams = {
    assert(moduleName.startsWith(Cmpi.typeString))

    val params =
      moduleName.replace(Cmpi.typeString, "").replace("_optimized", "")
    Cmpi.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: ComparatorParams,
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
      case "condOut" =>
        PortInstance(
          BlockPortID(1, flipped(PTOutput, hsType), pmC, Regular),
          0,
          hsType
        )
      case "conf" =>
        PortInstance(BlockPortID(5, PTInput, pmConf, Regular), 0, None)
    }
  }

  val clocked = Cmpi.clocked

  def emitVerilog(p: ComparatorParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new Icmp(p))
    val moduleName = Comparator(p).libName

    (v, moduleName)
  }
}

object IcmpOps {
  val CONFIG_BITS = 4

  val ugt = 0.U(CONFIG_BITS.W)
  val uge = 1.U(CONFIG_BITS.W)
  val sgt = 2.U(CONFIG_BITS.W)
  val sge = 3.U(CONFIG_BITS.W)
  val ult = 4.U(CONFIG_BITS.W)
  val ule = 5.U(CONFIG_BITS.W)
  val slt = 6.U(CONFIG_BITS.W)
  val sle = 7.U(CONFIG_BITS.W)
  val eq = 8.U(CONFIG_BITS.W)
  val ne = 9.U(CONFIG_BITS.W)
  val undef = 15.U(CONFIG_BITS.W)

  // mapping: submodules/dynamatic/build/include/dynamatic/Dialect/Handshake/Handshake.cpp.inc:3059

  def apply(cmp: ComparatorOperation) = cmp match {
    case CmpUgt     => ugt
    case CmpUge     => uge
    case CmpSgt     => sgt
    case CmpSge     => sge
    case CmpUlt     => ult
    case CmpUle     => ule
    case CmpSlt     => slt
    case CmpSle     => sle
    case CmpEq      => eq
    case CmpNe      => ne
    case CmpAnyComp => undef
  }
}

class Icmp(p: ComparatorParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Comparator(p).libName

  val a = IO(Flipped(Decoupled(UInt(p.width.W))))
  val b = IO(Flipped(Decoupled(UInt(p.width.W))))
  val condOut = IO(Decoupled(Bool()))
  val confType = UInt(4.W)
  val currentOp = Wire(confType)
  val desiredOp = IcmpOps(p.predicate)
  val isConfigurableOp = p.predicate == CmpAnyComp
  val conf = {
    if (isConfigurableOp) {
      Some(IO(Input(confType)))
    } else {
      None
    }
  }

  currentOp := {
    if (isConfigurableOp) {
      // did this way to prevent io prefix for conf
      conf.get
    } else {
      desiredOp
    }
  }

  val compareTypeOps = currentOp(2)
  val compareSignOps = currentOp(1)
  val compareEqOps = currentOp(0)

  val aToUnsigned = Wire(UInt(p.width.W))
  aToUnsigned := Mux(
    compareSignOps,
    if (p.width >= 2) Cat(~a.bits(p.width - 1), a.bits(p.width - 2, 0))
    else ~a.bits(0),
    a.bits
  )
  val bToUnsigned = Wire(UInt(p.width.W))
  bToUnsigned := Mux(
    compareSignOps,
    if (p.width >= 2) Cat(~b.bits(p.width - 1), b.bits(p.width - 2, 0))
    else ~b.bits(0),
    b.bits
  )

  val sum = aToUnsigned -& bToUnsigned
  val sum_cf = sum(p.width)

  val xor = a.bits ^ b.bits

  val cmpEq = xor.orR
  val eq = ~cmpEq
  val ne = cmpEq

  val cmp = Mux(
    compareTypeOps,
    sum_cf, // sum<0 means a<b
    ~sum_cf
  )

  // note || vs &&, || because <= => means allowed to be equal, < > means must not be equal
  val cmpWithEq = Mux(compareEqOps, cmp || eq, cmp && ~eq)

  val isEq = (currentOp === IcmpOps.eq)
  val isNe = (currentOp === IcmpOps.ne)

  condOut.bits := Mux(isEq, eq, Mux(isNe, ne, cmpWithEq))

  val outValid = Wire(Bool())
  val outReady = Wire(Bool())

  val join = Module(
    Join(
      JoinParams(2, recursivelyExtendPrefix = p.recursivelyExtendPrefix),
      Comparator(p).libName + "_" + prefix
    )
  )

  join.dIn(0).valid := a.valid
  join.dIn(1).valid := b.valid

  a.ready := join.dIn(0).ready
  b.ready := join.dIn(1).ready

  outValid := join.dOut.valid
  join.dOut.ready := outReady

  condOut.valid := outValid
  outReady := condOut.ready

  join.dIn(0).bits := DontCare
  join.dIn(1).bits := DontCare
}

object ComparatorGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Icmp(ComparatorParams(32, CmpAnyComp)))
    println(v)
  }
}
