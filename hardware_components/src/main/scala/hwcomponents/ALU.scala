package components

import java.io._

import arch.{Mux => AMux, _}
import archs.{
  Operator,
  OperatorStaticInfo => Opi,
  OperatorParams,
  JoinParams,
  Params,
  ALUOperation
}

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Reverse
import chisel3.util.log2Ceil
import chisel3.util.Cat
import chisel3.util.MuxCase
import chisel3.util.RegEnable
import circt.stage.ChiselStage

object ALU extends ChiselComponent[OperatorParams] {
  def wantedConfigurations: List[OperatorParams] = Opi.defaultConfigs

  def apply(p: OperatorParams): ALU = {
    new ALU(p)
  }

  def unpackParams(moduleName: String): OperatorParams = {
    assert(moduleName.startsWith(Opi.typeString))

    val params = moduleName.replace(Opi.typeString, "").replace("_optimized", "")
    Opi.unpackLibRep(params)
  }

  def portNameToPortInstance(
    portName: String,
    hsType: Option[HSType],
    p: OperatorParams,
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

  val clocked = Opi.clocked

  def emitVerilog(p: OperatorParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new ALU(p))
    val moduleName = Operator(p).libName

    (v, moduleName)
  }
}

object AluOps {
  val CONFIG_BITS = 5

  val add = 0.U(CONFIG_BITS.W)
  val and = 1.U(CONFIG_BITS.W)
  val or = 2.U(CONFIG_BITS.W)
  val shl = 3.U(CONFIG_BITS.W)
  val xor = 4.U(CONFIG_BITS.W)
  val ashr = 5.U(CONFIG_BITS.W)
  val lshr = 6.U(CONFIG_BITS.W)

  val ugt = 8.U(CONFIG_BITS.W)
  val uge = 9.U(CONFIG_BITS.W)
  val sgt = 10.U(CONFIG_BITS.W)
  val sge = 11.U(CONFIG_BITS.W)

  val ult = 12.U(CONFIG_BITS.W)
  val ule = 13.U(CONFIG_BITS.W)
  val slt = 14.U(CONFIG_BITS.W)
  val sle = 15.U(CONFIG_BITS.W)

  val sub = 16.U(CONFIG_BITS.W)
  val eq = 17.U(CONFIG_BITS.W)
  val ne = 18.U(CONFIG_BITS.W)

  val undef = 31.U(CONFIG_BITS.W)

  def apply(op: ALUOperation) = op match {
    case ALUOperation.add   => add
    case ALUOperation.and   => and
    case ALUOperation.or    => or
    case ALUOperation.shl   => shl
    case ALUOperation.xor   => xor
    case ALUOperation.ashr  => ashr
    case ALUOperation.shrsi => ashr
    case ALUOperation.shrui => lshr
    case ALUOperation.lshr  => lshr
    case ALUOperation.ugt   => ugt
    case ALUOperation.uge   => uge
    case ALUOperation.sgt   => sgt
    case ALUOperation.sge   => sge
    case ALUOperation.ult   => ult
    case ALUOperation.ule   => ule
    case ALUOperation.slt   => slt
    case ALUOperation.sle   => sle
    case ALUOperation.sub   => sub
    case ALUOperation.eqop    => eq
    case ALUOperation.neop    => ne
    case ALUOperation.anyop => undef
  }
}

class BigALU(p: OperatorParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + super.desiredName

  assert(p.width > 1)

  val a = IO(Flipped(Decoupled(UInt(p.width.W))))
  val b = IO(Flipped(Decoupled(UInt(p.width.W))))
  val res = IO(Decoupled(UInt(p.width.W)))
  val condOut = IO(Decoupled(Bool()))
  val conf = IO(Input(UInt(5.W)))

  val aSign = a.bits(p.width - 1)
  val bSign = b.bits(p.width - 1)
  val sameSign = aSign === bSign

  val subOps = conf(3) || conf(4)

  val sum = Wire(UInt((p.width + 1).W))

  val op_b = (~b.bits)
  sum := Cat(0.U, a.bits) + Mux(subOps, Cat(0.U, op_b) + 1.U, Cat(0.U, b.bits))
  val sumSign_sf = sum(p.width - 1)
  val sum_zf = (sum(p.width - 1, 0) === 0.U)
  val sum_cf = sum(p.width)
  val vf = a.bits(p.width - 1) ^ (op_b(p.width - 1)) ^ sum(p.width - 1) ^ sum_cf

  val compareTypeOps = conf(2)
  val compareSignOps = conf(1)
  val compareEqOps = conf(0)

  val xor = a.bits ^ b.bits
  val and = a.bits & b.bits
  val or = a.bits | b.bits

  val cmpEq = xor.orR
  val eq = ~cmpEq
  val ne = cmpEq

  val cmp = Mux(
    compareTypeOps,
    Mux( // less than
      compareSignOps,
      Mux( // signed or unsigend
        compareEqOps,
        (sumSign_sf =/= vf) || eq,
        (sumSign_sf =/= vf)
      ),
      Mux(compareEqOps, !sum_cf || eq, !sum_cf)
    ),
    Mux( // greater than
      compareSignOps,
      Mux( // signed or unsigend
        compareEqOps,
        (sumSign_sf === vf),
        !((sumSign_sf =/= vf) || eq)
      ),
      Mux(
        compareEqOps,
        sum_cf,
        (sum_cf && !eq)
      )
    )
  )

  val shamt = b.bits(log2Ceil(p.width), 0).asUInt
  val arithmetic = conf(1)
  val right = conf(2)
  val shin = Mux(right, a.bits, Reverse(a.bits))

  val shiftr = (Cat(!arithmetic && shin(p.width - 1), shin).asSInt >> shamt)(
    p.width - 1,
    0
  )
  val shiftl = Reverse(shiftr)

  val addSub = (conf === AluOps.add) || (conf === AluOps.sub)
  val cmplg = ((conf >= AluOps.ugt) && (conf <= AluOps.sle))
  val sr = ((conf === AluOps.ashr) || (conf === AluOps.lshr))
  val sl = (conf === AluOps.shl)
  val isAnd = (conf === AluOps.and)
  val isOr = (conf === AluOps.or)
  val isXor = (conf === AluOps.xor)
  val isEq = (conf === AluOps.eq)
  val isNe = (conf === AluOps.ne)

  res.bits := MuxCase(
    sum,
    IndexedSeq(
      (conf === AluOps.add) -> sum,
      (conf === AluOps.and) -> and,
      (conf === AluOps.or) -> or,
      (conf === AluOps.shl) -> shiftl,
      (conf === AluOps.xor) -> xor,
      (conf === AluOps.ashr) -> shiftr,
      (conf === AluOps.lshr) -> shiftr,
      (conf === AluOps.sub) -> sum
    )
  )

  condOut.bits := Mux(isEq, eq, Mux(isNe, ne, cmp))

  val outValid = Wire(Bool())
  val outReady = Wire(Bool())

  val join = Module(
    Join(
      JoinParams(2, recursivelyExtendPrefix = p.recursivelyExtendPrefix),
      desiredName + "_" + prefix
    )
  )
  join.dIn(0).valid := a.valid
  join.dIn(1).valid := b.valid
  a.ready := join.dIn(0).ready
  b.ready := join.dIn(1).ready
  outValid := join.dOut.valid
  join.dOut.ready := outReady

  join.dIn(0).bits := DontCare
  join.dIn(1).bits := DontCare

  val condActive = cmplg || isEq || isNe
  // val resActive = addSub || sr || sl || isAnd || isOr || isXor
  condOut.valid := Mux(condActive, outValid, 0.B)
  res.valid := Mux(!condActive, outValid, 0.B)

  outReady := Mux(condActive, condOut.ready, res.ready)
}

class SmallALU(p: OperatorParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + super.desiredName
  assert(p.width == 1)

  val a = IO(Flipped(Decoupled(UInt(p.width.W))))
  val b = IO(Flipped(Decoupled(UInt(p.width.W))))
  val res = IO(Decoupled(UInt(p.width.W)))
  val conf = IO(Input(UInt(5.W)))

  val subOps = conf(3) || conf(4)

  val sum = Wire(UInt((p.width + 1).W))

  val op_b = (~b.bits)
  sum := Cat(0.U, a.bits) + Mux(subOps, Cat(0.U, op_b) + 1.U, Cat(0.U, b.bits))

  val xor = a.bits ^ b.bits
  val and = a.bits & b.bits
  val or = a.bits | b.bits

  val join = Module(
    Join(
      JoinParams(2, recursivelyExtendPrefix = p.recursivelyExtendPrefix),
      desiredName + "_" + prefix
    )
  )

  join.dIn(0).valid := a.valid
  join.dIn(1).valid := b.valid
  a.ready := join.dIn(0).ready
  b.ready := join.dIn(1).ready
  res.valid := join.dOut.valid
  join.dOut.ready := res.ready

  join.dIn(0).bits := DontCare
  join.dIn(1).bits := DontCare

  res.bits := MuxCase(
    sum,
    IndexedSeq(
      (conf === AluOps.add) -> sum,
      (conf === AluOps.and) -> and,
      (conf === AluOps.or) -> or,
      (conf === AluOps.xor) -> xor,
      (conf === AluOps.sub) -> sum
    )
  )
}

class ALU(p: OperatorParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Operator(p).libName

  val a = IO(Flipped(Decoupled(UInt(p.width.W))))
  val b = IO(Flipped(Decoupled(UInt(p.width.W))))
  val res = IO(Decoupled(UInt(p.width.W)))

  val condOut = if (p.withCmp) {
    Some(IO(Decoupled(Bool())))
  } else None

  // Instantiate the proper operation when needed
  // Needed for the HLS verifier
  val confType = UInt(5.W)
  val conf = if (p.op == ALUOperation.anyop) Some(IO(Input(confType))) else None
  val confData = Wire(confType)
  confData := conf.fold(AluOps(p.op))(i => i)

  if (p.width == 1) {
    val alu = Module(new SmallALU(p, Operator(p).libName + "_" + prefix))
    alu.a <> a
    alu.b <> b
    alu.conf := confData
    res <> alu.res
    if (p.withCmp) {
      condOut.get.valid := DontCare
      condOut.get.bits := DontCare
      condOut.get.ready := DontCare
    }
  } else {
    val alu = Module(new BigALU(p, Operator(p).libName + "_" + prefix))
    alu.a <> a
    alu.b <> b
    alu.conf := confData
    res <> alu.res
    if (p.withCmp) {
      condOut.get <> alu.condOut
    } else {
      alu.condOut <> DontCare
    }
  }
}

object AluGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(
      new ALU(OperatorParams(1, ALUOperation.anyop))
    )
    println(v)
  }
}
