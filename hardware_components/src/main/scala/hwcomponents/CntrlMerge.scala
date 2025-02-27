package components

import arch.{Mux => AMux, _}
import archs.{
  CntrlMerge => CMerge,
  CntrlMergeStaticInfo => CMergei,
  CntrlMergeParams,
  TEHBParams,
  ForkParams,
  MergeParams,
  Params
}

import chisel3._
import chisel3.util.DecoupledIO
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import circt.stage.ChiselStage
import archs.ForkVariants
import archs.{EagerFork => EF}

object CntrlMerge extends ChiselComponent[CntrlMergeParams] {
  def wantedConfigurations: List[CntrlMergeParams] =
    CntrlMergeParams(2) :: CntrlMergeParams(
      3,
      indexWidth = 2
    ) :: CntrlMergeParams(4, indexWidth = 2) :: Nil

  val clocked = CMergei.clocked

  def unpackParams(moduleName: String): CntrlMergeParams = {
    assert(moduleName.startsWith(CMergei.typeString))

    val params =
      moduleName.replace(CMergei.typeString, "").replace("_optimized", "")
    CMergei.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: CntrlMergeParams,
      word: Int
  ): PortInstance = {
    portName match {
      case "bbIn" =>
        PortInstance(
          BlockPortID(0, flipped(PTInput, hsType), pmD, Regular),
          word,
          hsType
        )
      case "bb" =>
        PortInstance(
          BlockPortID(0, flipped(PTOutput, hsType), pmD, Regular),
          0,
          hsType
        )
      case "from" =>
        PortInstance(
          BlockPortID(1, flipped(PTOutput, hsType), pmC, Regular),
          0,
          hsType
        )
    }
  }

  def emitVerilog(p: CntrlMergeParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new CntrlMerge(p))
    val moduleName = CMerge(p).libName

    (v, moduleName)
  }
}

class CntrlMerge(p: CntrlMergeParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + CMerge(p).libName

  assert(p.indexWidth == log2Ceil(p.num))

  val bbIn = IO(Flipped(Vec(p.num, Decoupled(UInt(0.W))))) // aka ins
  val bb = IO((Decoupled(UInt(0.W)))) // aka outs
  val from = IO((Decoupled(UInt(p.indexWidth.W)))) // aka index

  val merge = Module(
    new Merge_notehb(
      MergeParams(
        log2Ceil(p.num),
        p.num,
        recursivelyExtendPrefix = p.recursivelyExtendPrefix
      ),
      CMerge(p).libName + "_" + prefix
    )
  )

  val tehb = Module(
    TEHB(
      TEHBParams(
        log2Ceil(p.num),
        1,
        recursivelyExtendPrefix = p.recursivelyExtendPrefix
      ),
      CMerge(p).libName + "_" + prefix
    )
  )

  val fork = Module(
    Fork(
      ForkParams(
        0,
        2,
        EF,
        recursivelyExtendPrefix = p.recursivelyExtendPrefix
      ),
      CMerge(p).libName + "_" + prefix
    )
  )

  merge.dIn.zip(bbIn).zipWithIndex.foreach {
    case ((m, b), idx) => {
      m.valid := b.valid
      b.ready := m.ready
      m.bits := idx.U
    }
  }

  merge.dOut.ready := tehb.dIn.ready
  tehb.dIn.bits := merge.dOut.bits
  tehb.dIn.valid := merge.dOut.valid

  tehb.dOut.ready := fork.dIn.ready
  fork.dIn.valid := tehb.dOut.valid
  fork.dIn.bits := DontCare

  fork.dOut(1).ready := bb.ready
  bb.valid := fork.dOut(1).valid
  bb.bits := DontCare

  fork.dOut(0).ready := from.ready

  from.valid := fork.dOut(0).valid 
  from.bits := tehb.dOut.bits
}

object CntrlMergeGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new CntrlMerge(CntrlMergeParams(2)))
    println(v)
  }
}
