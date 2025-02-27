package components

import arch.{Mux => AMux, _}
import archs.{
  Merge => Mg,
  MergeStaticInfo => Mgi,
  MergeParams,
  TEHBParams,
  Params
}

import chisel3._
import chisel3.util.DecoupledIO
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import chisel3.util.MuxCase
import circt.stage.ChiselStage

object Merge extends ChiselComponent[MergeParams] {
  def wantedConfigurations: List[MergeParams] = {
    MergeParams(32, 2) :: MergeParams(1, 2) :: MergeParams(0, 2) :: Nil
  }

  def apply(p: MergeParams): Merge = {
    new Merge(p)
  }

  def unpackParams(moduleName: String): MergeParams = {
    assert(moduleName.startsWith(Mgi.typeString))

    val params =
      moduleName.replace(Mgi.typeString, "").replace("_optimized", "")
    Mgi.unpackLibRep(params)
  }

  val clocked = Mgi.clocked

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: MergeParams,
      word: Int
  ): PortInstance = {
    portName match {
      case "dIn" =>
        PortInstance(
          BlockPortID(p.width, flipped(PTInput, hsType), pmD, Regular),
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

  def emitVerilog(p: MergeParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new Merge(p))
    val moduleName = Mg(p).libName

    (v, moduleName)
  }
}

class Merge_notehb(p: MergeParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Mg(p).libName

  val dIn = IO(Flipped(Vec(p.num, Decoupled(UInt(p.width.W)))))
  val dOut = IO(Decoupled(UInt(p.width.W)))

  def muxHorizontal(vals: List[(UInt, Bool)]): List[(UInt, Bool)] = {
    val even = vals.zipWithIndex.filter(_._2 % 2 == 0)
    val odd = vals.zipWithIndex.filter(_._2 % 2 == 1)

    val muxes = even.map(_._1).zip(odd.map(_._1)).map { case (p0, p1) =>
      (Mux(p0._2, p0._1, p1._1), Mux(p0._2, p0._2, p1._2))
    }

    val out = if (even.size < odd.size) {
      odd.last._1 :: muxes
    } else if (even.size > odd.size) {
      even.last._1 :: muxes
    } else {
      muxes
    }

    if (out.size == 1) {
      out
    } else {
      muxHorizontal(out)
    }
  }

  val (dataOut, validOut) = muxHorizontal(
    dIn.map(d => (d.bits, d.valid)).toList
  ).head

  dOut.bits := dataOut
  dOut.valid := validOut
  dIn.map(_.ready).foreach(_ := dOut.ready)
}

class Merge(p: MergeParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Mg(p).libName

  val dIn = IO(Flipped(Vec(p.num, Decoupled(UInt(p.width.W)))))
  val dOut = IO(Decoupled(UInt(p.width.W)))

  val merge_notb = Module(
    new Merge_notehb(
      MergeParams(
        p.width,
        p.num,
        recursivelyExtendPrefix = p.recursivelyExtendPrefix
      ),
      Mg(p).libName + "_" + prefix
    )
  )

  val tehb = Module(
    TEHB(
      TEHBParams(
        p.width,
        1,
        recursivelyExtendPrefix = p.recursivelyExtendPrefix
      ),
      Mg(p).libName + "_" + prefix
    )
  )

  dIn.zip(merge_notb.dIn).foreach {
    case (d, m) => {
      d <> m
    }
  }

  tehb.dIn.bits := merge_notb.dOut.bits
  tehb.dIn.valid := merge_notb.dOut.valid
  merge_notb.dOut.ready := tehb.dIn.ready

  tehb.dOut <> dOut
}

object MergeGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Merge(MergeParams(32, 4)))
    println(v)
  }
}
