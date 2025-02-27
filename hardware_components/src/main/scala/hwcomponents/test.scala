package components

import chisel3._
import chisel3.util.Decoupled
import circt.stage.ChiselStage

class Test extends Module {
  val in = IO(Flipped(Decoupled(Bool())))
  val out = IO(Decoupled(Bool()))

  in <> out
}

object TestGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Test)
    println(v)
  }
}
