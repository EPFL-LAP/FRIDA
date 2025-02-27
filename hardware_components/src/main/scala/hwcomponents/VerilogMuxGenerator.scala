package components

import frontend.GlobalParamsInst
import frontend.GlobalParams
import util.Util
import chisel3.util.log2Ceil

object VerilogMuxGenerator {
  def genModule(muxIns: Int): String = {
    val condSize = log2Ceil(muxIns)

    val cond = if (condSize == 1) {
      ""
    } else {
      "[" + (condSize - 1) + ":0]"
    }

    "module Mux" + muxIns + "to1(\n" +
      "  input [" + (muxIns - 1) + ":0] d,\n" +
      "  input " + cond + " s,\n" +
      "  output  out\n" +
      ");\n" +
      "assign out = d[s];\n" +
      "endmodule"
  }

  // TODO path is harcoded here, need to have something like GlobalParamsInst here too
  def apply(sizes: List[Int]): Unit = {
    sizes.map { muxSize =>
      {
        val fName =
          GlobalParams.root + "/build/hardware/muxs/Mux" + muxSize + "to1.v"

        val f = Util.writeOpen(fName)
        f.write(genModule(muxSize))
        f.close()
      }
    }
  }
}
