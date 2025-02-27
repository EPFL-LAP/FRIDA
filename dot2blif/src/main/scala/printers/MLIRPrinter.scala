package printers

import util.Util
import mlir.GenericOp

import io.AnsiColor._

object MLIRPrinter {
  def apply(fName: String, op: GenericOp): Unit = {
    println(YELLOW + "Printing: " + fName + RESET)

    val f = Util.writeOpen(fName)
    f.write(op.str(0))
    f.close()
  }
}
