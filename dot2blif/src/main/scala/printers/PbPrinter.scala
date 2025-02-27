package printers

import arch.PbType

object PbPrinter {
  def apply(pb: PbType, name: String): Unit = {
    pb.subBlocks.map(PbPrinter(_, name))

    if (pb.name == name) {
      println(pb)
    }
  }
}
