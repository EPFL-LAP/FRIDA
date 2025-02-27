package core

import arch.TBlock

import io.AnsiColor._

// TODO Blocks here is not great.. Refactor
trait IRPass[T] {
  def apply(ir: T, blocks: Map[String, TBlock]): T
}

object IRPassManager {
  def apply[T](passes: List[IRPass[T]], blocks: Map[String, TBlock], ir: T): T = {
    if (passes.isEmpty) {
      ir
    } else {
      println(CYAN + passes.head.getClass.getName + RESET)
      apply(passes.tail, blocks, passes.head(ir, blocks))
    }
  }
}
