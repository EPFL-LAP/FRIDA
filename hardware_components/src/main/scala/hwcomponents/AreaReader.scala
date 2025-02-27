package components

case class Area(
    instName: String,
    instCount: Double,
    total: Double,
    buffer: Double,
    inverted: Double,
    comb: Double,
    flop: Double,
    latch: Double,
    clockGate: Double,
    macr: Double,
    physical: Double,
    subComps: List[Area]
)

object AreaReader {
  def parseLine(line: String, subComps: List[Area]): Area = {
    val values = line.split("\\s+")

    val (instName, offset) =
      if (values(1).forall(_.isDigit)) (values(0), 0) else (values(1), 1)

    Area(
      instName,
      values(1 + offset).toDouble,
      values(2 + offset).toDouble,
      values(3 + offset).toDouble,
      values(4 + offset).toDouble,
      values(5 + offset).toDouble,
      values(6 + offset).toDouble,
      values(7 + offset).toDouble,
      values(8 + offset).toDouble,
      values(9 + offset).toDouble,
      values(10 + offset).toDouble,
      subComps
    )
  }

  def apply(moduleName: String, fname: String): Area = {
    val it = scala.io.Source.fromFile(fname).getLines()

    // Skips header and seperator
    it.next()
    it.next()

    def rec(it: Iterator[String]): List[Area] = {
      if (!it.hasNext) {
        Nil
      } else {
        val line = it.next()
        val subComps = rec(it)
        parseLine(line.trim(), subComps) :: subComps
      }
    }

    rec(it).head
  }
}
