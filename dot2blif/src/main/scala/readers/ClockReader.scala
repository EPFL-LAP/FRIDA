package readers

import scala.io.Source

case class ClockInfo(period: Double, unit: String)

object ClockReader {
  def apply(fname: String): ClockInfo = {
    val clkString = Source.fromFile(fname).getLines().next()
    val clk = clkString
      .filter(
        c => c.isDigit || (c == '.')
      )
      .toDouble
    val unit = clkString.filter(
      c => !c.isDigit && (c != '.')
    )

    ClockInfo(clk, unit)
  }
}
