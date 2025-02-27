package analysis

import frontend.GlobalParamsInst
import io.Source

object ParsePerformanceMetrics {
  def apply(params: GlobalParamsInst): Unit = {
    val setupTimingFName = params.buildDir + "/report_timing.setup.rpt"
    val delays = Source.fromFile(setupTimingFName).getLines().filter(_ contains "slack").map {
      sLine =>
        {
          sLine.filter(
            c => c.isDigit || c == '.'
          )
        }
    }

    println(delays.toList.head)
  }
}
