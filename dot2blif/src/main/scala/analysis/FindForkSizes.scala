package analysis

import crkt._
import archs.Fork
import core.Analysis
import arch.PTOutput
import frontend.GlobalParamsInst
import util.Util
import collection.mutable.{Map => MMap}

case class FindForkSizes(val storage: MMap[Int, Int]) extends Analysis {
  def aggregate(g: ElasticGraph): Unit = {
    g.nodes
      .map(_._2)
      .filter(_.nType == Fork)
      .map(_.ports.filter(_._1.pt == PTOutput).head._2.size)
      .groupBy(
        i => i
      )
      .map(
        (s, sizes) => (s, sizes.size)
      )
      .foreach {
        (size, num) =>
          {
            val inStorage = storage.getOrElse(size, 0)
            storage += (size -> (num + inStorage))
          }
      }
  }

  def print()(implicit params: GlobalParamsInst): Unit = {
    val fName = getFName()
    val f = Util.writeOpen(fName)

    f.write("fork size, num forks\n")
    f.write(
      storage
        .map(
          (s, num) => ("" + s + ", " + num)
        )
        .mkString("\n")
    )
    f.close()
  }
}
