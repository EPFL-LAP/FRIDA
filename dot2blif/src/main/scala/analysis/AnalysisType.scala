package analysis

import core.Analysis
import frontend.GlobalParamsInst
import crkt.ElasticGraph
import crkt.Node
import crkt.TNode
import arch._
import util.Util
import collection.mutable.{Map => MMap}

case class AnalysisType(val storage: MMap[String, Int]) extends Analysis {
  def aggregate(g: ElasticGraph): Unit = {
    g.nodes
      .map(_._2)
      .map(
        n => (n.typeStr)
      )
      .groupBy(
        t => t
      )
      .map(
        (t, ts) => (t, ts.size)
      )
      .foreach {
        (t, num) =>
          {
            val inStorage = storage.getOrElse(t, 0)
            storage += (t -> (num + inStorage))
          }
      }
  }

  def print()(implicit params: GlobalParamsInst): Unit = {
    val fname = getFName()

    val columns = "type, count"
    val csv = storage
      .map(
        (k, v) => (k + ", " + v)
      )
      .mkString("\n")

    val f = Util.writeOpen(fname)
    f.write(columns + "\n")
    f.write(csv + "\n")
    f.close()
  }
}
