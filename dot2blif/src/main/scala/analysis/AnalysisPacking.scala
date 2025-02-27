package analysis

import frontend.GlobalParamsInst
import crkt.ElasticGraph
import packerv2.MappedMolecule
import packerv2.Molecule
import util.Util
import arch.PTOutput

import io.AnsiColor._

object AnalysisPacking {
  def reportExternalEdges(
      params: GlobalParamsInst,
      g: ElasticGraph,
      packed: List[MappedMolecule[Molecule]],
      suffix: String
  ): Unit = {
    val extEdges = packed
      .map {
        mm =>
          {
            mm.m.mapping.locAssignments.map {
              (pn, n) =>
                {
                  n.ports
                    .map(_._2)
                    .flatten
                    .filter(_.id.pt == PTOutput)
                    .map {
                      p =>
                        {
                          p.distPorts
                            .map(_._2)
                            .filter {
                              dp =>
                                {
                                  !mm.m.mapping.nodeNames.contains(dp.thisNode)
                                }
                            }
                            .map {
                              dp =>
                                {
                                  val srcType = n.nType.typeString
                                  val dstType = g(dp.thisNode).nType.typeString

                                  (srcType, dstType)
                                }
                            }
                        }
                    }
                    .flatten
                }
            }.flatten
          }
      }
      .flatten
      .groupBy(
        a => a
      )
      .map(
        (k, v) => (k, v.size)
      )
      .toList
      .sortBy(_._2)

    val header = "Edge, Count"
    val rows = extEdges.map(
      (edge, count) => (edge._1 + " -> " + edge._2 + ", " + count)
    )

    val csv = (header :: rows).mkString("\n")

    val fName = params.buildDir + "/csv/ext_edges_" + suffix + ".csv"
    println(CYAN + "Printing: " + fName + RESET)

    val f = Util.writeOpen(fName)
    f.write(csv)
    f.close()
  }

  def apply(params: GlobalParamsInst, g: ElasticGraph, packed: List[MappedMolecule[Molecule]], suffix: String): Unit = {
    reportExternalEdges(params, g, packed, suffix)
  }
}
