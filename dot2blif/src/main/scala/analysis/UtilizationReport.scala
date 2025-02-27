package analysis

import frontend.GlobalParamsInst
import packerv2.MappedMolecule
import packerv2.Molecule
import packerv2.Legalizer
import util.Util
import packerv2.ImplementedMolecule

// TODO this seems buggy

object UtilizationReport {
  def savePrimitiveUtilization(params: GlobalParamsInst, mols: List[ImplementedMolecule]): Unit = {
    val report = mols
      .map(_.mm.get)
      .map {
        mm =>
          {
            val nonIdMappings = mm.m.mapping.locAssignments
              .filter(!_._2.name.contains(Legalizer.identity))
              .filter(!_._1.isIo)

            val nodesCount = nonIdMappings.size
            val nodesArea = nonIdMappings.map(_._1.block.physicalInfo.area).sum

            val prims = mm.m.pat.prims.filter(!_.isIo)

            // TODO this does not account for the configuration multiplexer area, not CB / SB

            val primsCount = prims.size
            val primsArea = prims.map(_.block.physicalInfo.area).sum

            (mm.pat.molName, (nodesCount, nodesArea, primsCount, primsArea))
          }
      }
      .groupBy(_._1)
      .map {
        (k, l) =>
          {
            val stats = l.map(_._2).reduce {
              (a, b) =>
                {
                  ((a._1 + b._1), (a._2 + b._2), (a._3 + b._3), (a._4 + b._4))
                }
            }

            // TODO probably want to report absolute utilization numbers

            (k, ((stats._1 / l.size), (stats._2 / l.size), (stats._3 / l.size), (stats._4 / l.size)))
          }
      }

    val header = "tile, circuit nodes, nodes area, architecture primitives, prims area\n"
    val rows = report
      .map {
        (name, stats) => (name + ", " + stats._1 + ", " + stats._2 + ", " + stats._3 + ", " + stats._4)
      }
      .mkString("\n")

    val csv = params.buildDir + "/csv/" + "prim_utilization.csv"

    val f = Util.writeOpen(csv)
    f.write(header)
    f.write(rows + "\n")
    f.close()
  }
}
