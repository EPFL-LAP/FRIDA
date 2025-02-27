package packerv2

import crkt._

import scala.annotation.targetName
import mlir.AttrInteger

object AnnotateMolecules {
  val molAttr = "mol"

  @targetName("anotUnMapped")
  def apply[M <: AbsMolecule](g: ElasticGraph, selectedMolecules: Seq[M]): ElasticGraph = {
    val nodeNameToMol = selectedMolecules
      .map {
        m =>
          {
            val molName = m.name

            m.mapping.nodes.map(
              n => (n, molName)
            )
          }
      }
      .flatten
      .toMap

    val nNodes = g.nodes.map {
      (nName, n) =>
        {
          val nNode = n.withAttr(molAttr, nodeNameToMol(n))

          (nName, nNode)
        }
    }

    ElasticGraph(nNodes, g.properties)
  }

  @targetName("anotMapped")
  def apply[M <: AbsMolecule](g: ElasticGraph, selectedMolecules: Seq[MappedMolecule[M]]): ElasticGraph = {
    AnnotateMolecules(g, selectedMolecules.map(_.m))
  }
}

object AnnotateBasicBlocks {
  val bbAttr = "mol"

  def apply(g: ElasticGraph): ElasticGraph = {
    val nNodes = g.nodes
      .map(_._2)
      .zipWithIndex
      .map {
        (n, i) =>
          {
            val bb = if (n.mlirAttr.contains("handshake.bb")) {
              n.mlirAttr("handshake.bb").asInstanceOf[AttrInteger].value
            } else {
              i
            }

            val nNode = n.withAttr(bbAttr, bb.toString())

            (nNode.name, nNode)
          }
      }
      .toMap

    ElasticGraph(nNodes, g.properties)
  }
}
