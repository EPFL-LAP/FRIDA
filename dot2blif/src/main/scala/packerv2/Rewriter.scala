package packerv2

import crkt._
import frontend.GlobalParamsInst
import core.ASpanning
import printers.MoleculePrinter

import collection.mutable.{Set => MSet}
import io.AnsiColor._

// TODO Probably want to remove the last from the Molecule here..
// TODO Have a different molecule object for the Enumeration than for

object Rewriter {
  def getSplittedMolecule(
      params: GlobalParamsInst,
      g: ElasticGraph,
      toSplit: MappedMolecule[Molecule],
      nodes: Set[TNode]
  ): MappedMolecule[Molecule] = {
    val nMolMapping = Mapping(
      nodes.toSeq
        .map(
          n => LocAssignment(toSplit.mapping.invAssignments(n), n)
        )
        .toSet
    )
    val nDagIds = nMolMapping.assignments.map(_.pn.dagIds).flatten.toSet
    val nMol = Molecule(toSplit.name + "0", toSplit.pat, nMolMapping, nDagIds)

    val callback = (r: Router) => Some(nMol.route(r, params, false))
    val nMapping = Router.withFullILP(g, nMol, callback, KeepMixed).get

    MappedMolecule(nMol, nMapping)
  }

  def splitMolecule(
      params: GlobalParamsInst,
      g: ElasticGraph,
      nPick: MappedMolecule[Molecule],
      toSplit: MappedMolecule[Molecule]
  ): List[MappedMolecule[Molecule]] = {
    val nPickNodes = nPick.m.mapping.nodes
    val pickedWithSuccs = nPickNodes
      .map(
        n => toSplit.m.successors(g, n)
      )
      .flatten

    val nSuccMolNodes = pickedWithSuccs -- nPickNodes
    val nSuccMol = getSplittedMolecule(params, g, toSplit, nSuccMolNodes)

    val toSplitNodes = toSplit.m.mapping.nodes
    val nPredMolNodes = toSplitNodes -- pickedWithSuccs
    val nPredsMol = getSplittedMolecule(params, g, toSplit, nPredMolNodes)

    // TODO If we fail to make a cut, simply put all nodes by themselves for now
    // TODO If we get a very sparse packing, this may be a source of sparsity
    if (nSuccMol.assignments.isEmpty && toSplit.pat.t.annotations.contains(ASpanning)) {
      val singleMols = nSuccMolNodes.toList.map {
        succN =>
          {
            val nM = getSplittedMolecule(params, g, toSplit, Set(succN))
            assert(nM.assignments.nonEmpty)

            nM
          }
      }

      (nPredsMol :: nSuccMol :: singleMols).filter(_.m.mapping.assignments.nonEmpty)
    } else {
      (nPredsMol :: nSuccMol :: Nil).filter(_.m.mapping.assignments.nonEmpty)
    }
  }

  def updateMatches(
      params: GlobalParamsInst,
      g: ElasticGraph,
      nPick: MappedMolecule[Molecule],
      matches: Seq[MappedMolecule[Molecule]]
  ): Seq[MappedMolecule[Molecule]] = {
    val nPickNodes = nPick.m.mapping.nodes

    matches.map {
      mm =>
        {
          val nAssignments = mm.m.mapping.assignments.filter {
            case LocAssignment(pn, n) => !nPickNodes.contains(n)
            case ea: ExtAssignment    => true
          }

          if (nAssignments.size == mm.m.mapping.assignments.size) {
            mm :: Nil
          } else if (nAssignments.isEmpty) {
            Nil
          } else {
            val nDagIds = nAssignments.map(_.pn.dagIds).flatten.toSet

            val nM = Molecule(mm.m.name, mm.m.pat, Mapping(nAssignments), nDagIds)

            val callback = (r: Router) => Some(nM.route(r, params, false))
            val mapping = Router.withFullILP(g, nM, callback, KeepMixed).get

            if (mapping.isEmpty) {
              splitMolecule(params, g, nPick, mm)
            } else {
              MappedMolecule(nM, mapping) :: Nil
            }
          }
        }
    }.flatten
  }

  def apply(
      params: GlobalParamsInst,
      g: ElasticGraph,
      allMatches: Seq[MappedMolecule[Molecule]]
  ): Seq[MappedMolecule[Molecule]] = {
    println(GREEN + "Rewrite" + RESET)

    val selectedNodes = MSet[TNode]()

    def rec(
        remainingMatches: Seq[MappedMolecule[Molecule]],
        acc: List[MappedMolecule[Molecule]]
    ): List[MappedMolecule[Molecule]] = {
      if (g.nodes.map(_._2).forall(selectedNodes.contains(_))) {
        acc
      } else {
        if (remainingMatches.filter(_.isValid()).sorted.isEmpty) {
          val remainingNodes = g.nodes.map(_._2).filter(!selectedNodes.contains(_))
          println("Could not match: " + remainingNodes.map(_.name).mkString(", "))
          scala.sys.error("Unmappable circuit!")
        } else {
          val nextPick = remainingMatches.filter(_.isValid()).sortBy(-_.objective()).head
          val nextPickNodes = nextPick.m.mapping.nodes

          println(
            "pick: " + nextPick.m.name
              + " with " + nextPick.numPrims + " prims, "
              + nextPick.numFbkEdges + " feedbacks"
          )

          // TODO may also want to break disconnected matches
          val nRemainingMatches = updateMatches(params, g, nextPick, remainingMatches)
          selectedNodes ++= nextPickNodes

          rec(nRemainingMatches, nextPick :: acc)
        }
      }
    }

    rec(allMatches.sortBy(-_.objective()), Nil)
  }
}
