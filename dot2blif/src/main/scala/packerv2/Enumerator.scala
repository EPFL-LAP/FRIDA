package packerv2

import crkt._
import core.CompilerState
import arch._
import frontend.GlobalParamsInst
import printers.MoleculePrinter
import util.Util
import archs.Entry
import archs.Exit
import archs.Fork
import packerv2.Enumerator.assignmentString
import core.ATileIo

import collection.mutable.{Set => MSet}
import collection.mutable.{Map => MMap}
import io.AnsiColor._

import com.gurobi.gurobi._

// TODO could make this a graph with only one color and find cliques only there...
object IncompatibilityGraph {
  object SymIncompatInfo {
    def get(dt: DTMolecule, la0: LocAssignment, la1: LocAssignment): Option[SymIncompatInfo] = {
      val la0Ina1Out = (dt.containsId(la0.pn.dagIds) && !dt.containsId(la1.pn.dagIds) && la1.pn.dagIds.nonEmpty)
      val la1Ina0Out = (!dt.containsId(la0.pn.dagIds) && la0.pn.dagIds.nonEmpty && dt.containsId(la1.pn.dagIds))

      if (la1Ina0Out || la1Ina0Out) {
        if (dt.containsId(la0.pn.dagIds)) {
          Some(SymIncompatInfo(la1.pn.dagIds.get.str, la1.n, la0.n))
        } else {
          Some(SymIncompatInfo(la0.pn.dagIds.get.str, la0.n, la1.n))
        }
      } else {
        None
      }
    }
  }

  case class SymIncompatInfo(sym: String, symNode: TNode, noneNode: TNode)

  def apply(
    router: Router,
    dt: DTMolecule,
    candidates: Seq[LocAssignment]
  )(implicit cs: CompilerState[RootPb], params: GlobalParamsInst): IncompatibilityGraph = {
    // Configuration -> canRoute
    val symInfo = MMap[SymIncompatInfo, Boolean]()

    val g = candidates.zipWithIndex
      .map {
        (a0, i) =>
          {
            val m0 = dt + a0

            val callback = () => {
              candidates
                .drop(i)
                .filter(_ != a0)
                .map {
                  a1 =>
                    {
                      // println
                      // println(Enumerator.assignmentString(a0) + " -> " + Enumerator.assignmentString(a1))
                      // println("nn: " + (a0.pn.dagIds.nonEmpty && a1.pn.dagIds.nonEmpty))
                      // println("dt: " + (!dt.containsId(a0.pn.dagIds) && !dt.containsId(a1.pn.dagIds)))
                      if ((a0.pn == a1.pn) || (a0.n == a1.n)) {
                        (a0, a1) :: (a1, a0) :: Nil
                      } else if (
                        a0.pn.dagIds.nonEmpty
                        && a1.pn.dagIds.nonEmpty
                        && !dt.containsId(a0.pn.dagIds)
                        && !dt.containsId(a1.pn.dagIds)
                        && (a0.pn.dagIds.get.str == a1.pn.dagIds.get.str)
                        && (a0.pn.dagIds.get.loc != a1.pn.dagIds.get.loc)
                      ) {
                        // println(Enumerator.assignmentString(a0) + " -> " + Enumerator.assignmentString(a1))
                        Nil
                      } else {
                        lazy val symIncompatInfo = SymIncompatInfo.get(dt, a0, a1)

                        if (symIncompatInfo.nonEmpty && symInfo.contains(symIncompatInfo.get)) {
                          if (symInfo(symIncompatInfo.get)) {
                            Nil
                          } else {
                            (a0, a1) :: (a1, a0) :: Nil
                          }
                        } else {
                          val m01 = m0 + a1

                          val recCallback = () => {
                            m01.isValid(cs.crkt, router, params)
                          }

                          val v = router.withAssignment(cs.crkt, m01, recCallback).getOrElse(false)

                          if (symIncompatInfo.isDefined) {
                            symInfo(symIncompatInfo.get) = v
                          }

                          if (!v) {
                            (a0, a1) :: (a1, a0) :: Nil
                          } else {
                            Nil
                          }
                        }
                      }
                    }
                }
                .flatten
            }

            router.withAssignment(cs.crkt, m0, callback).getOrElse(Nil)
          }
      }
      .flatten
      .toSet

    IncompatibilityGraph(g)
  }

  def crktChoice(root: LocAssignment, candidates: Seq[LocAssignment]): Set[LocAssignment] = {
    candidates.filter(_.n.name == root._2.name).toSet
  }
}

case class IncompatibilityGraph(g: Set[(LocAssignment, LocAssignment)]) {
  override def toString(): String = {
    g.map {
      (a0, a1) =>
        {
          ("(" + Enumerator.assignmentString(a0) + ", " + Enumerator.assignmentString(a1) + ")")
        }
    }.mkString(", ")
  }

  def choice(root: LocAssignment): Set[LocAssignment] = {
    g.filter(_._1 == root).map(_._2) + root
  }

  def keepClique(clique: Set[LocAssignment], choice: Set[LocAssignment]): Set[LocAssignment] = {
    choice
      .filter {
        a =>
          {
            clique.forall(
              ca => g.contains((a, ca))
            )
          }
      }
      .headOption
      .fold(clique)(
        a => keepClique(clique + a, choice - a)
      )
  }

  def archChoice(root: LocAssignment): Set[LocAssignment] = {
    keepClique(Set(root), choice(root).filter(_.n.name != root.n.name))
  }

  def choiceFromInit(initClique: Set[LocAssignment]): Set[LocAssignment] = {
    keepClique(initClique, initClique.map(choice(_)).flatten.filter(!initClique.contains(_)))
  }

  // Is always a clique
  def crktChoice(root: LocAssignment): Set[LocAssignment] = {
    choice(root).filter(_.n.name == root.n.name)
  }
}

sealed trait EnumMolecule extends AbsMolecule {
  def last: LocAssignment
  def parent: Option[EnumMolecule]

  def toMolecule(): Molecule = Molecule(name, pat, mapping, dagIds)

  def getMapping(): Mapping = {
    def rec(em: EnumMolecule, acc: List[LocAssignment]): List[LocAssignment] = {
      em.parent match {
        case None       => em.last :: acc
        case Some(ndtm) => rec(ndtm, em.last :: acc)
      }
    }

    val assignments = rec(this, Nil)
    Mapping(assignments.toSet)
  }

  def extendILP(g: ElasticGraph, router: Router, log: Boolean): RStatus = {
    router.addAssignment(g, this, log)
  }

  def shortenILP(g: ElasticGraph, router: Router): RStatus = {
    router.removeAssignment(g, this)
  }
}

case class CandidateMolecule(
    name: String,
    pat: RRG,
    dagIds: Set[DagId],
    last: LocAssignment,
    parent: Option[EnumMolecule]
) extends EnumMolecule {
  type T = CandidateMolecule

  lazy val mapping: Mapping = getMapping()

  def withName(nName: String): CandidateMolecule = {
    CandidateMolecule(nName, pat, dagIds, last, parent)
  }

  def findMapping(
      g: ElasticGraph,
      router: Router,
      params: GlobalParamsInst,
      log: Boolean
  ): Option[MappedMolecule[CandidateMolecule]] = {
    AbsMolecule.findMapping(g, router, params, this, log)
  }

  override def +(a: LocAssignment): CandidateMolecule = {
    val nDagIds = a.pn.dagIds.fold(dagIds)(
      d => dagIds + d
    )
    CandidateMolecule(name, pat, nDagIds, a, Some(this))
  }

  override def -(a: LocAssignment): Option[EnumMolecule] = {
    this.parent
  }
}

object DTMolecule {
  def apply(
      c: CandidateMolecule,
      validCandidates: List[CandidateMolecule],
      ig: IncompatibilityGraph,
      parent: Option[DTMolecule]
  ): DTMolecule = {
    val tDt = DTMolecule(c.name, c.pat, c.dagIds, c.last, parent, validCandidates, ig)
    val nIds = tDt.mapping.locAssignments.map(_._1.dagIds).flatten.toSet

    DTMolecule(c.name, c.pat, nIds, c.last, parent, validCandidates, ig)
  }
}

case class DTMolecule(
    name: String,
    pat: RRG,
    dagIds: Set[DagId],
    last: LocAssignment,
    parent: Option[DTMolecule],
    validCandidates: List[CandidateMolecule],
    ig: IncompatibilityGraph
) extends EnumMolecule {
  type T = DTMolecule

  lazy val mapping: Mapping = getMapping()

  def withName(nName: String): T = {
    DTMolecule(nName, pat, dagIds, last, parent, validCandidates, ig)
  }

  def findMapping(
      g: ElasticGraph,
      router: Router,
      params: GlobalParamsInst,
      log: Boolean
  ): Option[MappedMolecule[T]] = {
    AbsMolecule.findMapping(g, router, params, this, log)
  }

  override def +(a: LocAssignment): CandidateMolecule = {
    val nDagIds = a.pn.dagIds.fold(dagIds)(
      d => dagIds + d
    )
    CandidateMolecule(name, pat, nDagIds, a, Some(this))
  }

  override def -(a: LocAssignment): Option[EnumMolecule] = {
    this.parent
  }

  override def toString(): String = {
    val pStr = if (parent.isEmpty) {
      ""
    } else {
      Enumerator.assignmentString(parent.get.last)
    }

    name + " at " + pat.molName + ": " + Enumerator.assignmentString(last) + " from " + pStr + validCandidates + ig
  }
}

object Enumerator {
  def laString(a: LocAssignment): String = {
    YELLOW + a.pn.name + RESET + " -> " + GREEN + a.n.name + RESET
  }

  def eaString(ea: ExtAssignment): String = {
    YELLOW + ea.pn.name + RESET + " -> " + GREEN + ea.value + RESET
  }

  def assignmentString(a: Assignment): String = {
    a match {
      case la: LocAssignment => laString(la)
      case ea: ExtAssignment => eaString(ea)
    }
  }

  def mappingString(m: EnumMolecule): String = {
    val assignmentStr = m.mapping.assignments
      .map {
        case la @ LocAssignment(pn, n) => {
          if ((pn.name == m.last.pn.name) && (n.name == m.last.n.name)) {
            ""
          } else {
            laString(la)
          }
        }

        case ea @ ExtAssignment(pn, value) => {
          eaString(ea)
        }
      }
      .mkString(", ")

    val lastStr = "[" + laString(m.last) + "]"

    assignmentStr + ", " + lastStr
  }

  def apply(): Enumerator = Enumerator(Router())
}

case class Enumerator(router: Router) {
  var externalId: Int = 0

  def getCrktNeighbors(dt: DTMolecule, dir: PortType)(implicit cs: CompilerState[RootPb]): List[TNode] = {
    dt.mapping.nodes
      .map(
        n => cs.crkt.neighbors(n, dir)
      )
      .flatten
      .filter(
        n => !dt.mapping.nodeNames.contains(n.name)
      )
      .map(
        (n => (n.name, n))
      )
      .toMap
      .map(_._2)
      .toList
  }

  // TODO once a primitive type is reached, do not list primitives of the same type in the successors
  def getRRGNeighbors(
      dt: DTMolecule,
      dir: PortType
  )(implicit cs: CompilerState[RootPb]): List[RRGPrim] = {
    dt.mapping.prims
      .map {
        pn =>
          {
            dt.pat.neighbors(pn, dir)
          }
      }
      .flatten
      .toSet
      .toList
      .filter(
        pn => !dt.mapping.prims.contains(pn)
      )
  }

  def enumerateCandidates(crktNeighbors: List[TNode], rrgNeighbors: List[RRGPrim]): List[LocAssignment] = {
    rrgNeighbors
      .map {
        pn =>
          {
            crktNeighbors.map {
              n =>
                {
                  LocAssignment(pn, n)
                }
            }
          }
      }
      .flatten
      .filter(
        a => {
          a.pn.canMap(a.n)
        }
      )
  }

  def validCandidates(
      dt: DTMolecule,
      candidates: List[LocAssignment]
  )(implicit cs: CompilerState[RootPb], params: GlobalParamsInst): List[CandidateMolecule] = {
    val validSyms = MMap[TNode, Set[String]]()
    val invalidSyms = MMap[TNode, Set[String]]()

    candidates
      .filter(
        a => a.pn.canMap(a.n)
      )
      .map { // .filter(a => (a.pn.name == "Br__7") && (a.n.name == "cond_br3"))
        a =>
          {
            // println(a.pn.name + " -> " + a.n.name + " ")

            val ma = dt + a
            assert(ma.dagIds == ma.mapping.locAssignments.map(_._1.dagIds).flatten.toSet)

            val cacheable = a.pn.dagIds.nonEmpty && !dt.containsId(a.pn.dagIds)

            val cachedValid = cacheable && validSyms.get(a.n).fold(false)(_.contains(a.pn.dagIds.get.str))
            val cachedInvalid = cacheable && invalidSyms.get(a.n).fold(false)(_.contains(a.pn.dagIds.get.str))

            val res = if (cachedValid) {
              // val mm = router.withAssignment(cs.crkt, ma, () => ma.findMapping(cs.crkt, router, params)).getOrElse(None)
              // assert(mm.nonEmpty,
              //   "" + a + " -> " + a.pn.dagIds + " with " + validSyms(a.n).mkString(", "))

              Some(ma)
            } else if (cachedInvalid) {
              // val mm = router.withAssignment(cs.crkt, ma, () => ma.findMapping(cs.crkt, router, params)).getOrElse(None)
              // assert(mm.isEmpty || mm.get.isDisconnected(),
              //   "" + a + " -> " + a.pn.dagIds + " with " + invalidSyms(a.n).mkString(", "))

              None
            } else {
              val callback = () => ma.findMapping(cs.crkt, router, params, false)
              val mm = router.withAssignment(cs.crkt, ma, callback).getOrElse(None)

              if (mm.nonEmpty && mm.get.isConnected()) {
                if (a.pn.dagIds.nonEmpty && !dt.containsId(a.pn.dagIds)) {
                  validSyms(a.n) = validSyms.getOrElse(a.n, Set()) + a.pn.dagIds.get.str
                }

                mm.map(_.m)
              } else {
                if (a.pn.dagIds.nonEmpty && !dt.containsId(a.pn.dagIds)) {
                  invalidSyms(a.n) = invalidSyms.getOrElse(a.n, Set()) + a.pn.dagIds.get.str
                }

                None
              }
            }

            res
          }
      }
      .flatten
  }

  def inOpenedSymetry(dt: DTMolecule, candidates: List[CandidateMolecule]): Set[TNode] = {
    candidates
      .filter {
        m =>
          {
            m.last.pn.dagIds.nonEmpty && dt.containsId(m.last.pn.dagIds)
          }
      }
      .map(_.last.n)
      .toSet
  }

  def keepLocalOnly(dt: DTMolecule, inUsed: Set[TNode], la: LocAssignment): Boolean = {
    if (inUsed.contains(la.n)) {
      dt.containsId(la.pn.dagIds)
    } else {
      true
    }
  }

  def keepLocalOnly(dt: DTMolecule, candidates: List[CandidateMolecule]): List[CandidateMolecule] = {
    val inUsed = inOpenedSymetry(dt, candidates)
    candidates.filter {
      m =>
        {
          keepLocalOnly(dt, inUsed, m.last)
        }
    }
  }

  // If we can map something in both some already opened symmetry and an unexplored one, map to the explored one
  // If we can map something in both the default symetry and an unexplored one, accept both
  def keepLocalOnly(
      dt: DTMolecule,
      candidates: List[CandidateMolecule],
      ig: IncompatibilityGraph
  ): (List[CandidateMolecule], IncompatibilityGraph) = {
    val inUsed = inOpenedSymetry(dt, candidates)
    val nCandidates = candidates.filter(
      m => keepLocalOnly(dt, inUsed, m.last)
    )

    val nIGraph = ig.g.filter(
      (srcLa, dstLa) => keepLocalOnly(dt, inUsed, srcLa) && keepLocalOnly(dt, inUsed, dstLa)
    )
    val nIG = IncompatibilityGraph(nIGraph)

    (nCandidates, nIG)
  }

  def getNeighbors(dt: DTMolecule)(implicit
    cs: CompilerState[RootPb],
    params: GlobalParamsInst
  ): List[CandidateMolecule] = {
    def rec(dir: PortType): List[CandidateMolecule] = {
      // println("--")
      val crktNeighbors = getCrktNeighbors(dt, dir)
      // println("nCrkt: " + crktNeighbors.map(_.name).mkString(", "))

      val rrgNeighbors = getRRGNeighbors(dt, dir)
      // println("nRRG: " + rrgNeighbors.map(_.name).mkString(", "))

      val allCandidates = enumerateCandidates(crktNeighbors, rrgNeighbors)
      // println("all candidates:\n" + allCandidates.map(assignmentString(_)).mkString("\n"))

      val f = () => {
        keepLocalOnly(dt, validCandidates(dt, allCandidates))
      }

      val candidates = Util.timeOf("validCandidates", true)(f)
      // println("valid candidates:\n" + candidates.map(_.last).map(assignmentString(_)).mkString("\n"))
      // candidates.filter(_.isConnected()) // TODO should no longer be necessary

      candidates
    }

    val inNs = rec(PTInput)
    if (inNs.isEmpty) {
      rec(PTOutput)
    } else {
      inNs
    }
  }

  // TODO May want to prioritize the assignments which do not involve any choices
  def pickChoiceRoot(candidates: List[LocAssignment]): Option[LocAssignment] = {
    candidates
      .groupBy(_.n.name)
      .map(_._2)
      .filter(_.size > 1)
      .headOption
      .fold {
        candidates.sortBy(_.pn.name).headOption
      } {
        _.headOption
      }
  }

  def keepForcedMapping(m: List[MappedMolecule[CandidateMolecule]]): List[MappedMolecule[CandidateMolecule]] = {
    val forcedAssignments = Map(
      ("cond_br3" -> "Br__7"),
      ("cond_br2" -> "Br__0"),
      ("cond_br0" -> "Br__1"),
      ("mux1" -> "Mux32__0"),
      ("mux2" -> "Mux32__1")
    )

    m.filter {
      m =>
        {
          m.mapping.locAssignments
            .map(
              (pn, n) => (pn.name, n.name)
            )
            .forall {
              (pnName, nName) =>
                {
                  forcedAssignments.get(nName).fold(true)(_ == pnName)
                }
            }
        }
    }
  }

  def removeRedundant(dt: DTMolecule, candidates: List[CandidateMolecule]): List[CandidateMolecule] = {
    val localSyms = candidates.map(_.last.pn.dagIds).flatten.toSet
    val acceptedSyms = dt.acceptedSym(localSyms)

    candidates.filter(
      c =>
        c.last.pn.dagIds.fold(true)(
          id => acceptedSyms.contains(id)
        )
    )
  }

  // TODO will also want to prune buffer mappings, etc...
  def keepOneChoice(
      dt: DTMolecule,
      candidates: List[CandidateMolecule],
      ig: IncompatibilityGraph
  )(implicit cs: CompilerState[RootPb], params: GlobalParamsInst): List[CandidateMolecule] = {
    if (candidates.isEmpty) {
      Nil
    } else {
      val choiceRoot = pickChoiceRoot(candidates.map(_.last)).get
      val nAssignments = candidates.map(_.last)
      val nAssignmentsMap = candidates.groupBy(_.last).map {
        (k, v) =>
          assert(v.size == 1); (k, v.head)
      }

      // lazy val archChoice = ig.archChoice(choiceRoot)
      lazy val crktChoice = IncompatibilityGraph.crktChoice(choiceRoot, nAssignments)

      // println(archChoice.map(la => (la.n.name + " -> " + la.pn.name)))
      // println
      // println("crkt: " + crktChoice.map(la => (la.n.name + " -> " + la.pn.name)))

      // TODO If the complete circuit choice with symetries map to things with the same symetry, can ignore the choice.
      // TODO Even if we drop neighbors because of symetries, we can probably keep the validity computations around..
      val choiceRaw = ig.choiceFromInit(crktChoice).toList
      // println("choice: " + choiceRaw.map(la => (la.n.name + " -> " + la.pn.name)))

      val choiceSym = choiceRaw
        .sortWith(
          (a0, a1) => a0.pn.dagIds.isEmpty
        )
        .map(
          a => nAssignmentsMap(a)
        )

      val nextChoice = removeRedundant(dt, choiceSym)
      // val nextChoice = keepForcedMapping(nextChoiceSym)

      if (nextChoice.size > 1) {
        println("next choice: " + nextChoice.map(_.last).map(Enumerator.assignmentString(_)).mkString(", "))
      }

      nextChoice
    }
  }

  // TODO may want to update the candidate list to have a proper parent, if necessary
  def getDTMolecule(
      candidates: List[CandidateMolecule],
      nextChoice: List[CandidateMolecule],
      ig: IncompatibilityGraph,
      selected: CandidateMolecule,
      parent: Option[DTMolecule]
  ): DTMolecule = {
    val selectedLAs = nextChoice.map(_.last).toSet
    val mutExcl = ig.choice(selected.last)

    val nCandidates = candidates.filter(
      m => !selectedLAs.contains(m.last) && !mutExcl.contains(m.last)
    )
    val nLAs = nCandidates.map(_.last).toSet

    val nIgEdges = ig.g.filter {
      (srcLA, dstLA) =>
        {
          nLAs.contains(dstLA) && nLAs.contains(dstLA)
        }
    }

    val nIg = IncompatibilityGraph(nIgEdges)

    DTMolecule(selected, nCandidates, nIg, parent)
  }

  def getSuccessors(
    dt: DTMolecule
  )(implicit cs: CompilerState[RootPb], params: GlobalParamsInst): Seq[DTMolecule] = {
    val (candidates, ig) = if (dt.validCandidates.isEmpty) {
      val newCandidates = getNeighbors(dt)
      val newIg = IncompatibilityGraph(router, dt, newCandidates.map(_.last))

      (newCandidates, newIg)
    } else {
      // (keepLocalOnly(dt, dt.validCandidates), dt.ig)
      // (dt.validCandidates, dt.ig)

      keepLocalOnly(dt, dt.validCandidates, dt.ig)
    }

    val nextChoiceCandidates = keepOneChoice(dt, candidates, ig)

    val nextChoiceDTs = nextChoiceCandidates.map {
      cmm =>
        {
          getDTMolecule(candidates, nextChoiceCandidates, ig, cmm, Some(dt))
        }
    }

    nextChoiceDTs
  }

  def decisionTree(
    m: DTMolecule
  )(implicit cs: CompilerState[RootPb], params: GlobalParamsInst): Seq[DTMolecule] = {
    val callback = () => {
      // println
      // println("state: " + Enumerator.mappingString(m))
      // router.model.writeModel(m, params)
      // val mm = m.findMapping(cs.crkt, router, params).get
      // MoleculePrinter(params, mm.withName("debug" + m.mapping.assignments.size), false)

      val succs = Util.timeOf("getSuccessors", true)(
        () => getSuccessors(m)
      )
      // println(succs.map(_.last))
      val recMols = succs.map(decisionTree(_)).flatten

      if (recMols.isEmpty) {
        // is a Maximum Match

        // router.model.writeModel(m, params)
        // val mm = m.findMapping(cs.crkt, router, params).get
        // MoleculePrinter(params, mm.withName("debug" + m.mapping.assignments.size), false)
        // println(mm.assignments.mkString("\n"))

        m :: Nil
      } else { // return mappings containing this one
        recMols
      }
    }

    router.withAssignment(cs.crkt, m, callback).get
  }

  def logString(
      params: GlobalParamsInst,
      n: TNode,
      r: RRGPrim,
      cs: CompilerState[RootPb],
      rrg: RRG
  ): String = {
    r.name + ":" + YELLOW + r.block.name + RESET
      + " -> " + n.name + ":" + YELLOW + n.nType + RESET
  }

  def toMolecule(
      params: GlobalParamsInst,
      dtMols: Seq[DTMolecule],
      g: ElasticGraph
  ): List[MappedMolecule[Molecule]] = {
    dtMols.map {
      dt =>
        {
          val callback = (r: Router) => {
            val res = dt.route(r, params, true)
            if (res.isEmpty) {
              None
            } else {
              Some(res)
            }
          }

          val assignments = Router.withFullILP(g, dt, callback, KeepMixed).get
          MappedMolecule(dt.toMolecule(), assignments)
        }
    }.toList
  }

  def apply(
      params: GlobalParamsInst,
      n: TNode,
      r: RRGPrim,
      cs: CompilerState[RootPb],
      rrg: RRG
  ): Seq[MappedMolecule[Molecule]] = {
    if (r.canMap(n)) {
      val router = Router()
      val root = LocAssignment(r, n)

      println("--")
      println(logString(params, n, r, cs, rrg))

      val mol = DTMolecule("", rrg, Set(), root, None, Nil, IncompatibilityGraph(Set()))
      val mm = router.withAssignment(cs.crkt, mol, () => mol.findMapping(cs.crkt, router, params, false))

      if (mm.nonEmpty && mm.get.isEmpty) {
        router.withAssignment(cs.crkt, mol, () => mol.findMapping(cs.crkt, router, params, true))
      }

      if (mm.isEmpty) {
        // TODO this should only be used for CGRA-MR comparison
        // TODO In particular, with Riken the constant does not have a path to exit...
        router.freeILP()

        Nil
      } else {
        assert(mm.nonEmpty)
        assert(mm.get.nonEmpty)
        assert(mm.get.get.isValid())

        val mappedMols = decisionTree(mol)(cs, params).zipWithIndex.map {
          (m, i) =>
            {
              m.withName(m.pat.molName + "_" + r.name + "_" + n.name + "___" + i)
            }
        }

        router.freeILP()

        toMolecule(params, mappedMols, cs.crkt)
      }
    } else {
      Nil
    }
  }
}
