package packerv2

import frontend.GlobalParamsInst
import frontend.GlobalParams
import arch.RootPb
import crkt.TNode
import core.CompilerState
import printers.RRGPrinter
import crkt.PortNodeID
import printers.MoleculePrinter
import arch.DagId
import archs.Fork
import archs.Mux
import archs.CntrlMerge
import arch.Tile
import core.Namer
import crkt.Port
import arch.PortType
import arch.PTInput
import packerv2.Enumerator.assignmentString
import packerv2.Enumerator.mappingString
import arch.PTOutput
import crkt.ElasticEdge
import crkt.ElasticGraph
import crkt.Assignable
import crkt.CrktToMlirConverter
import printers.MLIRPrinter
import printers.DotPrinter
import core.ATileIo
import crkt.MlirToCrktConverter
import readers.MLIRParser
import arch.Impl
import arch.Hs
import crkt.Node
import archs.ForkParams
import analysis.AnalysisPacking
import archs.EagerFork

import io.AnsiColor._
import math.min
import collection.mutable.{Set => MSet}
import collection.mutable.ListBuffer
import sys.process._
import scala.concurrent.*
import ExecutionContext.Implicits.global
import scala.util.Failure
import scala.util.Success
import scala.concurrent.duration.Duration
import java.io.FileOutputStream
import java.io.File
import core.ASpanning
import scala.io.Source
import printers.RRGPrinter.dagIdStr
import arch.BlockPortID.PortInstanceParser
import util.Util

sealed trait PhysicalAssignment

sealed trait ILPVariable {
  def ilpName: String
}

object Assignment {
  def apply(m: AbsMolecule, pn: RRGPrim, n: TNode, e: ElasticEdge): Assignment = {
    if (m.isExt(n)) {
      ExtAssignment(m, pn, e)
    } else {
      LocAssignment(pn, n)
    }
  }
}

sealed trait Assignment extends PhysicalAssignment with ILPVariable {
  def pn: RRGPrim
}

case class LocAssignment(pn: RRGPrim, n: TNode) extends Assignment {
  override def hashCode(): Int = (pn.name, n.name).hashCode()

  override def equals(x: Any): Boolean = {
    if (x.isInstanceOf[LocAssignment]) {
      val a1 = x.asInstanceOf[LocAssignment]
      (a1.pn.name == this.pn.name) && (a1.n.name == this.n.name)
    } else {
      false
    }
  }

  override def toString(): String = {
    GREEN + "A    " + CYAN + pn.name + RESET + " -> " + n.name
  }

  def ilpName: String = {
    "F__" + pn.name + "__" + n.name
  }
}

object ExtAssignment {
  def apply(m: AbsMolecule, pn: RRGPrim, e: ElasticEdge): ExtAssignment = {
    val extValue = m.getExternalValue(e).get
    ExtAssignment(pn, extValue)
  }
}

case class ExtAssignment(pn: RRGPrim, value: PortNodeID) extends Assignment {
  override def hashCode(): Int = (pn.name, value).hashCode()

  override def equals(x: Any): Boolean = {
    if (x.isInstanceOf[ExtAssignment]) {
      val a1 = x.asInstanceOf[ExtAssignment]
      (a1.pn.name == this.pn.name)
      && (a1.value == this.value)
    } else {
      false
    }
  }

  override def toString(): String = {
    GREEN + "A    " + CYAN + pn.name + RESET + " -> " + Namer(value)
  }

  def ilpName: String = {
    "F__" + pn.name + "__" + Namer(value)
  }
}

case class RRAssignment(rr: RR, value: PortNodeID) extends PhysicalAssignment with ILPVariable {
  override def toString(): String = {
    YELLOW + "RRA  " + CYAN + rr.name + RESET + " -> " + value
  }

  def ilpName: String = {
    "R__" + rr.name + "__" + Namer(value)
  }
}

// case class RRSinkAssignment(rr: RR, value: PortNodeID, sink: PortNodeID) extends PhysicalAssignment with ILPVariable {
//   override def toString(): String = {
//     RED + "RRSA " + CYAN + rr.name + RESET + " -> " + value + " -> " + sink
//   }

//   def ilpName: String = {
//     "RS__" + rr.name +  "__" + Namer(value) + "__" + Namer(sink)
//   }
// }

case class RREdgeAssignment(
    src: RR,
    dst: RR,
    value: PortNodeID,
    sink: PortNodeID
) extends PhysicalAssignment
    with ILPVariable {
  override def toString(): String = {
    RED + "RREA " + CYAN + src.name + ", " + dst.name + RESET + " -> " + value + " -> " + sink
  }

  def ilpName: String = {
    "RS__" + src.name + dst.name + "__" + Namer(value) + "__" + Namer(sink)
  }
}

sealed trait ConstrType
case object RO extends ConstrType
case object RCU extends ConstrType
case object RCD extends ConstrType
case object RRU extends ConstrType
case object DID extends ConstrType
case object PLEG extends ConstrType
case object PEXCL extends ConstrType
case object NMAP extends ConstrType
case object STARTU extends ConstrType
case object STARTD extends ConstrType
case object ENDU extends ConstrType
case object ENDD extends ConstrType
case object LOOP extends ConstrType
case object ASGN extends ConstrType
case object HSPin extends ConstrType
case object STType extends ConstrType

case class ILPConstraint(
    baseName: ConstrType,
    terms: Set[(Double, ILPVariable)],
    op: Char,
    rhs: Double | ILPVariable
) {
  override def toString(): String = {
    "" + baseName + ": " + terms
      .map(
        (k, v) => ("" + k + " " + v + "")
      )
      .mkString("\n+ ") + " \n" + op + " " + rhs
  }

  def ilpName: String = baseName.toString()
}

case class Mapping(assignments: Set[Assignment]) {
  lazy val locAssignments = assignments
    .collect {
      case la: LocAssignment => la
    }
    .map(
      la => (la.pn, la.n)
    )
    .toMap
  lazy val invAssignments = locAssignments
    .map(
      (k, v) => (v, k)
    )
    .toMap

  lazy val nodes = locAssignments.values.toSet
  lazy val nodeNames = nodes
    .map(
      (n => (n.name, n))
    )
    .toMap
  lazy val prims = assignments.map(_.pn).toSet

  def +(a: Assignment): Mapping = {
    Mapping(assignments + a)
  }

  def -(a: Assignment): Mapping = {
    Mapping(assignments - a)
  }

  def contains(pn: RRGPrim): Boolean = {
    prims.contains(pn)
  }

  def contains(n: TNode): Boolean = {
    nodes.contains(n)
  }

  def get(n: TNode): Option[RRGPrim] = {
    invAssignments.get(n)
  }

  override def toString(): String = {
    assignments
      .map {
        case LocAssignment(pn, n)     => (pn.name + " -> " + n.name)
        case ExtAssignment(pn, value) => (pn.name + " -> " + value)
      }
      .mkString(", ")
  }
}

sealed trait EdgeType
case object ELoc extends EdgeType
case object EIExt extends EdgeType
case object EOExt extends EdgeType

object AbsMolecule {
  // TODO Could also be typeClass or F-Bounded type, but seems much more complicated?
  def findMapping[T <: AbsMolecule](
      g: ElasticGraph,
      router: Router,
      params: GlobalParamsInst,
      mol: T,
      log: Boolean
  ): Option[MappedMolecule[T]] = {
    val localMapping = router.route(params, mol, log)

    if (localMapping.isEmpty) {
      None
    } else {
      Some(MappedMolecule(mol, localMapping.get))
    }
  }
}

// TODO should we use this instead? trait AbsMolecule[T <: AbsMolecule[T]] { ???
trait AbsMolecule {
  def name: String
  def mapping: Mapping
  def pat: RRG
  def dagIds: Set[DagId]

  type T <: AbsMolecule

  def withName(nName: String): T
  def findMapping(g: ElasticGraph, router: Router, params: GlobalParamsInst, log: Boolean): Option[MappedMolecule[T]]

  def +(a: LocAssignment): AbsMolecule
  def -(a: LocAssignment): Option[AbsMolecule]

  def edges(g: ElasticGraph): List[ElasticEdge] = {
    mapping.nodes
      .map(_.ports.map(_._2).flatten)
      .flatten
      .map {
        p =>
          {
            p.distPorts
              .map(_._2)
              .filter {
                dp =>
                  {
                    mapping.contains(g(dp.thisNode))
                  }
              }
              .map {
                dp =>
                  {
                    ElasticEdge(p, dp)
                  }
              }
          }
      }
      .flatten
      .toSet
      .toList
  }

  def isExt(n: TNode): Boolean = {
    !mapping.invAssignments.contains(n)
  }

  def isExt(e: ElasticEdge): Boolean = {
    classifyEdge(e) match {
      case EIExt | EOExt => true
      case ELoc          => false
    }
  }

  def isExt(src: PortNodeID, dst: PortNodeID): Boolean = {
    classifyEdge(src, dst) match {
      case EIExt | EOExt => true
      case ELoc          => false
    }
  }

  def classifyEdge(e: ElasticEdge): EdgeType = {
    if (!this.mapping.nodeNames.contains(e.fromPort.thisNode)) {
      EIExt
    } else if (!this.mapping.nodeNames.contains(e.toPort.thisNode)) {
      EOExt
    } else {
      ELoc
    }
  }

  def classifyEdge(src: PortNodeID, dst: PortNodeID): EdgeType = {
    if (!this.mapping.nodeNames.contains(src.nodeName)) {
      EIExt
    } else if (!this.mapping.nodeNames.contains(dst.nodeName)) {
      EOExt
    } else {
      ELoc
    }
  }

  def getExternalValue(e: ElasticEdge): Option[PortNodeID] = {
    if (!this.mapping.nodeNames.contains(e.fromPort.thisNode)) {
      Some(e.fromPort.nodeID())
    } else if (!this.mapping.nodeNames.contains(e.toPort.thisNode)) {
      Some(e.toPort.nodeID())
    } else {
      None
    }
  }

  def getExternalNode(g: ElasticGraph, e: ElasticEdge): Option[TNode] = {
    if (!this.mapping.nodeNames.contains(e.fromPort.thisNode)) {
      Some(g.nodes(e.fromPort.thisNode))
    } else if (!this.mapping.nodeNames.contains(e.toPort.thisNode)) {
      Some(g.nodes(e.toPort.thisNode))
    } else {
      None
    }
  }

  def getLocalNode(e: ElasticEdge): Option[RRGPrim] = {
    if (mapping.nodeNames.contains(e.fromPort.thisNode)) {
      val n = mapping.nodeNames(e.fromPort.thisNode)
      Some(mapping.invAssignments(n))
    } else if (mapping.nodeNames.contains(e.toPort.thisNode)) {
      val n = mapping.nodeNames(e.toPort.thisNode)
      Some(mapping.invAssignments(n))
    } else {
      None
    }
  }

  def isValid(g: ElasticGraph, router: Router, params: GlobalParamsInst): Boolean = {
    this.findMapping(g, router, params, false).nonEmpty
  }

  def route(router: Router, params: GlobalParamsInst, log: Boolean): Set[PhysicalAssignment] = {
    router.route(params, this, log).getOrElse(Set())
  }

  def acceptedSym(nIds: Set[DagId]): Set[DagId] = {
    val local = dagIds

    val neighb = nIds
      .groupBy(_.str)
      .map(_._2)
      .map {
        syms =>
          {
            syms.filter(!local.contains(_)).reduceOption {
              (a, b) =>
                {
                  if (a.loc < b.loc) {
                    a
                  } else {
                    b
                  }
                }
            }
          }
      }
      .flatten

    local ++ neighb
  }

  // Always contains default DagId and unexplored symetries if none have been explored yet
  def containsId(did: Option[DagId]): Boolean = {
    did.fold(true) {
      d =>
        {
          // dagIds.groupBy(_.str).get(d.str).fold(true)(_.contains(d))
          dagIds.contains(d)
        }
    }
  }

  def successors(g: ElasticGraph, n: TNode): Set[TNode] = {
    def rec(starts: Set[TNode], succs: Set[TNode]): Set[TNode] = {
      if ((succs.size == mapping.assignments.size) || starts.isEmpty) {
        succs
      } else {
        val start = starts.head
        val nNodes = start
          .successors(g)
          .filter(
            n => mapping.nodeNames.contains(n.name)
          )

        val nSuccs = succs ++ nNodes
        val nStarts = starts.tail ++ (nNodes -- succs)

        rec(nStarts, nSuccs)
      }
    }

    if (mapping.invAssignments.contains(n)) {
      val start = Set(n)
      rec(start, start)
    } else {
      Set()
    }
  }

  override def toString(): String = {
    mapping.toString()
  }
}

object Molecule {
  def fromFile(fName: String, rrgs: Seq[RRG],  g: ElasticGraph): Molecule = {
    val fileContent = Source.fromFile(fName).getLines().toList

    val name = fileContent.head.replace("name:", "").replace(",", "").strip()

    val rrgName = fileContent.tail.head.replace("patName:", "").replace(",", "").strip()
    val rrgCandidates = rrgs.filter(_.molName == rrgName)
    assert(rrgCandidates.size == 1)
    val rrg = rrgCandidates.head

    val rrgPrimNameMap = rrg.prims.map(prim => (prim.name -> prim)).toMap

    val assignmentsStrs = fileContent.take(fileContent.indexOf("dagIds:")).drop(3)
    val assignments = assignmentsStrs.map {
      s => {
        val aStr = s.replace(",", "").strip().split("->")
        assert(aStr.size == 2)

        val pnName = aStr(0).strip()
        val nName = aStr(1).strip()

        val pn = rrgPrimNameMap(pnName)
        if(nName.contains("__")) {
          val nodeName = nName.split("__")(0)
          val pin = PortInstanceParser(nName.split("__")(1))

          ExtAssignment(pn, PortNodeID(nodeName, pin.id, pin.loc))
        } else {
          LocAssignment(pn, g(nName))
        }
      }
    }

    val mapping = Mapping(assignments.toSet)

    val dagIdStr = fileContent.drop(fileContent.indexOf("dagIds:")+1).filter(_.strip().nonEmpty)
    assert(dagIdStr.size <= 1)

    val dagIds = if(dagIdStr.isEmpty) {
      Set()
    } else {
      dagIdStr.head.split(",").toList.map {
        s => {
          val str = s.replace(",", "").split('[')(0)
          val loc = s.replace(",", "").split('[')(1).replace("]", "").toInt

          DagId(str, loc)
        }
      }.toSet
    }
    
    Molecule(name, rrg, mapping, dagIds)
  }
}

case class Molecule(
  name: String,
  pat: RRG,
  mapping: Mapping,
  dagIds: Set[DagId]
) extends AbsMolecule {
  type T = Molecule

  def withName(nName: String): Molecule = Molecule(nName, pat, mapping, dagIds)

  def findMapping(
      g: ElasticGraph,
      router: Router,
      params: GlobalParamsInst,
      log: Boolean
  ): Option[MappedMolecule[Molecule]] = {
    AbsMolecule.findMapping(g, router, params, this, log)
  }

  def serialize(): String = {
    val nameStr = "name: " + name
    val patStr = "patName: " + pat.molName
    val mappingStr = "mapping:\n  " + mapping.assignments.map {
      case LocAssignment(pn, n) => pn.name + " -> " + n.name
      case ExtAssignment(pn, value) => pn.name + " -> " + value.toString()
    }.mkString(",\n  ")
    val dagIdsStr = "dagIds:\n  " + dagIds.map(_.stringRep).mkString(", ")

    (nameStr :: patStr :: mappingStr :: dagIdsStr :: Nil).mkString(",\n")
  }

  override def +(a: packerv2.LocAssignment): Molecule = {
    val nDagIds = a.pn.dagIds.fold(dagIds)(
      d => dagIds + d
    )
    Molecule(name, pat, mapping + a, nDagIds)
  }

  override def -(a: packerv2.LocAssignment): Option[Molecule] = {
    val nMapping = mapping - a
    val nDagIds = nMapping.locAssignments.map(_._1.dagIds).flatten.toSet

    if (nMapping.locAssignments.isEmpty) {
      None
    } else {
      Some(Molecule(name, pat, nMapping, nDagIds))
    }
  }
}

case class MappedMolecule[M <: AbsMolecule](
    m: M,
    assignments: Set[PhysicalAssignment]
) extends Ordered[MappedMolecule[M]] {
  val pat = m.pat
  val name = m.name
  val mapping = m.mapping
  val dagIds = m.dagIds

  lazy val rrAssignments = assignments
    .collect {
      case rra: RRAssignment => rra
    }
    .groupBy(_.rr)
  lazy val rrsas = assignments
    .collect {
      case rrea: RREdgeAssignment => rrea
    }
    .groupBy(_.src)

  lazy val numPrims = m.mapping.assignments.size

  lazy val numExtEdges = {
    assignments
      .collect {
        case ea: ExtAssignment => ea
      }
      .map(_.value)
      .filter(
        value => !m.mapping.nodeNames.contains(value.nodeName)
      )
      .toSet
      .size
  }

  lazy val numIntEdges = {
    assignments
      .collect {
        case rrea: RREdgeAssignment => rrea
      }
      .filter(_.src.isInstanceOf[RRSource])
      .filter(
        rrea => m.mapping.nodeNames.contains(rrea.sink.nodeName)
      )
      .toSet
      .size
  }

  def beforeLegalizerFeedbacks = {
    assignments
      .collect {
        case rrea: RREdgeAssignment => rrea
      }
      .filter(_.src.isInstanceOf[RRGI])
      .map {
        rrea =>
          {
            (rrea.value, rrea.sink)
          }
      }
  }

  def afterLegalizerFeedbacks = {
    assignments
      .collect {
        case rrea: RREdgeAssignment => rrea
      }
      .filter(
        rrea => rrea.dst.isInstanceOf[RRSink] && m.pat.srcSinkToPrim(rrea.dst.asInstanceOf[RRSink]).isIo
      )
      .filter(
        rrea => m.mapping.nodeNames.contains(rrea.sink.nodeName)
      )
      .map {
        rrea =>
          {
            (rrea.value, rrea.sink)
          }
      }
  }

  lazy val feedbackEdges = beforeLegalizerFeedbacks ++ afterLegalizerFeedbacks
  lazy val numFbkEdges = feedbackEdges.size

  // TODO also add the number of absorbed edges
  def objective(): Double = {
    val cSize = 1.0
    val cFbk = 0.0
    val cExt = 0.0
    val cInt = 0.0

    (cSize * numPrims) + (cInt * numIntEdges) - (cFbk * numFbkEdges) - (cExt * numExtEdges)
  }

  def compare(that: MappedMolecule[M]): Int = {
    if (this.objective() > that.objective()) {
      1
    } else if (this.objective() < that.objective()) {
      -1
    } else {
      0
    }
  }

  def withName(nName: String): MappedMolecule[M] = {
    // TODO clean this up
    MappedMolecule(m.withName(nName).asInstanceOf[M], assignments)
  }

  def isValid(): Boolean = assignments.nonEmpty

  def +(a: LocAssignment): M = {
    (m + a).asInstanceOf[M] // TODO clean this up
  }

  def isFeedback(e: ElasticEdge): Boolean = {
    feedbackEdges.contains((e.fromPort.nodeID(), e.toPort.nodeID()))
  }

  def stepPath(rra: RRAssignment, n: Map[RR, Set[RR]]): Set[RRAssignment] = {
    n.getOrElse(rra.rr, Nil)
      .filter(
        nRR => rrAssignments.getOrElse(nRR, Nil).filter(_.value == rra.value).nonEmpty
      )
      .map(RRAssignment(_, rra.value))
      .toSet
  }

  def followPath(rra: RRAssignment, n: Map[RR, Set[RR]]): Set[LocAssignment] = {
    if (!n.contains(rra.rr)) {
      val prim = pat.srcSinkToPrim(rra.rr.asInstanceOf[RRSourceSink])
      if (m.mapping.locAssignments.contains(prim)) {
        Set(LocAssignment(prim, m.mapping.locAssignments(prim)))
      } else {
        Set()
      }
    } else {
      n(rra.rr)
        .filter(rrAssignments.contains(_))
        .filter(!_.isInstanceOf[RRGI])
        .map {
          recRR =>
            {
              rrAssignments(recRR).filter(_.value == rra.value).map(followPath(_, n)).flatten
            }
        }
        .flatten
    }
  }

  // TODO classify edge, etc...
  def startOfRoute(pn: RRGPrim, e: ElasticEdge): RREdgeAssignment = {
    lazy val destIn = !m.mapping.nodeNames.contains(e.toPort.thisNode)

    val candidates = pn.pinToSrcSink.map(_._2).toSet.map(rrsas.getOrElse(_, Set())).flatten.filter {
      rrsa =>
        {
          if (m.isExt(e) && destIn) { // only one path to exit
            val sinkNotIn = !m.mapping.nodeNames.contains(rrsa.sink.nodeName)
            (rrsa.value == e.fromPort.nodeID()) && sinkNotIn
          } else {
            (rrsa.value == e.fromPort.nodeID()) && (rrsa.sink == e.toPort.nodeID())
          }

        }
    }

    assert(candidates.size == 1, candidates.size)
    candidates.head
  }

  def getRRs(edge: ElasticEdge): Set[RREdgeAssignment] = {
    assignments
      .collect {
        case rrea: RREdgeAssignment => rrea
      }
      .filter(
        rrea => (rrea.value == edge.fromPort.nodeID()) && (rrea.sink == edge.toPort.nodeID())
      )
  }

  def localReachablePrimitives(a: LocAssignment): Set[LocAssignment] = {
    assignments
      .collect {
        case rra: RRAssignment => rra
      }
      .collect {
        case rra: RRAssignment if (rra.rr.isInstanceOf[RRSink] || rra.rr.isInstanceOf[RRSource]) => rra
      }
      .filter {
        case rra: RRAssignment => pat.srcSinkToPrim(rra.rr.asInstanceOf[RRSourceSink]).name == a.pn.name
      }
      .map {
        rra =>
          {
            rra.rr match {
              case sink: RRSink     => followPath(rra, pat.preds)
              case source: RRSource => followPath(rra, pat.succs)
              case other            => ???
            }
          }
      }
      .flatten
  }

  def findRouteSource(value: PortNodeID): RRAssignment = {
    val candidates = assignments
      .collect {
        case rra: RRAssignment => rra
      }
      .filter(_.value == value)
      .collect {
        case rra @ RRAssignment(RRSource(_, _, _), value) => rra
      }

    assert(candidates.size == 1)
    candidates.head
  }

  def isConnected(): Boolean = {
    def rec(starts: Set[LocAssignment], seen: Set[LocAssignment]): Boolean = {
      if (seen.size == m.mapping.locAssignments.size) {
        true
      } else if (starts.isEmpty) {
        false
      } else {
        val la = starts.head
        val nReachable = localReachablePrimitives(la).filter(!_.pn.annos.contains(ATileIo))
        val nSeen = seen ++ nReachable
        val nStarts = starts.tail ++ (nReachable -- seen)

        rec(nStarts, nSeen)
      }
    }

    val h = m.mapping.locAssignments.head
    val start = Set(LocAssignment(h._1, h._2))
    rec(start, start)
  }

  def isDisconnected(): Boolean = {
    !isConnected()
  }
}

object Packer {
  def findAllMatchesRec[T](
      params: GlobalParamsInst,
      rrgs: Seq[RRG],
      cs: CompilerState[RootPb],
      nodes: List[Node],
      log: Boolean
  ): Seq[MappedMolecule[Molecule]] = {
    println(GREEN + "Mining patterns " + params.parallel + RESET)

    val enumParams = nodes
      .map { // .filter(_.name == "fork10")
        n =>
          {
            rrgs.map { // .filter(_.molName.contains("Control"))
              rrg =>
                {
                  rrg.roots.map { // .filter(_.block.prim.isInstanceOf[Fork])CntrlMerge
                    r =>
                      {
                        (n, r, rrg)
                      }
                  }
                }
            }
          }
      }
      .flatten
      .flatten
      .toList

    if (params.parallel && !log) {
      val testMols = Future.sequence(enumParams.map {
        (n, r, rrg) =>
          {
            val f = Future {
              val log = new FileOutputStream(new File("/dev/null"))
              val t = Console.withOut(log) {
                Enumerator()(params, n, r, cs, rrg).toList
              }

              log.close()

              t
            }

            f.onComplete {
              case Failure(exception) => exception
              case Success(mols) =>
                mols.foreach(
                  m => MoleculePrinter(params, m.withName("enum_" + m.name), true)
                )
            }

            f
          }
      })

      // TODO this should be an onComplete and the rest is passed as a callback...
      Await.result(testMols, Duration.Inf).flatten
    } else {
      // TODO If we make the above parallel, adapt the interface of this too...
      enumParams.map {
        (n, r, rrg) =>
          {
            Enumerator()(params, n, r, cs, rrg).toList
          }
      }.flatten
    }
  }

  def findAllMatches[T](
      params: GlobalParamsInst,
      rrgs: Seq[RRG],
      cs: CompilerState[RootPb]
  ): Seq[MappedMolecule[Molecule]] = {
    // println(GREEN + "Mining patterns" + RESET)

    findAllMatchesRec(params, rrgs, cs, cs.crkt.nodes.map(_._2).toList, false)
  }

  def findForkMatches(
      breakFork: Node,
      params: GlobalParamsInst,
      rrgs: Seq[RRG],
      g: ElasticGraph
  ): Seq[MappedMolecule[Molecule]] = {
    assert(breakFork.nType.isInstanceOf[Fork])

    val candidates = rrgs
      .filter(_.t.annotations.contains(ASpanning))
      .map {
        rrg =>
          {
            rrg.roots
              .filter(
                r => r.canMap(breakFork)
              )
              .map {
                r =>
                  {
                    val molName = rrg.molName + "_" + r.name + "_" + breakFork.name
                    val dIds = if (r.dagIds.nonEmpty) Set(r.dagIds.get) else Set()
                    val mol = Molecule(molName, rrg, Mapping(Set(LocAssignment(r, breakFork))), dIds)

                    val callback = (r: Router) => {
                      val res = mol.route(r, params, false)
                      if (res.isEmpty) {
                        None
                      } else {
                        Some(res)
                      }
                    }

                    val assignments = Router.withFullILP(g, mol, callback, KeepAll).get
                    MappedMolecule(mol, assignments)
                  }
              }
          }
      }
      .flatten

    if (candidates.isEmpty) {
      val inPort = breakFork.ports.map(_._2).flatten.filter(_.id.pt == PTInput).head
      val outPorts = breakFork.ports.map(_._2).flatten.filter(_.id.pt == PTOutput)

      val keptPorts = outPorts.filter(_.loc < (outPorts.size / 2))
      val movedPorts = outPorts.filter(_.loc >= (outPorts.size / 2))

      val nExtPort = Port(movedPorts.head.id, "", Map(), breakFork.name, Map(), keptPorts.size)
      val nTargetPort = Port(movedPorts.head.id.flipped(), "", Map(), breakFork.name + "_break", Map(), 0)

      nExtPort.distPorts = Map((nTargetPort.nodeID() -> nTargetPort))
      nTargetPort.distPorts = Map((nExtPort.nodeID() -> nExtPort))

      val nBreakOutPorts = movedPorts.zipWithIndex.map {
        (p, i) =>
          {
            val nP = Port(p.id, p.name, p.attr, p.thisNode + "_break", p.distPorts, i)
            Port.updateDistPorts(p, nP)

            nP
          }
      }

      val nLocPorts = (inPort :: nExtPort :: keptPorts.toList).groupBy(_.id)
      val nBreakPorts = (nTargetPort :: nBreakOutPorts.toList).groupBy(_.id)

      val keptParams = ForkParams(breakFork.nType.p.asInstanceOf[ForkParams].width, keptPorts.size + 1, EagerFork)
      val breakPrim = Fork(keptParams)
      val nBreakFork = Node(breakFork.name, breakPrim, Map(), Map(), Set(), nLocPorts)

      val nFParams = ForkParams(breakFork.nType.p.asInstanceOf[ForkParams].width, nBreakOutPorts.size, EagerFork)
      val nBreakPrim = Fork(nFParams)
      val nBreakForkNode = Node(breakFork.name + "_break", nBreakPrim, Map(), Map(), Set(), nBreakPorts)

      val nNodes = (nBreakForkNode :: nBreakFork :: Nil)
      val nG = ElasticGraph(
        g.nodes ++ nNodes.map(
          n => (n.name, n)
        )
      )

      nNodes.map(findForkMatches(_, params, rrgs, nG)).flatten
    } else {
      candidates
    }
  }

  def breakFork(
      mm: MappedMolecule[Molecule],
      la: LocAssignment,
      archOuts: Int,
      crktOuts: Int
  ): (LocAssignment, Node) = {
    println(RED + "Breaking up a fork: " + la.n.name + " onto " + la.pn.name + "." + RESET)

    // println(la.n.nType)
    // println(la.pn.block.prim)

    val pn = la.pn
    val n = la.n

    assert(n.nType.p.asInstanceOf[ForkParams].width == 0, "Can only break 0-width fork so far...")

    val src = pn.pinToSrcSink
      .map(_._2)
      .collect {
        case src: RRSource => src
      }
      .toSet
      .filter(_.rrType.pmw.pb == Hs)

    assert(src.size == 1)

    val archOutEdges = mm
      .rrsas(src.head)
      .map(
        rrea => (rrea.value, rrea.sink)
      )
      .filter(
        (src, dst) => mm.m.isExt(src, dst)
      )

    assert(archOutEdges.size >= 1, "Cannot break fork: " + n.name + " mapped to " + pn.name)

    val exitEdge = archOutEdges.head
    val oEdges = mm
      .rrsas(src.head)
      .map(
        rrea => (rrea.value, rrea.sink)
      )
      .filter(_ != exitEdge)

    val keptPortIds = oEdges.map(_._1) // + n.ports.map(_._2).flatten.filter(_.id.pt == PTInput).head.nodeID()

    val (keptPorts, movedPorts) = n.ports
      .map(_._2)
      .flatten
      .partition(
        p => keptPortIds.contains(p.nodeID())
      )

    val nExtPort = Port(movedPorts.head.id, "", Map(), n.name, Map(), keptPorts.size)
    val nTargetPort = Port(movedPorts.head.id.flipped(), "", Map(), n.name + "_break", Map(), 0)

    nExtPort.distPorts = Map((nTargetPort.nodeID() -> nTargetPort))
    nTargetPort.distPorts = Map((nExtPort.nodeID() -> nExtPort))

    val nLocOutPorts = keptPorts.toList.zipWithIndex.map {
      (p, i) =>
        {
          val nP = Port(p.id, p.name, p.attr, p.thisNode, p.distPorts, i)
          Port.updateDistPorts(p, nP)

          nP
        }
    }

    val inPort = n.ports.map(_._2).flatten.filter(_.id.pt == PTInput).head
    val nLocPorts = (inPort :: nExtPort :: nLocOutPorts).groupBy(_.id)

    val nBreakOutPorts = movedPorts.filter(_.id.pt == PTOutput).zipWithIndex.map {
      (p, i) =>
        {
          val nP = Port(p.id, p.name, p.attr, p.thisNode + "_break", p.distPorts, i)
          Port.updateDistPorts(p, nP)

          nP
        }
    }

    val nBreakPorts = (nTargetPort :: nBreakOutPorts.toList).groupBy(_.id)

    // TODO add proper info in mlirAttr, if we ever want to print this back to MLIR
    val breakPrim = Fork(ForkParams(n.nType.p.asInstanceOf[ForkParams].width, nBreakOutPorts.size, EagerFork))
    val breakForkNode = Node(n.name + "_break", breakPrim, Map(), Map(), Set(), nBreakPorts)

    val nodePrim = Fork(ForkParams(n.nType.p.asInstanceOf[ForkParams].width, nLocOutPorts.size, EagerFork))
    val nNode = Node(n.name, nodePrim, n.mlirAttr, n.attr, Set(), nLocPorts)

    val nAssignment = LocAssignment(pn, nNode)

    (nAssignment, breakForkNode)
  }

  def fixupForkPortOveruseMolecule(
      mm: MappedMolecule[Molecule],
      rrgs: Seq[RRG],
      la: LocAssignment
  ): (LocAssignment, Option[Node]) = {
    val pn = la.pn
    val n = la.n

    if (pn.block.prim.isInstanceOf[Fork]) {
      val src = pn.pinToSrcSink
        .map(_._2)
        .collect {
          case src: RRSource => src
        }
        .toSet
        .filter(_.rrType.pmw.pb == Hs)

      assert(src.size == 1, src)

      val archOuts = mm.rrsas(src.head).size
      val crktOuts = n.ports.map(_._2).flatten.filter(_.id.pt == PTOutput).size

      if (archOuts != crktOuts) {
        assert(archOuts < crktOuts)

        // println("arch: " + archOuts)
        // println("crkt: " + crktOuts)
        // println(n)

        val (nLa, breakForkNode) = breakFork(mm, la, archOuts, crktOuts)

        (nLa, Some(breakForkNode))
      } else {
        (la, None)
      }
    } else {
      (la, None)
    }
  }

  // TODO should be recursive if need to break multiple times...
  def fixupForkPortOveruse(
      params: GlobalParamsInst,
      rrgs: Seq[RRG],
      packed: List[MappedMolecule[Molecule]]
  ): Seq[MappedMolecule[Molecule]] = {
    val nNodeList = ListBuffer[Node]()

    // Break forks that cannot reach the output on all ports and reccord added nodes
    val updatedMols = packed.map {
      mm =>
        {
          var updated = false
          assert(mm.m.mapping.assignments.collect {
            case ea: ExtAssignment => ea
          }.isEmpty)

          val nAssignments: Set[Assignment] = mm.m.mapping.locAssignments.map {
            (pn, n) =>
              {
                val la = LocAssignment(pn, n)
                val (nLa, nForkNode) = fixupForkPortOveruseMolecule(mm, rrgs, la)

                if (nForkNode.nonEmpty) {
                  updated = true
                  nNodeList += nForkNode.get
                }

                nLa
              }
          }.toSet

          if (updated) {
            Molecule(mm.m.name, mm.m.pat, Mapping(nAssignments), mm.m.dagIds)
          } else {
            mm.m
          }
        }
    }

    // Update core datastructures
    val nG = ElasticGraph(
      Legalizer.extractCrkt(updatedMols).nodes ++ nNodeList.map(
        n => (n.name, n)
      )
    )
    DotPrinter(params.buildDir + "/lowering/brokenforks.dot")(nG, Set())

    // Reroute molecules to fixup potential renames in the outputs
    val nMM = updatedMols.map {
      nM =>
        {
          val callback = (r: Router) => {
            val res = nM.route(r, params, true)
            if (res.isEmpty) {
              scala.sys.error("Unexpected router failure.")
            } else {
              Some(res)
            }
          }

          val rrAssignments = Router.withFullILP(nG, nM, callback, KeepAll).get
          MappedMolecule(nM, rrAssignments)
        }
    }

    // Find fork matches
    val forkMols = nNodeList.toList.map {
      n =>
        {
          findForkMatches(n, params, rrgs, nG)
        }
    }.flatten

    val allMols = (nMM ++ forkMols.toList)
    val finalG = Legalizer.extractCrkt(allMols.map(_.m))

    Rewriter(params, finalG, (nMM ++ forkMols.toList))
  }

  def shouldFixupPortOveruse(
      params: GlobalParamsInst,
      packed: List[MappedMolecule[Molecule]]
  ): Boolean = {
    packed.exists {
      mm =>
        {
          mm.m.mapping.locAssignments.filter(_._1.block.prim.isInstanceOf[Fork]).exists {
            (pn, n) =>
              {
                val src = pn.pinToSrcSink
                  .map(_._2)
                  .collect {
                    case src: RRSource => src
                  }
                  .toSet
                  .filter(_.rrType.pmw.pb == Hs)

                assert(src.size == 1, "" + src + " for " + pn.name)

                if (!mm.rrsas.contains(src.head)) {
                  MoleculePrinter(params, mm.withName("debug_fixup_fork"), false)

                  ???
                }

                val archOuts = mm.rrsas(src.head).size
                val crktOuts = n.ports.map(_._2).flatten.filter(_.id.pt == PTOutput).size

                (archOuts != crktOuts)
              }
          }
        }
    }
  }

  def findBufferMatches(
      params: GlobalParamsInst,
      rrgs: Seq[RRG],
      bufferCs: CompilerState[RootPb]
  ): Seq[MappedMolecule[Molecule]] = {
    val buffers = bufferCs.crkt.nodes.map(_._2).filter(_.nType.isBuf)

    findAllMatchesRec(params, rrgs, bufferCs, buffers.toList, false)
  }

  // overwrites the original mlir witht the buffered one
  def buffer(params: GlobalParamsInst, fName: String): Option[ElasticGraph] = {
    assert(params.cp.size == 1)
    val targetCP = if(params.timings) {
      params.cp.head
    } else {
      // Use very high clock period for non-timing driven: just want correct buffers
      50
    }

    val bufName = params.buildMlirDir + "/" + params.bench + "_buf.mlir"
    val finalName = params.buildMlirDir + "/" + params.bench + ".mlir"

    Seq(
      "cp",
      params.mlirDir + "/std_dyn_transformed.mlir",
      params.buildMlirDir + "/std_dyn_transformed.mlir"
    ).!

    val tim = if(params.timings) {
      "timings"
    } else {
      // Place buffers with default timing models in non-timing driven path
      "defaultTimings"
    }

    val bufferScript = Seq(
      GlobalParams.bufferScript,
      params.benchSrcDir,
      params.buildMlirDir,
      params.bench,
      fName,
      bufName,
      targetCP.toString(),
    )

    println(bufferScript.mkString(" "))

    val canonScript = Seq(
      GlobalParams.canonScript,
      params.benchSrcDir,
      params.buildMlirDir,
      params.bench,
      bufName,
      finalName
    )

    if (bufferScript.! == 0) {
      canonScript.!

      Some(MlirToCrktConverter(params, MLIRParser(finalName)))
    } else {
      None
    }
  }

  def addBuffers(
      params: GlobalParamsInst,
      rrgs: Seq[RRG],
      cs: CompilerState[RootPb],
      packed: List[MappedMolecule[Molecule]]
  ): Option[Seq[MappedMolecule[Molecule]]] = {
    println(GREEN + "Buffering" + RESET)
    val packedG = Legalizer.extractCrkt(packed.map(_.m))

    val annotatedG = FindEdgeConstraints(params, AnnotateMolecules(packedG, packed), packed.toList)
    DotPrinter(params.buildDir + "/lowering/packed_delays.dot")(annotatedG, Set())

    val mlir = AnnotateBufferConstraints(params, annotatedG)
    val fName = params.preBufAnnos
    MLIRPrinter(fName, mlir)

    val nGOpt = buffer(params, fName)

    nGOpt.map {
      nG =>
        {
          DotPrinter(params.buildDir + "/lowering/buffered.dot")(nG, Set())

          val lowered = CompilerState.lowerCircuit(nG, cs.archMasks, false)(params)
          DotPrinter(params.buildDir + "/lowering/buffered_lowered.dot")(lowered, Set())

          val extendedMolecules = ExtendPacking(params, lowered, packed)

          val bufferCs = CompilerState(cs.tiles, lowered, cs.archMasks, Nil)
          val bufferMolecules = findBufferMatches(params, rrgs, bufferCs)

          Rewriter(params, lowered, (bufferMolecules ++ extendedMolecules))
        }
    }
  }

  def cacheMolecules(params: GlobalParamsInst, mols: Seq[MappedMolecule[Molecule]]): Unit = {
    mols.map(_.m).zipWithIndex.map {
      (mol, i) => {
        val fName = params.packingCacheDir + "/" + i + ".cache"

        val f = Util.writeOpen(fName)
        f.write(mol.serialize())
        f.close()
      }
    }
  }

  def readMoleculeCache (
    params: GlobalParamsInst,
    cs: CompilerState[RootPb],
    rrgs: Seq[RRG]
  ): Seq[MappedMolecule[Molecule]] = {
    val dir = new File(params.packingCacheDir)

    dir.listFiles.toSeq.filter(_.toString().contains(".cache")).map {
      fName => {
        val m = Molecule.fromFile(fName.toString(), rrgs, cs.crkt)

        val callback = (r: Router) => {
          val res = m.route(r, params, true)
          if (res.isEmpty) {
            None
          } else {
            Some(res)
          }
        }

        val assignments = Router.withFullILP(cs.crkt, m, callback, KeepMixed).get

        MappedMolecule(m, assignments)
      }
    }
  }

  def plainPacking (
    params: GlobalParamsInst,
    cs: CompilerState[RootPb],
    rrgs: Seq[RRG]
  ): Seq[MappedMolecule[Molecule]] = {
    ("mkdir -p " + params.packingCacheDir).!

    // TODO also cache the routing solution, to ensure it stays the same...
    if(new File(params.packingCacheDir).listFiles.filter(_.toString().contains(".cache")).nonEmpty) {
      readMoleculeCache(params, cs, rrgs)
    } else {
      val matches = findAllMatches(params, rrgs, cs)

      val selectedMatches = Rewriter(params, cs.crkt, matches)
      val gAnnotated = AnnotateMolecules(cs.crkt, selectedMatches)
      val feedbackEdges = selectedMatches.map(_.feedbackEdges).flatten.toSet

      DotPrinter(params.buildDir + "/lowering/packed.dot")(gAnnotated, feedbackEdges)
      AnalysisPacking(params, cs.crkt, selectedMatches.toList, "pre_buf")

      cacheMolecules(params, selectedMatches)

      selectedMatches
    }
  }

  def bufferedPacking(
      params: GlobalParamsInst,
      cs: CompilerState[RootPb],
      rrgs: Seq[RRG],
      plainMatches: Seq[MappedMolecule[Molecule]]
  ): Option[Seq[MappedMolecule[Molecule]]] = {
    val bufferedMatchesOpt = addBuffers(params, rrgs, cs, plainMatches.toList)

    bufferedMatchesOpt.map {
      bufferedMatches =>
        {
          val gBuf = Legalizer.extractCrkt(bufferedMatches.map(_.m))
          val gAnnotatedBuf = AnnotateMolecules(gBuf, bufferedMatches)
          val feedbackEdgesBufs = bufferedMatches.map(_.feedbackEdges).flatten.toSet
          DotPrinter(params.buildDir + "/lowering/packed_buffers.dot")(gAnnotatedBuf, feedbackEdgesBufs)

          AnalysisPacking(params, gBuf, bufferedMatches.toList, "buf")

          bufferedMatches
        }
    }
  }

  def legalizeForks(
      params: GlobalParamsInst,
      rrgs: Seq[RRG],
      bufferedMatches: Seq[MappedMolecule[Molecule]]
  ): Seq[MappedMolecule[Molecule]] = {
    if (shouldFixupPortOveruse(params, bufferedMatches.toList)) {
      fixupForkPortOveruse(params, rrgs, bufferedMatches.toList)
    } else {
      bufferedMatches.toList
    }
  }

  def implementPacking(
      params: GlobalParamsInst,
      cs: CompilerState[RootPb],
      brokenForkMatches: Seq[MappedMolecule[Molecule]]
  ) = {
    val legalMatches = Legalizer(0)(params, brokenForkMatches).toList
    val nG = Legalizer.extractCrkt(legalMatches.map(_.m))

    val mMatches = legalMatches.map {
      mm =>
        {
          ImplementMolecule(mm)
        }
    }

    CompilerState(cs.tiles, nG, cs.archMasks, mMatches)
  }

  def apply(
      params: GlobalParamsInst,
      cs: CompilerState[RootPb],
      tiles: Map[String, Tile]
  ): Option[CompilerState[RootPb]] = {
    val rrgs = cs.tiles
      .map(_._2)
      .map(
        pb => RRG.gen(pb, tiles(pb.name), params)
      )
      .toSeq // TODO collapse multiplexers
    rrgs.foreach(RRGPrinter(params, _))

    val plainMatches = plainPacking(params, cs, rrgs)
    val bufferedMatchesOpt = bufferedPacking(params, cs, rrgs, plainMatches)

    bufferedMatchesOpt.map {
      bufferedMatches =>
        {
          val brokenForkMatches = legalizeForks(params, rrgs, bufferedMatches)
          implementPacking(params, cs, brokenForkMatches)
        }
    }
  }
}
