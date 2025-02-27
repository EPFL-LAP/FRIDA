package packerv2

import crkt._
import arch.Pin
import arch.D
import arch.Hs
import arch.PTInput
import arch.PortType
import arch.PTOutput
import arch.BlockPortID
import core.Namer
import frontend.GlobalParamsInst
import printers.MoleculePrinter
import core.ATileIo
import packerv2.Enumerator.assignmentString
import packerv2.Enumerator.mappingString
import util.Util
import archs._
import core.AForkNumExits

import collection.mutable.{Map => MMap}
import collection.mutable.{Set => MSet}
import scala.util.{Try, Success, Failure}
import io.AnsiColor._
import sys.process._

import com.gurobi.gurobi._

sealed trait ForkHandling
case object KeepOne extends ForkHandling // Keep one external output per fork
case object KeepAll extends ForkHandling // Keep as much as you can
case object KeepMixed extends ForkHandling // Keep all external outputs per fork

object EdgeInfo {
  def externalEdges(g: ElasticGraph, m: AbsMolecule): Seq[ElasticEdge] = {
    m.mapping.nodes
      .map(_.ports.map(_._2).flatten)
      .flatten
      .map {
        p =>
          {
            p.distPorts
              .map(_._2)
              .filter(
                dp => !m.mapping.contains(g(dp.thisNode))
              )
              .map {
                dp =>
                  {
                    ElasticEdge(p, dp)
                  }
              }
          }
      }
      .flatten
      .toSeq
  }

  def getForkDirectExists(
      m: AbsMolecule,
      pn: RRGPrim
  ): Int = {
    pn.block.prim match {
      case Fork(_) => {
        pn.annos
          .collect {
            case a: AForkNumExits => a
          }
          .headOption
          .fold(0)(_.num)
      }
      case other => ???
    }
  }

  def keepOneForkOutput(
      g: ElasticGraph,
      m: AbsMolecule,
      extEdges: Seq[ElasticEdge],
      forkBehavior: ForkHandling
  ): Seq[ElasticEdge] = {
    forkBehavior match {
      case KeepAll => extEdges

      case KeepOne => {
        extEdges
          .groupBy(_.fromPort.thisNode)
          .map {
            (fromNodeName, edgesFromNode) =>
              {
                val fromNode = g(fromNodeName)

                if (m.mapping.contains(fromNode) && fromNode.nType.isInstanceOf[Fork]) {
                  edgesFromNode.sortBy(_.fromPort.loc).head :: Nil
                } else {
                  edgesFromNode
                }
              }
          }
          .flatten
          .toSeq
      }

      case KeepMixed => {
        // TODO Should be better handled with proper handling of variable sized components
        extEdges
          .groupBy(_.fromPort.thisNode)
          .map {
            (fromNodeName, edgesFromNode) =>
              {
                val fromNode = g(fromNodeName)

                if (m.mapping.contains(fromNode) && fromNode.nType.isInstanceOf[Fork]) {
                  val pn = m.mapping.invAssignments(fromNode)
                  val existsPossible = getForkDirectExists(m, pn)

                  assert(existsPossible > 0, pn.name + " had " + existsPossible + " exits possible.")

                  edgesFromNode.sortBy(_.fromPort.loc).take(existsPossible)
                } else {
                  edgesFromNode
                }
              }
          }
          .flatten
          .toSeq
      }
    }
  }

  def keepOneDataOutput(g: ElasticGraph, m: AbsMolecule, extEdges: Seq[ElasticEdge]): Seq[ElasticEdge] = {
    extEdges
      .groupBy(_.fromPort)
      .map {
        (srcP, edges) =>
          {
            val fromNode = g(srcP.thisNode)

            if (m.mapping.contains(fromNode)) {
              srcP.id.pmw.pb match {
                case D => {
                  val kept = edges.sortBy(_.toPort.thisNode).head
                  edges.sortBy(_.toPort.thisNode).head :: Nil
                }

                case others => edges
              }
            } else {
              edges
            }
          }
      }
      .flatten
      .toSeq
  }

  def internalEdges(g: ElasticGraph, m: EnumMolecule): Seq[ElasticEdge] = {
    m.last.n.ports
      .map(_._2)
      .flatten
      .map {
        p =>
          {
            p.distPorts
              .map(_._2)
              .filter(
                dp => m.mapping.contains(g(dp.thisNode))
              )
              .map {
                dp =>
                  {
                    ElasticEdge(p, dp)
                  }
              }
          }
      }
      .flatten
      .toSeq
  }

  def concreteExtEdges(g: ElasticGraph, m: AbsMolecule, forkBehavior: ForkHandling): Seq[ElasticEdge] = {
    keepOneDataOutput(g, m, keepOneForkOutput(g, m, externalEdges(g, m), forkBehavior))
  }

  def apply(g: ElasticGraph, m: EnumMolecule): EdgeInfo = {
    val oldExt = (m - m.last) match {
      case Some(em) => concreteExtEdges(g, em, KeepOne).toSet
      case None     => Set()
    }

    val newExt = concreteExtEdges(g, m, KeepOne).toSet

    val addedLocal = internalEdges(g, m)

    val removed = (oldExt.toSet -- newExt.toSet)
    val addedExt = (newExt.toSet -- oldExt.toSet)

    val ef = EdgeInfo(addedLocal.toSet, addedExt, removed)

    // if(m.last.n.name.contains("sink")) {
    //   println(ef)
    //   ???
    // }

    ef
  }
}

case class EdgeInfo(
    localAdded: Set[ElasticEdge],
    extAdded: Set[ElasticEdge],
    extRemoved: Set[ElasticEdge]
) {
  override def toString(): String = {
    "local:\n  " + localAdded.mkString("\n  ") + "\n"
      + "extAdded:\n  " + extAdded.mkString("\n  ") + "\n"
      + "extRemoved:\n  " + extRemoved.mkString("\n  ")
  }
}

object Gurobi {
  def apply(): Gurobi = Gurobi(
    MMap[GRBVar, ILPVariable](),
    MMap[ILPVariable, GRBVar](),
    MMap[ILPVariable, Int](),
    MMap[ILPVariable, GRBVar](),
    MMap[ILPConstraint, GRBConstr](),
    MMap[ILPConstraint, Int]()
  )
}

case class Gurobi(
    vars: MMap[GRBVar, ILPVariable],
    invVars: MMap[ILPVariable, GRBVar],
    varsRefCount: MMap[ILPVariable, Int],
    obj: MMap[ILPVariable, GRBVar],
    constrs: MMap[ILPConstraint, GRBConstr],
    constrsRefCount: MMap[ILPConstraint, Int]
) {
  val model = {
    val env = new GRBEnv(true)
    env.set("OutputFlag", "0")
    env.start()
    new GRBModel(env)
    // val model = new GRBModel(new GRBEnv())
  }

  var constrId: Int = 0

  def getConstrId(): Int = {
    val id = constrId
    constrId += 1

    id
  }

  def getVar(v: ILPVariable): GRBVar = {
    invVars(v)
  }

  // Only support binary variables so far
  def addVariable(v: ILPVariable): Unit = {
    if (varsRefCount.contains(v)) {
      varsRefCount(v) = varsRefCount(v) + 1
    } else {
      val gv = model.addVar(0.0, 1.0, 0.0, GRB.BINARY, v.ilpName)

      vars(gv) = v
      invVars(v) = gv
      varsRefCount(v) = 1

      v match {
        case rra: RRAssignment => obj(rra) = gv
        case ea: ExtAssignment => obj(ea) = gv
        case other             =>
      }
    }
  }

  def removeVariable(v: ILPVariable): Boolean = {
    varsRefCount(v) = varsRefCount(v) - 1

    if (varsRefCount(v) == 0) {
      val gv = getVar(v)

      model.remove(gv)
      vars -= gv
      invVars -= v
      varsRefCount -= v

      v match {
        case rra: RRAssignment => obj -= rra
        case ea: ExtAssignment => obj -= ea
        case other             =>
      }

      true
    } else {
      false
    }
  }

  def addConstraint(constr: ILPConstraint): Unit = {
    if (!constrs.contains(constr)) {
      val expr = new GRBLinExpr()

      constr.terms.foreach {
        term =>
          {
            expr.addTerm(term._1, getVar(term._2))
          }
      }

      val gId = getConstrId()

      val gConstr = constr.rhs match {
        case d: Double      => model.addConstr(expr, constr.op, d, constr.ilpName + "_" + gId)
        case v: ILPVariable => model.addConstr(expr, constr.op, getVar(v), constr.ilpName + "_" + gId)
      }

      constrs(constr) = gConstr
      constrsRefCount(constr) = 1
    } else {
      constrsRefCount(constr) += 1
    }
  }

  def removeConstraint(constr: ILPConstraint): Unit = {
    constrsRefCount(constr) -= 1

    if (constrsRefCount(constr) == 0) {
      val gConstr = constrs(constr)
      model.remove(gConstr)

      constrs -= constr
      constrsRefCount -= constr
    }
  }

  def removeAllConstraints(constr: ILPConstraint): Unit = {
    model.getConstrs().foreach(model.remove(_))
  }

  def setInitObjective: Unit = {
    model.setObjective(new GRBLinExpr())
  }

  // TODO It seems that all RRA are not in the final objective? Why?
  def setObjective(log: Boolean): Unit = {
    val expr = new GRBLinExpr()

    obj.foreach {
      term =>
        {
          val factor = term._1 match {
            case rra: RRAssignment => {
              rra.rr match {
                case rrgi: RRGI => 100.0
                case other      => 1.0
              }
            }

            case other => 1.0
          }

          expr.addTerm(factor, term._2)
        }
    }

    model.setObjective(expr, GRB.MINIMIZE)
  }

  def writeModel(m: AbsMolecule, params: GlobalParamsInst): Unit = {
    val size = m.mapping.assignments.size
    val fName = params.buildDir + "/ilp/model" + size + ".lp"

    val s = "mkdir -p " + params.buildDir + "/ilp/"
    s.!!

    println(YELLOW + "Printing: " + fName + RESET)

    model.write(fName)
  }

  def writeIIS(m: AbsMolecule, params: GlobalParamsInst): Unit = {
    val size = m.mapping.assignments.size
    val fName = params.buildDir + "/ilp/model" + size + ".ilp"

    println(YELLOW + "Printing: " + fName + RESET)
    model.computeIIS()
    model.write(fName)
  }
}

object Router {
  // TODO properly handle fork outputs here...
  // None if router fails, success otherwise
  def withFullILP[T](
      g: ElasticGraph,
      m: AbsMolecule,
      f: (Router) => Option[T],
      forkHandling: ForkHandling
  ): Option[T] = {
    val r = Router()
    r.addAll(g, m, forkHandling)
    val res = f(r)

    r.freeILP()

    res
  }

  def apply(): Router = Router(
    Gurobi(),
    MMap[RR, ILPConstraint](),
    MMap[RRGPrim, ILPConstraint](),
    MSet[RREdgeAssignment]()
  )
}

sealed trait RStatus {
  def &(o: RStatus): RStatus
  def fold(f: () => RStatus): RStatus = {
    this match {
      case RSuccess => f()
      case RAborted => RAborted
    }
  }
}

case object RSuccess extends RStatus {
  def &(o: RStatus): RStatus = o match {
    case RSuccess => RSuccess
    case RAborted => RAborted
  }
}
case object RAborted extends RStatus {
  def &(o: RStatus): RStatus = RAborted
}

case class Router(
    model: Gurobi,
    // Keep track of the coninuously updated constraints
    roConstraints: MMap[RR, ILPConstraint],
    primExclConstraints: MMap[RRGPrim, ILPConstraint],
    // Non incrementally update variable info
    crossingEdges: MSet[RREdgeAssignment]
) {
  // TODO could also limit the PNs according to symmetry here...
  def getPotentialPNs(g: ElasticGraph, m: AbsMolecule, n: TNode, e: Option[ElasticEdge]): Set[RRGPrim] = {
    if (m.mapping.contains(n)) {
      Set(m.mapping.get(n).get)
    } else {
      assert(m.isExt(n), n.name + "sould be an IO node.")

      val res = if (m.mapping.contains(g(e.get.fromPort.thisNode))) {
        val locN = g(e.get.fromPort.thisNode)
        val locPN = m.mapping.invAssignments(locN)

        val pnSrcs = locPN.pinToSrcSink
          .map(_._2)
          .collect {
            case src: RRSource => src
          }
          .filter(
            src => src.rrType == e.get.fromPort.id
          )
          .toSet

        m.pat.getReachableIOs(pnSrcs.asInstanceOf[Set[RR]])
      } else if (m.mapping.contains(g(e.get.toPort.thisNode))) {
        val locN = g(e.get.toPort.thisNode)
        val locPN = m.mapping.invAssignments(locN)

        val pnSinks = locPN.pinToSrcSink
          .map(_._2)
          .collect {
            case sink: RRSink => sink
          }
          .filter(
            sink => sink.rrType == e.get.toPort.id
          )
          .toSet

        m.pat.getReachableIOs(pnSinks.asInstanceOf[Set[RR]])
      } else {
        ???
      }

      res
    }
  }

  def canMap(rrg: RRG, srcSink: RRSourceSink, value: PortNodeID): Boolean = {
    if (rrg.getPrim(srcSink).isIo) {
      ((srcSink.rrType.pt == value.pId.pt)
      && (srcSink.rrType.pmw.pb == value.pId.pmw.pb)
      && (srcSink.rrType.concreteWidth == value.pId.concreteWidth)
      && (srcSink.rrType.dummy == value.pId.dummy))
    } else {
      ((srcSink.rrType.pt == value.pId.pt)
      && (srcSink.rrType.pmw.pb == value.pId.pmw.pb)
      && (srcSink.rrType.concreteWidth == value.pId.concreteWidth)
      && (srcSink.rrType.dummy == value.pId.dummy)
      && (srcSink.rrType.pmw.pm == value.pId.pmw.pm))
    }
  }

  def getSources(m: AbsMolecule, pn: RRGPrim, n: TNode, value: PortNodeID): Set[RR] = {
    pn.pinToSrcSink
      .map(_._2)
      .collect {
        case src: RRSource => src
      }
      .filter {
        src =>
          {
            if (m.isExt(n)) {
              canMap(m.pat, src, value)
            } else {
              src.rrType == value.pId
            }
          }
      }
      .toSet
  }

  def getSinks(m: AbsMolecule, pn: RRGPrim, n: TNode, target: PortNodeID): Set[RR] = {
    pn.pinToSrcSink
      .map(_._2)
      .collect {
        case src: RRSink => src
      }
      .filter {
        sink =>
          {
            if (m.isExt(n)) {
              canMap(m.pat, sink, target)
            } else {
              sink.rrType == target.pId
            }
          }
      }
      .toSet
  }

  def getStarts(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, up: Boolean): Set[RR] = {
    val (start, n) = if (up) {
      val value = e.toPort.nodeID()
      val n = g.nodes(value.nodeName)

      (value, n)
    } else {
      val target = e.fromPort.nodeID()
      val n = g.nodes(target.nodeName)

      (target, n)
    }

    getPotentialPNs(g, m, n, Some(e))
      .map {
        pn =>
          {
            pn.pinToSrcSink
              .map(_._2)
              .toSet
              .collect { case sink: RRSink if (up) => sink; case src: RRSource if (!up) => src }
              .filter(
                src => if (m.isExt(n)) canMap(m.pat, src, start) else src.rrType == start.pId
              )
          }
      }
      .flatten
      .toSet
  }

  def getDirection(m: AbsMolecule, e: ElasticEdge): Boolean = {
    m.classifyEdge(e) match {
      case ELoc | EOExt => false
      case EIExt        => true
    }
  }

  def getNodeInDirection(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): TNode = {
    m.classifyEdge(e) match {
      case ELoc | EOExt => g(e.toPort.thisNode)
      case EIExt        => g(e.fromPort.thisNode)
    }
  }

  def handleContConstr[T](
      add: Boolean,
      constrs: MMap[T, ILPConstraint],
      key: T,
      addTerms: Set[(Double, ILPVariable)],
      constrInst: (Set[(Double, ILPVariable)]) => ILPConstraint
  ): Unit = {
    if (constrs.contains(key)) {
      val oldConstr = constrs(key)
      model.removeConstraint(oldConstr)
    }

    if (add) {
      val nTerms = (constrs.get(key).fold(Set())(_.terms) ++ addTerms).filter {
        (d, v) =>
          {
            model.invVars.contains(v)
          }
      }
      val constr = constrInst(nTerms)

      model.addConstraint(constr)
      constrs(key) = constr
    } else {
      val nTerms = constrs(key).terms.filter {
        (f, v) =>
          {
            if (!model.varsRefCount.contains(v) || (model.varsRefCount(v) == 0)) {
              !(addTerms.contains((f, v)))
            } else {
              true
            }
          }
      }

      assert(
        nTerms
          .map(_._2)
          .forall(
            v => model.invVars.contains(v)
          )
      )

      val nConstr = constrInst(nTerms)

      if (nTerms.nonEmpty) {
        model.addConstraint(nConstr)
        constrs(key) = nConstr
      } else {
        constrs -= key
      }
    }
  }

  def onAllAssignments[T](
      g: ElasticGraph,
      m: AbsMolecule,
      e: ElasticEdge,
      f: Assignment => T
  ): Seq[T] = {
    if (m.isExt(e)) {
      m.pat.getReachablePrims(getStarts(g, m, e, getDirection(m, e))).toSeq.map {
        prim =>
          {
            f(ExtAssignment(m, prim, e))
          }
      }
    } else {
      val dn = getNodeInDirection(g, m, e)

      m.pat.getReachablePrims(getStarts(g, m, e, getDirection(m, e))).toList.map {
        prim =>
          {
            f(LocAssignment(prim, dn))
          }
      }
    }
  }

  def onAllRRAssignments[T](
      g: ElasticGraph,
      m: AbsMolecule,
      e: ElasticEdge,
      up: Boolean,
      f: RRAssignment => T
  ): Seq[T] = {
    val value = e.fromPort.nodeID()

    val reachableRRs = if (m.isExt(e)) {
      m.pat.getReachableRRToIOs(getStarts(g, m, e, up))
    } else {
      m.pat.getReachableRRs(getStarts(g, m, e, up))
    }

    if (reachableRRs.isEmpty) {
      println
      println("empty reachable RRs")
      println("" + e + " -> " + up + ", " + m.isExt(e))
      println(getStarts(g, m, e, up))
      println("--")
      println(m.mapping.invAssignments.map(_._1.name).mkString(", "))
      println

      val (start, n) = if (up) {
        val value = e.toPort.nodeID()
        val n = g.nodes(value.nodeName)

        (value, n)
      } else {
        val target = e.fromPort.nodeID()
        val n = g.nodes(target.nodeName)

        (target, n)
      }

      println(n)
      println(getPotentialPNs(g, m, n, Some(e)))

      ???
    }

    reachableRRs.toSeq.map {
      rr =>
        {
          f(RRAssignment(rr, value))
        }
    }
  }

  def onAllRRSAssignments[T](
      g: ElasticGraph,
      m: AbsMolecule,
      e: ElasticEdge,
      up: Boolean,
      f: RREdgeAssignment => T
  ): Seq[T] = {
    val value = e.fromPort.nodeID()
    val sink = e.toPort.nodeID()

    val reachableRRs = if (m.isExt(e)) {
      m.pat.getReachableEdgesToIos(getStarts(g, m, e, up))
    } else {
      m.pat.getReachableEdges(getStarts(g, m, e, up))
    }

    reachableRRs.toSeq.map {
      e =>
        {
          f(RREdgeAssignment(e.src, e.dst, value, sink))
        }
    }
  }

  def genLastAssignment(g: ElasticGraph, m: EnumMolecule, add: Boolean): Unit = {
    if (add) {
      model.addVariable(LocAssignment(m.last.pn, m.last.n))
    } else {
      model.removeVariable(LocAssignment(m.last.pn, m.last.n))
    }
  }

  def addLastAssignment(g: ElasticGraph, m: EnumMolecule): Unit = {
    genLastAssignment(g, m, true)
  }

  def removeLastAssignment(g: ElasticGraph, m: EnumMolecule): Unit = {
    genLastAssignment(g, m, false)
  }

  def genVarAssignment(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean): Unit = {
    val callback = (a: Assignment) => {
      if (add) {
        model.addVariable(a)
      } else {
        model.removeVariable(a)
      }
    }

    onAllAssignments(g, m, e, callback)
  }

  def addVarAssignment(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genVarAssignment(g, m, e, true)
  }

  def removeVarAssignment(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genVarAssignment(g, m, e, false)
  }

  def genVarValues(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean, up: Boolean): Unit = {
    val callback = (rra: RRAssignment) => {
      if (add) {
        model.addVariable(rra)
      } else {
        model.removeVariable(rra)
      }
    }

    onAllRRAssignments(g, m, e, up, callback)
  }

  def addVarValues(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genVarValues(g, m, e, true, getDirection(m, e))
  }

  def removeVarValues(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genVarValues(g, m, e, false, getDirection(m, e))
  }

  def isCrossing(m: AbsMolecule, rrea: RREdgeAssignment): Boolean = {
    val srcIsPin = rrea.src.isInstanceOf[RRPin]
    val dstIsPin = rrea.dst.isInstanceOf[RRPin]
    lazy val samePrim = m.pat.getPrim(rrea.src.asInstanceOf[RRPin]) == m.pat.getPrim(rrea.dst.asInstanceOf[RRPin])
    lazy val isIo = m.pat.getPrim(rrea.src.asInstanceOf[RRPin]).isIo

    // println("" + rrea + " -> " + (srcIsPin && dstIsPin && samePrim && !isIo))

    srcIsPin && dstIsPin && samePrim && !isIo
  }

  def genVarValuesSinks(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean, up: Boolean): Unit = {
    val callback = (rrea: RREdgeAssignment) => {
      if (add) {
        model.addVariable(rrea)

        if (isCrossing(m, rrea)) {
          crossingEdges += rrea
        }
      } else {
        val removed = model.removeVariable(rrea)

        if (isCrossing(m, rrea) && removed) {
          crossingEdges -= rrea
        }
      }
    }

    onAllRRSAssignments(g, m, e, up, callback)
  }

  def addVarValuesSinks(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genVarValuesSinks(g, m, e, true, getDirection(m, e))
  }

  def removeVarValuesSinks(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genVarValuesSinks(g, m, e, false, getDirection(m, e))
  }

  // Only free nodes are IO maps
  def genConstrNodeMapping(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean): Unit = {
    val extValue = m.getExternalValue(e)
    val extNode = m.getExternalNode(g, e)

    assert(extValue.isDefined, "Except IO nodes, all nodes are mapped.")

    val terms = getPotentialPNs(g, m, extNode.get, Some(e)).filter(_.annos.contains(ATileIo)).map {
      pn =>
        {
          (1.0, ExtAssignment(m, pn, e))
        }
    }

    val constr = ILPConstraint(NMAP, terms.toSet, GRB.GREATER_EQUAL, 1.0)

    if (add) {
      model.addConstraint(constr)
    } else {
      model.removeConstraint(constr)
    }
  }

  def addConstrNodeMapping(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrNodeMapping(g, m, e, true)
  }

  def removeConstrNodeMapping(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrNodeMapping(g, m, e, false)
  }

  // only required for the IOs
  def genConstrPrimExcl(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean): Unit = {
    val extValue = m.getExternalValue(e)
    val extNode = m.getExternalNode(g, e)

    assert(extValue.isDefined && extNode.isDefined, "Except IO nodes, all nodes are mapped.")

    val ioPrims = getPotentialPNs(g, m, extNode.get, Some(e)).filter(_.annos.contains(ATileIo))

    ioPrims.foreach {
      ioPrim =>
        {
          val addTerms = Set((1.0 -> ExtAssignment(m, ioPrim, e).asInstanceOf[ILPVariable]))
          val constrInst = ((nTerms: Set[(Double, ILPVariable)]) => ILPConstraint(PEXCL, nTerms, GRB.LESS_EQUAL, 1.0))

          handleContConstr(add, primExclConstraints, ioPrim, addTerms, constrInst)
        }
    }
  }

  def addConstrPrimExcl(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrPrimExcl(g, m, e, true)
  }

  def removeConstrPrimExcl(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrPrimExcl(g, m, e, false)
  }

  def genConstrPrimLegality(m: AbsMolecule, e: ElasticEdge, add: Boolean): Unit = {
    val extValue = m.getExternalValue(e)
    assert(extValue.isDefined, "Except IO nodes, all nodes are mapped.")

    m.pat.prims.toSeq.filter(_.annos.contains(ATileIo)).foreach {
      pn =>
        {
          val terms = (1.0, ExtAssignment(m, pn, e)) :: Nil
          val constr = ILPConstraint(PLEG, terms.toSet, GRB.EQUAL, 0.0)

          if (add) {
            model.addConstraint(constr)
          } else {
            model.removeConstraint(constr)
          }
        }
    }
  }

  def addConstrPrimLegality(m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrPrimLegality(m, e, true)
  }

  def removeConstrPrimLegality(m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrPrimLegality(m, e, false)
  }

  def genConstrRouteOveruse(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean, up: Boolean): Unit = {
    val callback = (rra: RRAssignment) => rra

    onAllRRAssignments(g, m, e, up, callback)
      .groupBy(_.rr)
      .map(
        (k, v) => (k, v.map(_.value))
      )
      .map {
        (rr, values) =>
          {
            assert(values.size == 1)
            val value = values.head

            val addTerms = Set((1.0 -> RRAssignment(rr, value).asInstanceOf[ILPVariable]))
            val constrInst = {
              (nTerms: Set[(Double, ILPVariable)]) => ILPConstraint(RO, nTerms.toSet, GRB.LESS_EQUAL, rr.capacity)
            }

            handleContConstr(add, roConstraints, rr, addTerms, constrInst)
          }
      }
  }

  def addConstrRouteOveruse(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrRouteOveruse(g, m, e, true, getDirection(m, e))
  }

  def removeConstrRouteOveruse(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrRouteOveruse(g, m, e, false, getDirection(m, e))
  }

  def genConstrRRUsage(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean, up: Boolean): Unit = {
    def rec(rrea: RREdgeAssignment, rra: RRAssignment): Unit = {
      val terms = (1.0, rra) :: Nil
      val constr = ILPConstraint(RRU, terms.toSet, GRB.GREATER_EQUAL, rrea)

      if (add) {
        model.addConstraint(constr)
      } else {
        model.removeConstraint(constr)
      }
    }

    val callback = (rrea: RREdgeAssignment) => {
      val srcRRA = RRAssignment(rrea.src, rrea.value)
      val dstRRA = RRAssignment(rrea.dst, rrea.value)

      rec(rrea, srcRRA)
      rec(rrea, dstRRA)
    }

    onAllRRSAssignments(g, m, e, up, callback)
  }

  def addConstrRRUsage(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrRRUsage(g, m, e, true, getDirection(m, e))
  }

  def removeConstrRRUsage(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrRRUsage(g, m, e, false, getDirection(m, e))
  }

  def genConstrRoutingContinuity(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean, up: Boolean): Unit = {
    val callback = (rrea: RREdgeAssignment) => {
      val nRRs = (if (up) {
                    m.pat.preds.getOrElse(rrea.dst, Set())
                  } else {
                    m.pat.succs.getOrElse(rrea.dst, Set())
                  }).filter(
        nRR => model.varsRefCount.contains(RREdgeAssignment(rrea.dst, nRR, rrea.value, rrea.sink))
      )

      if (nRRs.nonEmpty) {
        val terms = nRRs.toSeq.map(
          nRR => (1.0, RREdgeAssignment(rrea.dst, nRR, rrea.value, rrea.sink))
        )

        val ct = if (up) RCU else RCD
        val constr = ILPConstraint(ct, terms.toSet, GRB.GREATER_EQUAL, rrea)

        if (add) {
          model.addConstraint(constr)
        } else {
          model.removeConstraint(constr)
        }
      }
    }

    onAllRRSAssignments(g, m, e, up, callback)
  }

  def addConstrRoutingContinuity(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrRoutingContinuity(g, m, e, true, getDirection(m, e))
  }

  def removeConstrRoutingContinuity(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrRoutingContinuity(g, m, e, false, getDirection(m, e))
  }

  def genConstrStarts(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean, up: Boolean): Unit = {
    val value = e.fromPort.nodeID()
    assert(value.pId.pt == PTOutput)

    val target = e.toPort.nodeID()
    assert(target.pId.pt == PTInput)

    val n = if (!up) g.nodes(value.nodeName) else g.nodes(target.nodeName)
    val pns = getPotentialPNs(g, m, n, Some(e))

    pns.foreach {
      pn =>
        {
          val sa = Assignment(m, pn, n, e)

          val startRRs = if (!up) {
            getSources(m, pn, n, value)
          } else {
            getSinks(m, pn, n, target)
          }

          startRRs.foreach {
            srcSink =>
              {
                val pins = srcSink match {
                  case src: RRSource => {
                    m.pat.succs(src)
                  }

                  case sink: RRSink => {
                    m.pat.preds(sink)
                  }

                  case other => scala.sys.error("Unexpected rr.")
                }

                val terms = pins
                  .map(
                    pin => RREdgeAssignment(srcSink, pin, value, target)
                  )
                  .filter(model.varsRefCount.contains(_))
                  .map(
                    t => (1.0, t)
                  )

                val ct = if (up) STARTU else STARTD
                val constr = ILPConstraint(ct, terms.toSet, GRB.EQUAL, sa)

                if (add) {
                  model.addConstraint(constr)
                } else {
                  model.removeConstraint(constr)
                }
              }
          }
        }
    }
  }

  def addConstrStarts(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrStarts(g, m, e, true, getDirection(m, e))
  }

  def removeConstrStarts(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrStarts(g, m, e, false, getDirection(m, e))
  }

  def genConstrEnds(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean, up: Boolean): Unit = {
    val endConstr = (rrea: RREdgeAssignment, endRR: RRSourceSink) => {
      val prim = m.pat.srcSinkToPrim(endRR)
      val n = if (up) g.nodes(rrea.value.nodeName) else g.nodes(rrea.sink.nodeName)

      val sa = Assignment(m, prim, n, e)

      val terms = (1.0, sa) :: Nil
      val ct = if (up) ENDU else ENDD
      val constr = ILPConstraint(ct, terms.toSet, GRB.GREATER_EQUAL, rrea)

      if (add) {
        model.addConstraint(constr)
      } else {
        model.removeConstraint(constr)
      }
    }

    val callback = (rrea: RREdgeAssignment) => {
      rrea.dst match {
        case src @ RRSource(name, capacity, rrType) if (up) => endConstr(rrea, src)
        case sink @ RRSink(name, capacity, rrType) if (!up) => endConstr(rrea, sink)
        case other                                          =>
      }
    }

    onAllRRSAssignments(g, m, e, up, callback)
  }

  def addConstrEnds(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrEnds(g, m, e, true, getDirection(m, e))
  }

  def removeConstrEnds(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrEnds(g, m, e, false, getDirection(m, e))
  }

  // TODO finalize this constraint
  // TODO could also make the input ports use only one sink -> but maybe more complicated....
  def genConstrPreventHsPinOveruse(
      g: ElasticGraph,
      m: AbsMolecule,
      edges: Set[ElasticEdge],
      add: Boolean
  ): Unit = {
    val rreas = edges.map {
      e =>
        {
          val up = getDirection(m, e)
          val callback = (rrea: RREdgeAssignment) => rrea

          onAllRRSAssignments(g, m, e, up, callback)
        }
    }.flatten

    rreas
      .filter {
        rrea =>
          {
            val up = m.classifyEdge(rrea.value, rrea.sink) match {
              case EIExt        => true
              case EOExt | ELoc => false
            }

            ((up && rrea.src.isInstanceOf[RRSink]) || (!up && rrea.dst.isInstanceOf[RRSink]))
          }
      }
      .groupBy(
        rrea => (rrea.src, rrea.dst, rrea.value, rrea.sink.nodeName)
      )
      .filter(_._2.size > 1)
      .map(_._2)
      .map {
        competing =>
          {
            val terms = competing.map(
              rrea => (1.0, rrea)
            )
            val constr = ILPConstraint(HSPin, terms.toSet, GRB.LESS_EQUAL, 1.0)

            if (add) {
              model.addConstraint(constr)
            } else {
              model.removeConstraint(constr)
            }
          }
      }
  }

  def addConstrPreventHsPinOveruse(g: ElasticGraph, m: AbsMolecule, edges: Set[ElasticEdge]): Unit = {
    genConstrPreventHsPinOveruse(g, m, edges, true)
  }

  def removeConstrPreventHsPinOveruse(g: ElasticGraph, m: AbsMolecule, edges: Set[ElasticEdge]): Unit = {
    genConstrPreventHsPinOveruse(g, m, edges, false)
  }

  def canBeDuplicated(value: PortNodeID): Boolean = {
    (value.pId.pt == PTOutput)
    && (value.pId.pmw.pb == D)
  }

  // TODO make sure this can be called anywhere in the remove loop....
  def genConstrPreventLoop(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean, up: Boolean): Unit = {
    val callback = (rrea: RREdgeAssignment) => rrea
    val rreas = onAllRRSAssignments(g, m, e, up, callback)

    rreas.groupBy(_.dst).filter(_._2.size > 1).map {
      (rr, candidates) =>
        {
          val terms = candidates.map(
            rrea => (1.0, rrea)
          )
          val constr = ILPConstraint(LOOP, terms.toSet, GRB.LESS_EQUAL, 1.0)

          if (add) {
            model.addConstraint(constr)
          } else {
            model.removeConstraint(constr)
          }
        }
    }
  }

  def addConstrPreventLoop(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrPreventLoop(g, m, e, true, getDirection(m, e))
  }

  def removeConstrPreventLoop(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrPreventLoop(g, m, e, false, getDirection(m, e))
  }

  def genConstrSourceSinkType(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean, up: Boolean): Unit = {
    val callback = (rrea: RREdgeAssignment) => rrea
    val rreas = onAllRRSAssignments(g, m, e, up, callback)

    rreas.filter(_.dst.isInstanceOf[RRSourceSink]).foreach {
      rrea =>
        {
          val disableEdge = rrea.dst match {
            case source: RRSource => !canMap(m.pat, source, rrea.value)
            case sink: RRSink     => !canMap(m.pat, sink, rrea.sink)
            case other            => scala.sys.error("Unexpected routing resource type.")
          }

          if (disableEdge) {
            val terms = (1.0, rrea) :: Nil
            val constr = ILPConstraint(STType, terms.toSet, GRB.EQUAL, 0.0)

            if (add) {
              model.addConstraint(constr)
            } else {
              model.removeConstraint(constr)
            }
          }
        }
    }
  }

  def addConstrSourceSinkType(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrSourceSinkType(g, m, e, true, getDirection(m, e))
  }

  def removeConstrSourceSinkType(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genConstrSourceSinkType(g, m, e, false, getDirection(m, e))
  }

  def crossCompatible(v0: PortNodeID, v1: PortNodeID): Boolean = {
    ((v0.nodeName == v1.nodeName)
    && (v0.loc == v1.loc)
    && (v0.pId.pmw.pm == v1.pId.pmw.pm)
    && (v0.pId.pt == v1.pId.pt)
    && (v0.pId.width == v1.pId.width)
    && (v0.pId.dummy == v1.pId.dummy))
  }

  def genConstrIDUsageRec(edges: List[RREdgeAssignment], add: Boolean, log: Boolean): Unit = {
    edges.foreach {
      edge0 =>
        {
          // At least one node in common in both D and Hs edges, with the same meaning and same loc

          val compatibleCandidates = edges.filter {
            edge1 =>
              {
                crossCompatible(edge0.value, edge1.value) || crossCompatible(edge0.sink, edge1.sink)
              }
          }

          val (dCandidates, hsCandidates) = compatibleCandidates.partition(_.value.pId.pmw.pb == D)

          val constr = if ((dCandidates.size == 0) || (hsCandidates.size == 0)) {
            // Cannot use the data and hs crossings, cannot use the identity

            if (log) {
              println("" + edge0 + " = 0")
            }

            val terms = (1.0, edge0) :: Nil
            ILPConstraint(DID, terms.toSet, GRB.EQUAL, 0.0)
          } else {
            // Since we are in a crossing, pin capacities ensure that one of D and one of Hs are used at a time
            // So, we just set the sum to 0, positive for d and negative for hs

            val dTerms = dCandidates.map(
              drrea => (1.0, drrea)
            )
            val hsTerms = hsCandidates.map(
              hsrrea => (-1.0, hsrrea)
            )

            if (log) {
              println((dTerms ++ hsTerms).mkString("\n = 0"))
            }

            ILPConstraint(DID, (dTerms ++ hsTerms).toSet, GRB.EQUAL, 0)
          }

          if (add) {
            model.addConstraint(constr)
          } else {
            model.removeConstraint(constr)
          }
        }
    }
  }

  def genConstrIDUsage(g: ElasticGraph, m: AbsMolecule, add: Boolean): Unit = {
    val primToCrossing = crossingEdges
      .groupBy(
        rrea => m.pat.getPrim(rrea.src.asInstanceOf[RRPin])
      )
      .filter(!_._1.isIo)

    // println(primToCrossing.map((k, v) => (k.name + " ->\n" + v.mkString("\n"))).mkString("\n\n"))

    primToCrossing.foreach {
      (prim, edges) =>
        {
          val requiredBundles = prim.block.prim match {
            case idPrim: IdentityPrimitive => {
              idPrim.requiredBundles(idPrim.p)
            }

            case Entry(_) => Set()
            case Exit(_)  => Set()

            case other => scala.sys.error("Expected identity primitiive: " + prim.name)
          }

          if (requiredBundles.size > 1) {
            assert(requiredBundles.size == 2)
            assert(requiredBundles.filter(_._2 == D).size == 1)
            assert(requiredBundles.filter(_._2 == Hs).size == 1)

            // println
            // println
            // println(edges.mkString("\n"))

            genConstrIDUsageRec(edges.toList, add, false)
          }
        }
    }
  }

  def addConstrIDUsage(g: ElasticGraph, m: AbsMolecule): Unit = {
    genConstrIDUsage(g, m, true)
  }

  def removeConstrIDUsage(g: ElasticGraph, m: AbsMolecule): Unit = {
    genConstrIDUsage(g, m, false)
  }

  def initObjective() = model.setInitObjective
  def setObjective(log: Boolean) = model.setObjective(log)

  def freeILP(): Unit = {
    model.model.dispose()
  }

  def setAssignment(a: Assignment, add: Boolean, value: Double): Unit = {
    val terms = (1.0, a) :: Nil
    val constr = ILPConstraint(ASGN, terms.toSet, GRB.EQUAL, value)

    if (add) {
      model.addConstraint(constr)
    } else {
      model.removeConstraint(constr)
    }
  }

  def genSetLocMapping(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge, add: Boolean): Unit = {
    val callback = (locA: Assignment) => {
      locA match {
        case ExtAssignment(pn, value) =>
        case LocAssignment(pn, n) => {
          if (m.mapping.contains(pn) && (m.mapping.locAssignments(pn) == n)) {
            setAssignment(locA, add, 1.0)
          } else {
            setAssignment(locA, add, 0.0)
          }
        }
      }
    }

    onAllAssignments(g, m, e, callback)
  }

  def addConstrSetLocMapping(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genSetLocMapping(g, m, e, true)
  }

  def removeConstrSetLocMapping(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    genSetLocMapping(g, m, e, false)
  }

  def genSetLastMapping(g: ElasticGraph, m: EnumMolecule, add: Boolean): Unit = {
    setAssignment(LocAssignment(m.last.pn, m.last.n), add, 1.0)
  }

  def addSetLastMapping(g: ElasticGraph, m: EnumMolecule): Unit = {
    genSetLastMapping(g, m, true)
  }

  def removeSetLastMapping(g: ElasticGraph, m: EnumMolecule): Unit = {
    genSetLastMapping(g, m, false)
  }

  // Set to 0 to all except proposed mapping
  // Not called with free nodes
  // def genSetExtMapping(g: ElasticGraph, m: AbsMolecule, edges: Set[ElasticEdge], add: Boolean): Unit = {
  //   edges.groupBy(e => m.getExternalValue(e)).map(_._2).foreach {
  //     commonValuedEdges => {
  //       val pns = commonValuedEdges.map {
  //         e => {
  //           val extN = m.getExternalNode(g, e).get
  //           getPotentialPNs(g, m, extN, Some(e))
  //         }
  //       }.flatten

  //       commonValuedEdges.map {
  //         e => {
  //           val callback = (locA: Assignment) => {
  //             if(!pns.contains(locA.pn)) {
  //               setAssignment(locA, add, 0.0)
  //             }
  //           }

  //           onAllAssignments(g, m, None, Some(e), callback)
  //         }
  //       }
  //     }
  //   }
  // }

  // def addConstrSetExtMapping(g: ElasticGraph, m: AbsMolecule, edges: Set[ElasticEdge]): Unit = {
  //   genSetExtMapping(g, m, edges, true)
  // }

  // def removeConstrSetExtMapping(g: ElasticGraph, m: AbsMolecule, edges: Set[ElasticEdge]): Unit = {
  //   genSetExtMapping(g, m, edges, false)
  // }

  def addLocalILPState(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    addVarValues(g, m, e)
    addVarValuesSinks(g, m, e)

    model.model.update()

    addConstrRouteOveruse(g, m, e)
    addConstrRRUsage(g, m, e)
    addConstrRoutingContinuity(g, m, e)
    addConstrStarts(g, m, e)
    addConstrEnds(g, m, e)
    addConstrPreventLoop(g, m, e)
    addConstrSourceSinkType(g, m, e)

    model.model.update()
  }

  def removeLocalILPState(g: ElasticGraph, m: AbsMolecule, e: ElasticEdge): Unit = {
    // Need to do it before removing the values...
    removeConstrPreventLoop(g, m, e)
    removeConstrRoutingContinuity(g, m, e)
    removeConstrStarts(g, m, e)

    removeVarValues(g, m, e)
    removeVarValuesSinks(g, m, e)

    removeConstrRouteOveruse(g, m, e)
    removeConstrRRUsage(g, m, e)
    removeConstrSourceSinkType(g, m, e)
    removeConstrEnds(g, m, e)

    model.model.update()
  }

  def addExternalLinks(g: ElasticGraph, m: AbsMolecule, extEdges: Set[ElasticEdge]): RStatus = {
    extEdges
      .map {
        e =>
          {
            val reachableRRs = m.pat.getReachableRRToIOs(getStarts(g, m, e, getDirection(m, e)))

            if (reachableRRs.nonEmpty) {
              val extN = m.getExternalNode(g, e).get

              addVarAssignment(g, m, e)
              addLocalILPState(g, m, e)

              addConstrNodeMapping(g, m, e)
              addConstrPrimExcl(g, m, e)

              model.model.update()

              RSuccess
            } else {
              RAborted
            }
          }
      }
      .reduceOption(_ & _)
      .getOrElse(RSuccess)
  }

  def removeExternalLinks(g: ElasticGraph, m: AbsMolecule, extEdges: Set[ElasticEdge]): RStatus = {
    extEdges
      .map {
        e =>
          {
            val reachableRRs = m.pat.getReachableRRToIOs(getStarts(g, m, e, getDirection(m, e)))

            if (reachableRRs.nonEmpty) {
              val extN = m.getExternalNode(g, e).get

              removeVarAssignment(g, m, e)
              removeLocalILPState(g, m, e)

              removeConstrNodeMapping(g, m, e)
              removeConstrPrimExcl(g, m, e)

              model.model.update()

              RSuccess
            } else {
              RAborted
            }
          }
      }
      .reduceOption(_ & _)
      .getOrElse(RSuccess)
  }

  def addAssignment(g: ElasticGraph, m: EnumMolecule, log: Boolean): RStatus = {
    val f = () => {
      val edgeInfo = EdgeInfo(g, m)

      val extRemoveStatus = if (edgeInfo.extRemoved.nonEmpty) {
        removeConstrPreventHsPinOveruse(g, (m - m.last).get, edgeInfo.extRemoved)
        removeExternalLinks(g, (m - m.last).get, edgeInfo.extRemoved)
      } else {
        RSuccess
      }

      assert(extRemoveStatus == RSuccess, "\n" + edgeInfo)

      addLastAssignment(g, m)
      addSetLastMapping(g, m)

      edgeInfo.localAdded.foreach {
        e =>
          {
            addVarAssignment(g, m, e)
            addLocalILPState(g, m, e)
            addConstrSetLocMapping(g, m, e)
          }
      }

      addConstrPreventHsPinOveruse(g, m, edgeInfo.localAdded)

      val extAddStatus = addExternalLinks(g, m, edgeInfo.extAdded)

      addConstrIDUsage(g, m)
      addConstrPreventHsPinOveruse(g, m, edgeInfo.extAdded)

      model.model.update()

      extRemoveStatus & extAddStatus
    }

    Util.timeOf("addAssignment", true)(f)
  }

  def removeAssignment(g: ElasticGraph, m: EnumMolecule): RStatus = {
    val f = () => {
      val edgeInfo = EdgeInfo(g, m)

      removeConstrIDUsage(g, m)

      val extRemoveStatus = removeExternalLinks(g, m, edgeInfo.extAdded)
      removeConstrPreventHsPinOveruse(g, m, edgeInfo.extAdded)

      removeLastAssignment(g, m)
      removeSetLastMapping(g, m)

      edgeInfo.localAdded.map {
        e =>
          {
            removeVarAssignment(g, m, e)
            removeLocalILPState(g, m, e)
            removeConstrSetLocMapping(g, m, e)
          }
      }

      removeConstrPreventHsPinOveruse(g, m, edgeInfo.localAdded)

      val extAddStatus = if (edgeInfo.extRemoved.nonEmpty) {
        val t = addExternalLinks(g, (m - m.last).get, edgeInfo.extRemoved)
        addConstrPreventHsPinOveruse(g, (m - m.last).get, edgeInfo.extRemoved)
        t
      } else {
        RSuccess
      }

      assert(extAddStatus == RSuccess)

      model.model.update()

      extRemoveStatus & extAddStatus
    }

    Util.timeOf("removeAssignment", true)(f)
  }

  // TODO refactor using this function
  def withAssignment[T](g: ElasticGraph, m: EnumMolecule, f: () => T): Option[T] = {
    val status = m.extendILP(g, this, false)

    val res = status match {
      case RSuccess => Some(f())
      case RAborted => None
    }

    m.shortenILP(g, this)

    res
  }

  def genAllAssignments(m: AbsMolecule, add: Boolean): Unit = {
    m.mapping.assignments.foreach {
      a =>
        {
          if (add) {
            model.addVariable(a)
          } else {
            model.removeVariable(a)
          }

          setAssignment(a, add, 1.0)
        }
    }
  }

  def addAllAssignments(m: AbsMolecule): Unit = {
    genAllAssignments(m, true)
  }

  def removeAllAssignments(m: AbsMolecule): Unit = {
    genAllAssignments(m, false)
  }

  def addAll(g: ElasticGraph, m: AbsMolecule, forkBehavior: ForkHandling): Unit = {
    addAllAssignments(m)

    m.edges(g).foreach {
      e =>
        {
          addVarAssignment(g, m, e)
          addLocalILPState(g, m, e)
          addConstrSetLocMapping(g, m, e)
        }
    }

    addConstrPreventHsPinOveruse(g, m, m.edges(g).toSet)

    val externalEdges = EdgeInfo.concreteExtEdges(g, m, forkBehavior)
    addExternalLinks(g, m, externalEdges.toSet)

    addConstrIDUsage(g, m)
    addConstrPreventHsPinOveruse(g, m, externalEdges.toSet)
  }

  def removeAll(g: ElasticGraph, m: AbsMolecule): Unit = {
    removeConstrIDUsage(g, m)
    removeAllAssignments(m)

    m.edges(g).foreach {
      e =>
        {
          removeVarAssignment(g, m, e)
          removeLocalILPState(g, m, e)
          removeConstrSetLocMapping(g, m, e)
        }
    }

    removeConstrPreventHsPinOveruse(g, m, m.edges(g).toSet)

    val externalEdges = EdgeInfo.externalEdges(g, m)
    removeExternalLinks(g, m, externalEdges.toSet)
    removeConstrPreventHsPinOveruse(g, m, externalEdges.toSet)
  }

  def printModel(model: GRBModel): Unit = {
    model.getVars().filter(_.get(GRB.DoubleAttr.X) > 0.0).map {
      v =>
        {
          val str = v.get(GRB.StringAttr.VarName)
          val value = v.get(GRB.DoubleAttr.X)

          println(str + ": " + value)
        }
    }
  }

  def getSolution(m: AbsMolecule): Set[PhysicalAssignment] = {
    val assignedRRs = model.model
      .getVars()
      .toSeq
      .filter(_.get(GRB.DoubleAttr.X) > 0.0)
    // .filter(!_.get(GRB.StringAttr.VarName).contains("RS__"))

    val sol: Set[PhysicalAssignment] = assignedRRs
      .map(model.vars(_))
      .collect {
        case a: Assignment          => a
        case rra: RRAssignment      => rra
        case rrea: RREdgeAssignment => rrea
      }
      .toSet

    sol
      .collect {
        case rrea: RREdgeAssignment => rrea
      }
      .foreach {
        rrea =>
          {
            assert(sol.contains(RRAssignment(rrea.src, rrea.value)))
            assert(sol.contains(RRAssignment(rrea.dst, rrea.value)))
          }
      }

    sol
  }

  def route(params: GlobalParamsInst, m: AbsMolecule, log: Boolean): Option[Set[PhysicalAssignment]] = {
    val f = () => {
      model.model.update()

      setObjective(log)

      model.model.reset()
      model.model.optimize()

      model.model.get(GRB.IntAttr.Status) match {
        case GRB.OPTIMAL => Some(getSolution(m))
        case other => {
          if (log) {
            model.writeModel(m, params)
            model.writeIIS(m, params)

            ???
          }

          None
        }
      }
    }

    Util.timeOf("route", true)(f)
  }
}
