package packerv2

import crkt._
import arch._
import core.ACanIdentity
import core.AIdentityCrkt
import archs.IdentityPrimitive
import archs.Source
import archs.Constant
import archs.Exit
import archs.Entry
import archs.IdentityInfo
import frontend.GlobalParamsInst
import printers.DotPrinter
import printers.MoleculePrinter
import core.ATileIo

import collection.mutable.{Set => MSet}
import collection.mutable.{Map => MMap}

case class IdentityID(width: Int, pt: PortType, pb: PortBundle, dummy: DummyType)

// TODO Place this at proper location
object Legalizer {
  val identity = "identity"

  def extractCrkt(mols: Seq[Molecule]): ElasticGraph = {
    val nodes = mols.map(_.mapping.nodes).flatten
    ElasticGraph(
      nodes
        .map(
          n => (n.name, n)
        )
        .toMap
    )
  }
}

case class Legalizer(var idCount: Int = 0) {
  import Legalizer._

  def getArchSrcCst(rrg: RRG, pn: RRGPrim): (RRGPrim, RRGPrim) = {
    def rec(exploring: Set[RR], seen: Set[RR], acc: Set[RRGPrim]): Set[RRGPrim] = {
      if (exploring.isEmpty) {
        acc
      } else {
        val next = exploring.head

        next match {
          case src: RRSource => {
            val prim = rrg.srcSinkToPrim(src)

            if (prim.annos.contains(AIdentityCrkt)) {
              val exploreNext = prim.pinToSrcSink.map(_._2).filter {
                rr => rr.isInstanceOf[RRSink] && (rr.rrType.pt == PTInput) && (rr.rrType.pmw.pb == Hs)
              }

              rec(exploring.tail ++ exploreNext, seen ++ exploreNext, acc + prim)
            } else {
              rec(exploring.tail, seen, acc)
            }
          }

          case rrgi: RRGI => rec(exploring.tail, seen, acc)

          case pin: RRPin if (pin.rrType.pt == PTOutput) => {
            val exploreNext = rrg.preds(pin).collect {
              case src: RRSource => src
            }
            rec(exploring.tail ++ exploreNext, seen ++ exploreNext, acc)
          }

          case other => {
            val exploreNext = rrg.preds(other)
            rec(exploring.tail ++ exploreNext, seen ++ exploreNext, acc)
          }
        }
      }
    }

    val startRR = pn.pinToSrcSink.map(_._2).filter {
      srcSink =>
        {
          srcSink.isInstanceOf[RRSink]
          && (srcSink.rrType.pt == PTInput)
          && (srcSink.rrType.pmw.pb == Hs)
          && (srcSink.rrType.pmw.pm.isInstanceOf[PMCond])
        }
    }

    assert(startRR.size == 1)
    val identityCrkt = rec(Set(startRR.head), Set(startRR.head), Set())

    assert(
      identityCrkt.size == 2,
      "Only support src + cst for now, had: " + identityCrkt.map(_.name) + " starting at: " + startRR
    )

    val src = identityCrkt.filter(_.block.prim.isInstanceOf[Source]).head
    val cst = identityCrkt.filter(_.block.prim.isInstanceOf[Constant]).head

    (src, cst)
  }

  def insertPrimitive(
      m: Molecule,
      idSubCrkt: List[TNode],
      pn: RRGPrim
  ): List[LocAssignment] = {
    assert(idSubCrkt.filter(pn.block.prim.canMap(_, pn.block.prim.p)).size == 1)

    val id = idSubCrkt.filter(pn.block.prim.canMap(_, pn.block.prim.p)).head
    val la = LocAssignment(pn, id)

    // println("-----------------------------------------")
    // println
    // println(pn)
    // println(pn.block.p)
    // println(id)
    // println(id.nType.p)
    // println(m.mapping.locAssignments.map((k,v) => k.name + " -> " + v.name).mkString("\n"))
    // println(id.ports.map(_._2).flatten.map(_.distPorts))
    // println("-----------------------------------------")

    val srcCst = if (idSubCrkt.size == 1) {
      Nil
    } else {
      val srcCrkt = idSubCrkt.filter(_.nType.isInstanceOf[Source]).head
      val cstCrkt = idSubCrkt.filter(_.nType.isInstanceOf[Constant]).head

      val (srcArch, cstArch) = getArchSrcCst(m.pat, pn)

      LocAssignment(srcArch, srcCrkt) :: LocAssignment(cstArch, cstCrkt) :: Nil
    }

    la :: srcCst
  }

  def instantiatePrim(
      m: Molecule,
      pn: RRGPrim,
      idInfos: List[IdentityInfo]
  ): List[LocAssignment] = {
    val idName = Legalizer.identity + idCount
    idCount = idCount + 1

    // println(pn.name + " -> " + idName)

    val idBlock = pn.block.prim.asInstanceOf[IdentityPrimitive]
    val idSubCrkt = idBlock.getIdentitySubCrkt(idBlock.p, idInfos, idName)

    insertPrimitive(m, idSubCrkt, pn)
  }

  def getIdentityInfo(
      g: ElasticGraph,
      mm: MappedMolecule[Molecule],
      crossingEdges: List[RREdgeAssignment]
  ): List[IdentityInfo] = {
    val pn = mm.pat.getPrim(crossingEdges.head.src.asInstanceOf[RRPin])
    assert(pn.annos.contains(ACanIdentity))

    crossingEdges
      .groupBy(_.value.pId.pmw.pb)
      .map {
        (pb, pbEdges) =>
          {
            assert(pbEdges.size == 1, pbEdges)

            val inValue = pbEdges.head.value
            val outSink = pbEdges.head.sink

            val inValueIn = mm.m.mapping.nodeNames.contains(inValue.nodeName)
            val outValueIn = mm.m.mapping.nodeNames.contains(outSink.nodeName)

            val (inPort, outPorts) = if (inValueIn) {
              val inP = mm.m.mapping.nodeNames(inValue.nodeName).getPort(inValue)
              val outPs = inP.distPorts.map(_._2)

              (inP, outPs)
            } else {
              val sinkP = mm.m.mapping.nodeNames(outSink.nodeName).getPort(outSink)
              assert(sinkP.distPorts.size == 1)

              val inP = sinkP.distPorts.head._2

              (inP, sinkP :: Nil)
            }

            IdentityInfo(pn, inPort, outPorts.toList)
          }
      }
      .toList
  }

  def getMMIdInfos(params: GlobalParamsInst, g: ElasticGraph, mm: MappedMolecule[Molecule]): List[IdentityInfo] = {
    mm.assignments
      .collect {
        case rrea: RREdgeAssignment => rrea
      }
      .filter {
        case RREdgeAssignment(src, dst, value, sink) => {
          val srcIsPin = src.isInstanceOf[RRPin]
          val dstIsPin = dst.isInstanceOf[RRPin]
          lazy val samePrim = mm.m.pat.getPrim(src.asInstanceOf[RRPin]) == mm.m.pat.getPrim(dst.asInstanceOf[RRPin])
          lazy val isIo = mm.m.pat.getPrim(src.asInstanceOf[RRPin]).isIo

          srcIsPin && dstIsPin && samePrim && !isIo
        }
      }
      .groupBy(
        rrea => mm.pat.getPrim(rrea.src.asInstanceOf[RRPin])
      )
      .map {
        (prim, crossingEdges) =>
          {
            getIdentityInfo(g, mm, crossingEdges.toList)
          }
      }
      .flatten
      .toList
  }

  def updateMolecule(idInfos: List[IdentityInfo], m: Molecule): Molecule = {
    val (pn, links) = idInfos.groupBy(_.pn).head

    val nLocs = instantiatePrim(m, pn, links)

    val nAssignments = m.mapping.assignments ++ nLocs
    Molecule(m.name, m.pat, Mapping(nAssignments), m.dagIds)
  }

  def filterMapping(mm: MappedMolecule[Molecule]): Set[PhysicalAssignment] = {
    val extIoRRs = mm.pat.prims
      .filter(_.annos.contains(ATileIo))
      .map {
        pn =>
          {
            pn.block.prim match {
              case Entry(_) => {
                val sink = pn.pinToSrcSink
                  .map(_._2)
                  .collect {
                    case sink: RRSink => sink
                  }
                  .head
                val sinkPin = mm.pat.preds(sink)

                sinkPin + sink
              }
              case Exit(_) => {
                val source = pn.pinToSrcSink
                  .map(_._2)
                  .collect {
                    case src: RRSource => src
                  }
                  .head
                val sourcePin = mm.pat.succs(source)

                sourcePin + source
              }

              case other => scala.sys.error("Unexpected match: " + pn.name)
            }
          }
      }
      .flatten

    mm.assignments.filter {
      case RRAssignment(rr, value) => !rr.isInstanceOf[RRGI] && !extIoRRs.contains(rr)
      case RREdgeAssignment(src, dst, value, sink) => {
        !src.isInstanceOf[RRGI] && !dst.isInstanceOf[RRGI] && !extIoRRs.contains(dst) && !extIoRRs.contains(src)
      }
      case other => true
    }
  }

  def insertIo(
      mm: MappedMolecule[Molecule],
      rra: RRAssignment,
      pin: RRPin,
      srcSink: RRSourceSink,
      assignments: Set[PhysicalAssignment]
  ): Set[PhysicalAssignment] = {
    val rras = mm.rrsas(rra.rr).filter(_.value == rra.value)

    val nRRA = RRAssignment(srcSink, rra.value)
    val nRRAs = srcSink match {
      case src: RRSource => {
        rras.map(
          rrsa => RREdgeAssignment(srcSink, pin, rrsa.value, rrsa.sink)
        )
      }

      case sink: RRSink => {
        rras.map(
          rrsa => RREdgeAssignment(pin, srcSink, rrsa.value, rrsa.sink)
        )
      }
    }

    val pn = mm.m.pat.srcSinkToPrim(srcSink)
    val nA = ExtAssignment(pn, rra.value)

    nRRAs ++ Set(nA, nRRA, rra)
  }

  def insertIos(mm: MappedMolecule[Molecule], assignments: Set[PhysicalAssignment]): Set[PhysicalAssignment] = {
    assignments.map {
      case rra @ RRAssignment(pin @ RRPin(_, _, _, rrT, _), value) => {
        rrT.pt match {
          case PTInput => {
            val sink = mm.pat
              .succs(pin)
              .collect {
                case sink: RRSink => sink
              }
              .head

            if (!assignments.contains(RRAssignment(sink, value))) {
              assert(mm.pat.srcSinkToPrim(sink).annos.contains(ATileIo))

              insertIo(mm, rra, pin, sink, assignments)
            } else {
              rra :: Nil
            }
          }

          case PTOutput => {
            val src = mm.pat
              .preds(pin)
              .collect {
                case src: RRSource => src
              }
              .head

            if (!assignments.contains(RRAssignment(src, value))) {
              assert(mm.pat.srcSinkToPrim(src).annos.contains(ATileIo))

              insertIo(mm, rra, pin, src, assignments)
            } else {
              rra :: Nil
            }
          }

          case other => scala.sys.error("Expected defined direction.")
        }
      }

      case other => other :: Nil
    }.flatten
  }

  def legalizeIos(nMapped: List[MappedMolecule[Molecule]]): List[MappedMolecule[Molecule]] = {
    nMapped.map {
      mm =>
        {
          val internalMapping = filterMapping(mm)
          val internalWithIos = insertIos(mm, internalMapping)

          val nAssignments = internalWithIos.collect {
            case a: Assignment => a
          }

          val nM = Molecule(mm.m.name, mm.m.pat, Mapping(nAssignments), mm.m.dagIds)
          MappedMolecule(nM, internalWithIos)
        }
    }
  }

  def apply(params: GlobalParamsInst, matches: Seq[MappedMolecule[Molecule]]): Seq[MappedMolecule[Molecule]] = {
    def rec(
        unhandledMols: List[MappedMolecule[Molecule]],
        handledMols: List[Molecule],
        g: ElasticGraph
    ): List[Molecule] = {
      if (unhandledMols.isEmpty) {
        handledMols
      } else {
        val allMolecules = unhandledMols.map(_.m) ++ handledMols

        val mm = unhandledMols.head
        val idInfos = getMMIdInfos(params, g, mm)

        if (idInfos.isEmpty) {
          rec(unhandledMols.tail, mm.m :: handledMols, g)
        } else {
          val nM = updateMolecule(idInfos, mm.m)
          val nAllMols = nM :: (unhandledMols.tail.map(_.m) ++ handledMols)
          val nG = extractCrkt(nAllMols)

          val callback = (r: Router) => {
            Some(nM.route(r, params, false))
          }

          val nAssignments = Router.withFullILP(nG, nM, callback, KeepAll).get
          val nMM = MappedMolecule(nM, nAssignments)

          rec(nMM :: unhandledMols.tail, handledMols, nG)
        }
      }
    }

    val initG = extractCrkt(matches.map(_.m))
    val molsWithIdentities = rec(matches.toList, Nil, initG)

    val nG = extractCrkt(molsWithIdentities)
    val gAnnotated = AnnotateMolecules(nG, molsWithIdentities)

    DotPrinter(params.buildDir + "/lowering/packed_legal.dot")(gAnnotated, Set())

    val nMapped = molsWithIdentities.zipWithIndex.map {
      (m, i) =>
        {
          // println(m.name)

          val callback = (r: Router) => {
            Some(m.route(r, params, true))
          }

          val mapping = Router.withFullILP(nG, m, callback, KeepAll).get
          assert(mapping.nonEmpty)

          MappedMolecule(m, mapping)
        }
    }

    val legal = legalizeIos(nMapped)

    legal.zipWithIndex.foreach {
      (mm, i) =>
        {
          MoleculePrinter(params, mm.withName(mm.name + "_" + i), true)
          // println(m.mapping.assignments.map((k,v) => (k.name, v.name)).mkString(", "))
          // println(mapping.mkString("\n"))
        }
    }

    legal
  }
}
