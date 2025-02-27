package packerv2

import crkt.PortNodeID
import crkt.Node
import arch._
import readers.PlaceLocInfo
import core.ATileIo

sealed trait Producer {
  def loc: PbLoc
}
case class LinkProducer(loc: PbLoc, link: Link) extends Producer
case class SourceProducer(loc: PbLoc, value: PortNodeID) extends Producer

case class PinMapping(loc: PbLoc, prod: Option[Producer])

case class ImplementedMolecule(
    tile: RootPb,
    locMap: Map[PbLoc, Option[Producer]],
    mm: Option[MappedMolecule[Molecule]],
    blockMap: Set[(TBlock, String)], // block to node name, should be the other way around....
    primMap: Map[String, String], // prim name to node name
    locToValue: Map[PbLoc, PortNodeID]
) {
  def getLoc(locs: List[PlaceLocInfo]): (Int, Int) = {
    val nodeNames = primMap.map(_._2).toSet

    locs
      .filter {
        case PlaceLocInfo(blockName, x, y, xOffset, yOffset) => {
          nodeNames.contains(blockName)
        }
      }
      .headOption
      .fold(
        scala.sys
          .error("Expected tile to have a location for\n" + primMap.mkString("\n") + "\n" + locs.map(_.blockName))
      ) {
        pl => (pl.x, pl.y)
      }
  }

  def pbAssigned(pb: PbType, arch: Arch): Boolean = {
    locMap
      .filter(
        (loc, prod) => arch.contains(loc.pin.id)
      )
      .exists {
        (loc, prod) =>
          {
            (loc.pbName == pb.name) && prod.nonEmpty
          }
      }
  }

  def usesPrimitive(pb: PbType, arch: Arch): Boolean = {
    val recUse = pb.subBlocks.exists(usesPrimitive(_, arch))

    pb match {
      case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
        locMap
          .filter(
            (loc, prod) => arch.contains(loc.pin.id)
          )
          .exists {
            (loc, prod) =>
              {
                (loc.pbName == pb.name) && prod.nonEmpty
              }
          }
      }

      case other => recUse
    }
  }

  def usesClockedPrimitive(pb: PbType, arch: Arch): Boolean = {
    val recUse = pb.subBlocks.exists(usesClockedPrimitive(_, arch))

    pb match {
      case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
        locMap
          .filter(
            (loc, prod) => arch.contains(loc.pin.id)
          )
          .exists {
            (loc, prod) =>
              {
                (loc.pbName == pb.name) && prod.nonEmpty && prim.physicalInfo.clocked
              }
          }
      }

      case other => recUse
    }
  }

  def getPrim(n: Node): TBlock = {
    blockMap.filter(_._2 == n.name).head._1
  }
}

// Works only at the Hs+D level

object ImplementMolecule {
  def collectPbs(tile: RootPb): Map[String, PbType] = {
    def rec(pb: PbType): List[(String, PbType)] = {
      pb match {
        case prim: PrimPb   => (prim.name -> prim) :: Nil
        case inter: InterPb => (inter.name -> inter) :: inter.subBlocks.toList.map(rec(_)).flatten
        case root: RootPb   => (root.name -> root) :: root.subBlocks.toList.map(rec(_)).flatten
      }
    }

    rec(tile).toMap
  }

  def rrToLoc(rrg: RRG, pbMap: Map[String, PbType], rr: RR): ArchLoc = {
    rr match {
      case cmux: RRCMux => RouteLoc(pbMap(cmux.pbName), cmux.muxPointer)
      case pin: RRPin => {
        val prim = rrg.getPrim(pin)

        if (prim.isIo) {
          PrimLoc(rrg.pb, rrg.pinMap(pin))
        } else {
          PrimLoc(pbMap(prim.name), pin.toPin())
        }
      }

      case other => scala.sys.error("Unexpected routing ressource: " + other)
    }
  }

  def getPinMap(aLoc: ArchLoc, producer: Option[Producer], value: PortNodeID): PinMapping = {
    aLoc match {
      case PrimLoc(pb, pin) => {
        val loc = PbLoc(pb.name, pin)
        PinMapping(loc, Some(SourceProducer(loc, value)))
      }

      case RouteLoc(pb, pbLink) => {
        pbLink match {
          case d @ Direct(srcLoc, dstLoc, delay) => {
            PinMapping(dstLoc, Some(LinkProducer(srcLoc, d)))
          }

          case m @ Mux(sources, dstLoc, delay) => {
            val srcLoc = producer.get match {
              case SourceProducer(loc, value) => loc

              case LinkProducer(loc, link) => {
                link match {
                  case Direct(srcLoc, dstLoc, delay) => dstLoc
                  case Mux(sources, dstLoc, delay)   => dstLoc
                  case other                         => scala.sys.error("Unexpected link.")
                }
              }
            }

            PinMapping(dstLoc, Some(LinkProducer(srcLoc, m)))
          }

          case other => scala.sys.error("Unexpected link.")
        }
      }
    }
  }

  def startPinMap(startLoc: ArchLoc, rrea: RREdgeAssignment, prevRREAMap: List[PinMapping]): List[PinMapping] = {
    startLoc match {
      case PrimLoc(pb, pin) => {
        val loc = PbLoc(pb.name, pin)
        PinMapping(loc, Some(SourceProducer(loc, rrea.value))) :: prevRREAMap
      }

      case RouteLoc(pb, pbLink) => {
        pbLink match {
          case Mux(sources, dstLoc, delay) => prevRREAMap
          case other                       => scala.sys.error("Unexpected link.")
        }
      }
    }
  }

  def findPath(
      rrg: RRG,
      pbMap: Map[String, PbType],
      pbParents: Map[String, PbType],
      mm: MappedMolecule[Molecule],
      rrea: RREdgeAssignment,
      prevRREAMap: List[PinMapping]
  ): List[PinMapping] = {
    def rec(aLoc: ArchLoc, targetLoc: ArchLoc, acc: List[PinMapping]): List[PinMapping] = {
      val reachedTarget = targetLoc match {
        case pl @ PrimLoc(pb, pin) => {
          aLoc match {
            case PrimLoc(rpb, pin) => false
            case RouteLoc(rpb, pbLink) => {
              pbLink match {
                case Direct(srcLoc, dstLoc, delay) => dstLoc == PbLoc(pb.name, pin)
                case Mux(sources, dstLoc, delay)   => dstLoc == PbLoc(pb.name, pin)
                case CLK(srcPb, dstPb)             => scala.sys.error("Unexpected link.")
              }
            }
          }
        }

        case rl: RouteLoc => {
          aLoc match {
            case pl: PrimLoc   => false
            case arl: RouteLoc => rl == arl
          }
        }
      }

      val producer = acc.headOption.map(_._2).getOrElse(None)
      val pinMapping = getPinMap(aLoc, producer, rrea.value)

      if (reachedTarget) {
        // if((rrea.value.nodeName == "mux2") && (rrea.value.loc == 0)) {
        //   println(rrea)
        //   println((pinMapping :: acc).mkString("\n"))
        // }

        pinMapping :: acc
      } else {
        val recLocs = RRG.getNextRLocs(aLoc, pbParents, false)

        recLocs.map(rec(_, targetLoc, pinMapping :: acc)).filter(_.nonEmpty).headOption.getOrElse(Nil)
      }
    }

    mm.m.classifyEdge(rrea.value, rrea.sink) match {
      case EOExt | ELoc => {
        val srcLoc = rrToLoc(rrg, pbMap, rrea.src)
        val dstLoc = rrToLoc(rrg, pbMap, rrea.dst)

        val initPinMap = startPinMap(srcLoc, rrea, prevRREAMap)

        val pinMap = rec(srcLoc, dstLoc, initPinMap)
        assert(pinMap.nonEmpty)

        pinMap
      }

      case EIExt => {
        val srcLoc = rrToLoc(rrg, pbMap, rrea.src)
        val dstLoc = rrToLoc(rrg, pbMap, rrea.dst)

        val initPinMap = startPinMap(dstLoc, rrea, prevRREAMap)

        val pinMap = rec(dstLoc, srcLoc, initPinMap)
        assert(pinMap.nonEmpty)

        pinMap
      }
    }
  }

  def removeInternalLinks(rreas: List[RREdgeAssignment]): List[RREdgeAssignment] = {
    rreas.filter {
      rrea =>
        {
          !rrea.src.isInstanceOf[RRSource]
          && !rrea.src.isInstanceOf[RRSink]
          && !rrea.dst.isInstanceOf[RRSource]
          && !rrea.dst.isInstanceOf[RRSink]
        }
    }
  }

  def sortRREAs(rreas: Set[RREdgeAssignment]): List[RREdgeAssignment] = {
    def rec(loc: RREdgeAssignment, acc: List[RREdgeAssignment]): List[RREdgeAssignment] = {
      val next = rreas.filter(_.src == loc.dst)
      if (next.isEmpty) {
        acc
      } else {
        assert(next.size == 1)

        rec(next.head, next.head :: acc)
      }
    }

    val starts = rreas.filter(
      rrea => !rreas.exists(_.dst == rrea.src)
    )

    if (starts.size > 2) {
      println("start:")
      println(starts.mkString("\n"))
      println("rreas:")
      println(rreas.mkString("\n"))
    }

    assert(starts.size <= 2)

    val sortedEdges = starts.toList
      .map(
        start => rec(start, start :: Nil)
      )
      .flatten

    if (sortedEdges.head.dst.isInstanceOf[RRSource]) {
      removeInternalLinks(sortedEdges)
    } else {
      removeInternalLinks(sortedEdges.reverse)
    }
  }

  def getLocToValueMap(mm: MappedMolecule[Molecule], pbMap: Map[String, PbType]): Map[PbLoc, PortNodeID] = {
    def getPinMap(rrea: RREdgeAssignment, src: Boolean): Option[(PbLoc, PortNodeID)] = {
      val rr = if (src) rrea.src else rrea.dst

      rr match {
        case pin: RRPin => {
          val primLoc = rrToLoc(mm.m.pat, pbMap, pin)
          assert(primLoc.isInstanceOf[PrimLoc])

          val pbLoc = PbLoc(primLoc.pb.name, primLoc.asInstanceOf[PrimLoc].pin)

          if (primLoc.pb.isInstanceOf[PrimPb]) {
            if (pin.rrType.pt == PTInput) {
              Some((pbLoc -> rrea.sink))
            } else {
              Some((pbLoc -> rrea.value))
            }
          } else {
            None
          }
        }

        case other => None
      }
    }

    mm.assignments
      .collect {
        case rrea: RREdgeAssignment => rrea
      }
      .filter {
        rrea =>
          {
            rrea.src.isInstanceOf[RRPin] || rrea.dst.isInstanceOf[RRPin]
          }
      }
      .map {
        rrea =>
          {
            val srcPinMap = getPinMap(rrea, true)
            val dstPinMap = getPinMap(rrea, false)

            (srcPinMap :: dstPinMap :: Nil).flatten
          }
      }
      .flatten
      .toMap
  }

  def fillOpenLocs(mm: MappedMolecule[Molecule], locMap: Map[PbLoc, Option[Producer]]): Map[PbLoc, Option[Producer]] = {
    def rec(pb: PbType): Seq[(PbLoc, Option[Producer])] = {
      val subMap = pb.subBlocks.map(rec(_)).flatten

      val pbLocs = pb.bi.ports.map(_._2).map(_.toPins()).flatten.map {
        pin =>
          {
            val loc = PbLoc(pb.name, pin)

            if (locMap.contains(loc)) {
              (loc, locMap(loc))
            } else {
              (loc, None)
            }
          }
      }

      (pbLocs ++ subMap).toSeq
    }

    rec(mm.m.pat.pb).toMap
  }

  def allValuesExits(mm: MappedMolecule[Molecule], mol: ImplementedMolecule): Boolean = {
    val assignments = mm.assignments.collect {
      case a: Assignment => a
    }

    val values = assignments.map {
      case LocAssignment(pn, n) => {
        n.values
      }

      case ExtAssignment(pn, value) => {
        if (value.pId.pt == PTInput) { // Is an output port of the tile, not SourceProducer expected here
          Nil
        } else {
          value :: Nil
        }
      }
    }.flatten

    val sourceProds = mol.locMap.map(_._2).flatten.collect {
      case sp: SourceProducer => sp
    }

    // all values exist
    values.forall {
      value =>
        {
          val vExists = sourceProds.exists(_.value == value)

          assert(vExists, "value " + value + " exists in the mapped molecule but not in the implemented molecule")

          vExists
        }
    }
  }

  def allNodePortsExists(mm: MappedMolecule[Molecule], mol: ImplementedMolecule): Boolean = {
    val assignments = mm.assignments.collect {
      case a: Assignment => a
    }

    assignments.forall {
      case LocAssignment(pn, n) => {
        val prodLocs = mol.locMap.map {
          (loc, prod) =>
            {
              prod match {
                case None => None
                case Some(SourceProducer(srcLoc, value)) => {
                  if (srcLoc.pbName == pn.name) {
                    Some(srcLoc)
                  } else {
                    None
                  }
                }

                case Some(LinkProducer(_, _)) => {
                  if (loc.pbName == pn.name) {
                    Some(loc)
                  } else {
                    None
                  }
                }
              }
            }
        }.flatten

        val expectedAllLocs = n.ports.map(
          (id, ps) => (id, ps.size)
        )

        expectedAllLocs.forall {
          (id, num) =>
            {
              val prodsWithSameId = prodLocs.filter(_.pin.id == id).size
              val cond = prodsWithSameId == num

              if (!cond) {
                println(mm.name)
                println(mm.m.mapping)
                println("--")
                println(prodLocs.mkString("\n"))
                println("--")
                println(mol.locMap.filter(_._1.pbName == "CMerge__").mkString("\n"))
                println("--")
                println(
                  mm.assignments
                    .collect {
                      case rrea: RREdgeAssignment => rrea
                    }
                    .filter(_.value.nodeName == "control_merge2")
                    .mkString("\n")
                )
              }

              assert(
                cond,
                "for " + n.name + " and id " + id + " expected " + num
                  + " prod but got " + prodLocs.filter(_.pin.id == id)
              )

              cond
            }
        }
      }

      case ExtAssignment(pn, value) => {
        // TODO hard to recover original PbType name
        true
      }
    }
  }

  def pathsAreContinuous(mm: MappedMolecule[Molecule], mol: ImplementedMolecule): Boolean = {
    mol.locMap.forall {
      (loc, prod) =>
        {
          prod match {
            case None => true

            case Some(SourceProducer(srcLoc, value)) => {
              val connectedLink = mol.locMap.exists {
                (rLoc, rProd) =>
                  {
                    rProd match {
                      case None                       => false
                      case Some(SourceProducer(_, _)) => false
                      case Some(LinkProducer(oSrcLoc, link)) => {
                        oSrcLoc == srcLoc
                      }
                    }
                  }
              }

              assert(connectedLink, "" + SourceProducer(srcLoc, value) + " is not driving any link.")

              connectedLink
            }

            case Some(LinkProducer(srcLoc, link)) => {
              val connectedLink = mol.locMap.exists {
                (_, rProd) =>
                  {
                    rProd match {
                      case None                          => false
                      case Some(SourceProducer(rLoc, _)) => rLoc == loc
                      case Some(LinkProducer(rLoc, _))   => rLoc == loc
                    }
                  }
              }

              val isTargetLocIo = mol.tile.name == loc.pbName
              val isTargetLocPrim = (
                mm.m.mapping.locAssignments.keySet.map(_.name).contains(loc.pbName)
                  && (loc.pin.id.pt == PTInput)
              )

              assert(
                connectedLink || isTargetLocIo || isTargetLocPrim,
                "" + LinkProducer(srcLoc, link) + " is not driving any link."
              )

              connectedLink || isTargetLocIo || isTargetLocPrim
            }
          }
        }
    }
  }

  def allLocsExist(mol: ImplementedMolecule): Boolean = {
    def rec(pb: PbType): Boolean = {
      pb.bi.ports.map(_._2).map(_.toPins()).flatten.forall {
        pin =>
          {
            val loc = PbLoc(pb.name, pin)
            val locIn = mol.locMap.contains(loc)

            assert(locIn, "" + loc + " is not in the implemented molecule.")

            locIn
          }
      } && pb.subBlocks.forall(rec(_))
    }

    rec(mol.tile)
  }

  def verify(mm: MappedMolecule[Molecule], mol: ImplementedMolecule): Boolean = {
    (allValuesExits(mm, mol)
    && allNodePortsExists(mm, mol)
    && pathsAreContinuous(mm, mol)
    && allLocsExist(mol))
  }

  def apply(mm: MappedMolecule[Molecule]): ImplementedMolecule = {
    val rootPb = mm.m.pat.pb

    val pbMap = collectPbs(rootPb)
    val pbParents = RRG.pbToParent(rootPb)

    val locMap = mm.assignments
      .collect {
        case rrea: RREdgeAssignment => rrea
      }
      .groupBy(
        rrea => (rrea.value, rrea.sink)
      )
      .map(_._2)
      .map {
        rreas =>
          {
            val path = sortRREAs(rreas).foldLeft(Nil) {
              (pinMap: List[PinMapping], rrea) =>
                {
                  findPath(mm.m.pat, pbMap, pbParents, mm, rrea, pinMap)
                }
            }

            // if(mm.name.contains("Ancillary_cmergeMuxMerges___control_merge5___0")
            //   && (rreas.head.value.nodeName == "mux2")
            //   && (rreas.head.value.loc == 0)) {
            //   println(sortRREAs(rreas).mkString("\n"))
            //   println(path.mkString("\n"))
            // }

            path
          }
      }
      .flatten
      .map(
        pinMapping => (pinMapping.loc, pinMapping.prod)
      )
      .toMap

    val allLocs = fillOpenLocs(mm, locMap)
    val blockMap = mm.assignments
      .collect {
        case la: LocAssignment => la
      }
      .map(
        la => (la.pn.block, la.n.name)
      )
      .toSet
    val primMap = mm.assignments
      .collect {
        case la: LocAssignment => la
      }
      .map(
        la => (la.pn.name, la.n.name)
      )
      .toMap
    val locToValue = getLocToValueMap(mm, pbMap)

    // if(mm.name.contains("Ancillary_cmergeMuxMerges___control_merge5___0")) {
    //   println(locToValue.filter(_._2.nodeName == "mux2").mkString("\n"))

    //   ???
    // }

    val mol = ImplementedMolecule(
      rootPb,
      allLocs,
      Some(mm),
      blockMap,
      primMap,
      locToValue
    )

    assert(verify(mm, mol))

    mol
  }
}
