package packerv2

import arch._
import core.Annotation
import core.Namer
import core.AMoleculeRoot
import core.AEquivalent
import crkt.Node
import crkt.TNode
import crkt.Port
import core.AIo
import archs.Entry
import archs.Exit
import archs.Source
import core.ACanIdentity
import util.Util
import util.Cache
import archs.EntryParams
import archs.ExitParams
import archs.SrcConstant
import frontend.GlobalParamsInst
import core.ATileIo

import collection.mutable.{Set => MSet}
import collection.mutable.{Map => MMap}
import io.AnsiColor._
import archs.TEHB
import archs.OEHB
import archs.EB
import readers.MLIRParser.block

object RR {
  val infinity = Integer.MAX_VALUE
}

sealed trait RR {
  def name: String
  def capacity: Int
  def rrType: BlockPortID
}

case class RRCMux(
    pbName: String,
    cmuxName: String,
    capacity: Int,
    rrType: BlockPortID,
    muxPointer: Mux // Used for easy implementation of the packing solution, in ImplementMolecule
) extends RR {
  override def toString(): String = {
    YELLOW + "RRCMUX" + RESET + rrType + "[" + GREEN + capacity + RESET + "]"
  }

  val name = pbName + "_" + cmuxName
}

object RRPin {
  def name(pb: PbType, id: BlockPortID, loc: Int): String = {
    "pin_" + Util.validVprName(pb.name) + "_" + Namer(Pin(id, loc))
  }
}

case class RRPin(primName: String, name: String, capacity: Int, rrType: BlockPortID, loc: Int) extends RR {
  override def toString(): String = {
    YELLOW + "RRPIN" + RESET + rrType + "[" + GREEN + capacity + RESET + "]"
  }

  def toPin(): Pin = Pin(rrType, loc)
}

case class RRGI(pref: String, width: Int) extends RR {
  val name = pref + "GI" + width
  val capacity = RR.infinity
  val rrType = BlockPortID(width, PTUndef, PortMeaningWrapper(PMData(None), D), Regular)

  override def toString(): String = {
    YELLOW + "RRGI" + RESET + width
  }
}

sealed trait RRSourceSink extends RR

object RRSource {
  def name(pb: PbType, id: BlockPortID, pins: Set[Pin]): String = {
    "source_" + Util.validVprName(pb.name) + "_" + Namer(id)
  }
}

case class RRSource(name: String, capacity: Int, rrType: BlockPortID) extends RRSourceSink {
  override def toString(): String = {
    YELLOW + "RRSrc" + RESET + rrType + "[" + GREEN + capacity + RESET + "]"
  }
}

object RRSink {
  def name(pb: PbType, id: BlockPortID, pins: Set[Pin]): String = {
    "sink_" + Util.validVprName(pb.name) + "_" + Namer(id)
  }
}

case class RRSink(name: String, capacity: Int, rrType: BlockPortID) extends RRSourceSink {
  override def toString(): String = {
    YELLOW + "RRSink" + RESET + rrType + "[" + GREEN + capacity + RESET + "]"
  }
}

case class RREdge(src: RR, dst: RR) {
  override def toString(): String = "" + src + " -> " + dst
}

case class RRGPrim(
  name: String,
  block: TBlock,
  pinToSrcSink: Map[Pin, RRSourceSink],
  annos: Set[Annotation],
  dagIds: Option[DagId]
) {
  override def toString(): String = {
    name + ": " + block.name + "\n"
      + "  " + block.blockInterface.ports.mkString("\n  ")
  }

  def withAnnotation(annot: Annotation): RRGPrim = {
    RRGPrim(name, block, pinToSrcSink, annos + annot, dagIds)
  }

  def withDagId(dId: Option[DagId]): RRGPrim = {
    RRGPrim(name, block, pinToSrcSink, annos, dId)
  }

  def canMap(n: TNode): Boolean = {
    block.prim.canMap(n, block.p)
  }

  def isIo: Boolean = annos.contains(ATileIo)
  def isBuf: Boolean = block.prim match {
    case TEHB(_) | OEHB(_) | EB(_) => true
    case other                     => false
  }

  // Hashcode and equals to speed up Sets of RRGPrim
  override def hashCode(): Int = name.hashCode()

  override def equals(x: Any): Boolean = {
    if (x.isInstanceOf[RRGPrim]) {
      val oPN = x.asInstanceOf[RRGPrim]
      this.name == oPN.name
    } else {
      false
    }
  }
}

case class RRG(
    molName: String,
    prims: Set[RRGPrim],
    rrs: Set[RR],
    preds: Map[RR, Set[RR]],
    succs: Map[RR, Set[RR]],
    srcSinkToPrim: Map[RRSourceSink, RRGPrim],
    t: Tile,
    pb: RootPb,
    pinMap: Map[RRPin, Pin]
) {
  val rrCache = Cache[RR, Set[RR]]()
  val rrToIoCache = Cache[RR, Set[RR]]()
  val primCache = Cache[RR, Set[RRGPrim]]()
  val ioCache = Cache[RR, Set[RRGPrim]]()

  lazy val roots = prims.filter(_.annos.contains(AMoleculeRoot)).toSet
  lazy val ios: Map[(PortType, Int), Int] = prims
    .filter(_.isIo)
    .map(_.block)
    .map {
      block =>
        {
          block.blockInterface.ports.map(_._2).map {
            bp =>
              {
                (0 until bp.words).map {
                  w =>
                    {
                      val pts = block.prim match {
                        case Entry(p) => {
                          bp.id.pmw.pb match {
                            case D     => PTInput :: Nil
                            case Hs    => PTInput :: PTOutput :: Nil
                            case other => ???
                          }
                        }

                        case Exit(p) => {
                          bp.id.pmw.pb match {
                            case D     => PTOutput :: Nil
                            case Hs    => PTInput :: PTOutput :: Nil
                            case other => ???
                          }
                        }
                      }

                      val width = bp.id.concreteWidth

                      pts.map(
                        pt => (pt, width)
                      )
                    }
                }
              }
          }
        }
    }
    .flatten
    .flatten
    .flatten
    .groupBy(
      t => t
    )
    .map(
      (k, v) => (k, v.size)
    )
    .toMap

  def neighbors(prim: RRGPrim, dir: PortType): Set[RRGPrim] = {
    def rec(rr: RR, links: Map[RR, Set[RR]]): Set[RRGPrim] = {
      rr match {
        case sink: RRSink => {
          Set(srcSinkToPrim(sink))
        }

        case src: RRSource => {
          Set(srcSinkToPrim(src))
        }

        case other => {
          links(other)
            .filter(!_.isInstanceOf[RRGI])
            .map {
              recRR =>
                {
                  rec(recRR, links)
                }
            }
            .flatten
        }
      }
    }

    prim.pinToSrcSink
      .map(_._2)
      .map {
        case src: RRSource => {
          if (dir == PTInput) {
            Set()
          } else {
            succs(src)
              .map(
                rr => rec(rr, succs)
              )
              .flatten
          }
        }

        case sink: RRSink => {
          if (dir == PTInput) {
            preds(sink)
              .map(
                rr => rec(rr, preds)
              )
              .flatten
          } else {
            Set()
          }
        }
      }
      .flatten
      .toSet
      .filter(!_.annos.contains(ATileIo))
  }

  def getPrim(pin: RRPin): RRGPrim = {
    srcSinkToPrim(getSrcSink(pin))
  }

  def getPrim(srcSink: RRSourceSink): RRGPrim = {
    srcSinkToPrim(srcSink)
  }

  // For now we assume that cmux can bypass primitives only one at a time
  def getBypassing(cmux: RRCMux): Option[RRGPrim] = {
    if (preds(cmux).size != 2) {
      None
    } else {
      val bpOutCandidate = preds(cmux).filter(succs(_).size == 1)
      if ((bpOutCandidate.size != 1) || !bpOutCandidate.head.isInstanceOf[RRPin]) {
        None
      } else {
        val primOut = bpOutCandidate.head.asInstanceOf[RRPin]
        assert(primOut.rrType.pt == PTOutput)

        val bpPrim = getPrim(primOut)

        val beforePrimCandidate = preds(cmux).filter(succs(_).size > 1)
        if (beforePrimCandidate.size != 1) {
          None
        } else {
          val prevRR = beforePrimCandidate.head
          if (
            succs(prevRR).exists(
              nRR => nRR.isInstanceOf[RRPin] && getPrim(nRR.asInstanceOf[RRPin]) == bpPrim
            )
          ) {
            Some(bpPrim)
          } else {
            None
          }
        }
      }
    }
  }

  def getLocalPrimSuccessors(pn: RRGPrim): Set[RRGPrim] = {
    neighbors(pn, PTOutput)
  }

  def getSrcSink(pin: RRPin): RRSourceSink = {
    pin.rrType.pt match {
      case PTInput =>
        succs(pin).collect {
          case sink: RRSink => sink
        }.head
      case PTOutput =>
        preds(pin).collect {
          case source: RRSource => source
        }.head
      case other => ???
    }
  }

  def bypassingIo(rr: RR, recRR: RR): Boolean = {
    val rrIsPin = rr.isInstanceOf[RRPin]
    val recRRIsPin = recRR.isInstanceOf[RRPin]

    lazy val samePrim = getPrim(rr.asInstanceOf[RRPin]) == getPrim(recRR.asInstanceOf[RRPin])
    lazy val primIo = getPrim(rr.asInstanceOf[RRPin]).isIo

    rrIsPin && recRRIsPin && samePrim && primIo
  }

  def genReachableRRs(startRR: RR, toIos: Boolean): Set[RR] = {
    var seen = MSet[RR]()
    var accepted = MSet[RR]()

    val links = startRR match {
      case src: RRSource => this.succs
      case sink: RRSink  => this.preds
      case other         => ???
    }

    def rec(rr: RR, path: Set[RR]): Unit = {
      if (accepted.contains(rr)) {
        accepted ++= path
      } else if (!seen.contains(rr)) {
        seen += rr

        // println(rr.name)

        val reachableRRs = links
          .getOrElse(rr, Set())
          .filter {
            recRR =>
              {
                if (!toIos) {
                  true
                } else {
                  !bypassingIo(rr, recRR)
                }
              }
          }

        if (reachableRRs.isEmpty && toIos) {
          rr match {
            case rrSrcSink: RRSourceSink => {
              if (srcSinkToPrim(rr.asInstanceOf[RRSourceSink]).isIo) {
                accepted ++= (path + rr)
              } else {
                Set()
              }
            }

            case other => {
              val oneSeen = links.getOrElse(rr, Set()).exists(accepted.contains(_))
              if (oneSeen) {
                accepted ++= (path + rr)
              }
            }
          }

        } else if (reachableRRs.isEmpty) {
          accepted ++= (path + rr)
        } else {
          reachableRRs.map(rec(_, path + rr))
        }
      }
    }

    rec(startRR, Set(startRR))

    accepted.toSet
  }

  def getReachableRRs(rr: RR): Set[RR] = {
    rrCache.getOrPut(rr, (aRR: RR) => genReachableRRs(aRR, false))
  }

  def getReachableRRs(rrs: Set[RR]): Set[RR] = {
    rrs.map(getReachableRRs(_)).flatten
  }

  def getReachablePrims(start: RR): Set[RRGPrim] = {
    val f = (rr: RR) => {
      getReachableRRs(start)
        .collect {
          case src: RRSource if (start.isInstanceOf[RRSink])  => src
          case sink: RRSink if (start.isInstanceOf[RRSource]) => sink
        }
        .map(srcSinkToPrim(_))
    }

    primCache.getOrPut(start, f)
  }

  def getReachablePrims(starts: Set[RR]): Set[RRGPrim] = {
    starts
      .map(
        start => getReachablePrims(start)
      )
      .flatten
  }

  def getReachableRRToIOs(start: RR): Set[RR] = {
    rrToIoCache.getOrPut(start, (rr: RR) => genReachableRRs(rr, true))
  }

  def getReachableRRToIOs(starts: Set[RR]): Set[RR] = {
    starts
      .map(
        start => getReachableRRToIOs(start)
      )
      .flatten
  }

  def getReachableIOs(start: RR): Set[RRGPrim] = {
    val f = (rr: RR) => {
      getReachableRRToIOs(start)
        .collect {
          case src: RRSource if (start.isInstanceOf[RRSink])  => src
          case sink: RRSink if (start.isInstanceOf[RRSource]) => sink
        }
        .map(srcSinkToPrim(_))
        .filter(_.annos.contains(ATileIo))
    }

    ioCache.getOrPut(start, f)
  }

  def getReachableIOs(starts: Set[RR]): Set[RRGPrim] = {
    starts
      .map(
        start => getReachableIOs(start)
      )
      .flatten
  }

  def getReachableEdges(start: RR, toIo: Boolean): Set[RREdge] = {
    val reachable = if (toIo) {
      getReachableRRToIOs(start)
    } else {
      getReachableRRs(start)
    }

    val links = start match {
      case src: RRSource => this.succs
      case sink: RRSink  => this.preds
      case other         => ???
    }

    reachable.map {
      rr =>
        {
          links.getOrElse(rr, Set()).filter(reachable.contains(_)).map {
            nRR =>
              {
                RREdge(rr, nRR)
              }
          }
        }
    }.flatten
  }

  def getReachableEdges(starts: Set[RR]): Set[RREdge] = {
    starts.map(getReachableEdges(_, false)).flatten
  }

  def getReachableEdgesToIos(starts: Set[RR]): Set[RREdge] = {
    starts.map(getReachableEdges(_, true)).flatten
  }
}

case class LocalRRG(prim: RRGPrim, rrs: Set[RR], pinMap: Map[Pin, RRPin], srcSinkMap: Map[Pin, RRSourceSink]) {
  def getLocalLinks(): (Map[RR, Set[RR]], Map[RR, Set[RR]]) = {
    val preds = MMap[RR, MSet[RR]]()
    val succs = MMap[RR, MSet[RR]]()

    pinMap.foreach {
      (pin, pinRR) =>
        {
          pinRR.rrType.pt match {
            case PTInput => {
              val nSuccs = succs
                .get(pinRR)
                .fold(MSet[RR](srcSinkMap(pin)))(
                  set => set += srcSinkMap(pin)
                )
              succs(pinRR) = nSuccs

              val nPreds = preds
                .get(srcSinkMap(pin))
                .fold(MSet[RR](pinRR))(
                  set => set += pinRR
                )
              preds(srcSinkMap(pin)) = nPreds
            }

            case PTOutput => {
              val nPreds = preds
                .get(pinRR)
                .fold(MSet[RR](srcSinkMap(pin)))(
                  set => set += srcSinkMap(pin)
                )
              preds(pinRR) = nPreds

              val nSuccs = succs
                .get(srcSinkMap(pin))
                .fold(MSet[RR](pinRR))(
                  set => set += pinRR
                )
              succs(srcSinkMap(pin)) = nSuccs
            }

            case other => scala.sys.error("Expected defined direction")
          }
        }
    }

    val finalPreds = preds
      .map(
        (rr, drrs) => (rr, drrs.toSet)
      )
      .toMap
    val finalSuccs = succs
      .map(
        (rr, drrs) => (rr, drrs.toSet)
      )
      .toMap

    (finalPreds, finalSuccs)
  }
}

sealed trait Direction
case object Down extends Direction
case object Up extends Direction

sealed trait ArchLoc {
  def pb: PbType
}

case class RouteLoc(pb: PbType, pbLink: Link) extends ArchLoc {
  override def toString(): String = {
    pb.name + "." + pbLink
  }

  override def hashCode(): Int = (pb.name, pbLink).hashCode()
  override def equals(x: Any): Boolean = {
    if (x.isInstanceOf[RouteLoc]) {
      val oRLoc = x.asInstanceOf[RouteLoc]
      (oRLoc.pb.name == pb.name) && (oRLoc.pbLink == pbLink)
    } else {
      false
    }
  }
}

case class PrimLoc(pb: PbType, pin: Pin) extends ArchLoc {
  override def toString(): String = {
    pb.name + "." + pin
  }

  override def hashCode(): Int = (pb.name, pin).hashCode()
  override def equals(x: Any): Boolean = {
    if (x.isInstanceOf[PrimLoc]) {
      val oRLoc = x.asInstanceOf[PrimLoc]
      (oRLoc.pb.name == pb.name) && (oRLoc.pin == pin)
    } else {
      false
    }
  }
}

object RRG {
  val ioBaseName = "IO"

  def pbToParent(tile: PbType): Map[String, PbType] = {
    def rec(pb: PbType, parent: Option[PbType]): Map[String, PbType] = {
      pb match {
        case prim: PrimPb => {
          if (parent.nonEmpty) {
            Map(prim.name -> parent.get)
          } else {
            Map()
          }
        }

        case nonLeaf: NonLeafPb => {
          if (parent.nonEmpty) {
            nonLeaf.subBlocks.map(rec(_, Some(pb))).reduce(_ ++ _) + (pb.name -> parent.get)
          } else {
            nonLeaf.subBlocks.map(rec(_, Some(pb))).reduce(_ ++ _)
          }
        }
      }
    }

    rec(tile, None)
  }

  def getSourceSink(pb: PbType, id: BlockPortID, pins: Set[Pin]): RRSourceSink = {
    id.pt match {
      case PTInput => {
        val name = RRSink.name(pb, id, pins)
        RRSink(name, pins.size, id)
      }

      case PTOutput => {
        val name = RRSource.name(pb, id, pins)
        RRSource(name, pins.size, id)
      }

      case _ => scala.sys.error("Expected defined direction in RRG generation.")
    }
  }

  def getPinsIo(pb: PbType, ioLoc: PrimLoc): (Set[RR], Map[Pin, RRPin]) = {
    val srcId = ioLoc.pin.id.flipped()
    val id = BlockPortID(srcId.width, srcId.pt, PortMeaningWrapper(PMData(None), srcId.pmw.pb), srcId.dummy)
    val name = RRPin.name(pb, id, ioLoc.pin.loc)

    val pin = RRPin(pb.name, name, 1, id, 0)
    val pinMap = Map[Pin, RRPin]((ioLoc.pin -> pin))

    (Set[RR](pin), pinMap)
  }

  def getPinsPrim(pb: PbType, bi: BlockInterface): (Set[RR], Map[Pin, RRPin]) = {
    val (pins, pinsMap) = bi.ports
      .map {
        (id, bp) =>
          {
            (0 until bp.words).map {
              w =>
                {
                  val name = RRPin.name(pb, id, w)
                  val pin = RRPin(pb.name, name, 1, id, w)

                  val pinMap = (Pin(bp.id, w) -> pin)

                  (pin, pinMap)
                }
            }
          }
      }
      .flatten
      .unzip

    // if(pb.name == "brcidcstforkFork__55") {
    //   println(pb)
    //   println(pb.bi)
    //   println(pb.links)
    //   println(pinsMap.mkString("\n"))

    //   ???
    // }

    (pins.toSet, pinsMap.toMap)
  }

  def getSrcSinkIo(pb: PbType, ioLoc: PrimLoc): (Set[RR], Map[Pin, RRSourceSink]) = {
    val srcId = ioLoc.pin.id.flipped()
    val id = BlockPortID(srcId.width, srcId.pt, PortMeaningWrapper(PMData(None), srcId.pmw.pb), srcId.dummy)
    val srcSink = getSourceSink(pb, id, Set(ioLoc.pin))
    val pinToSrcSinks = Map[Pin, RRSourceSink]((ioLoc.pin -> srcSink))

    (Set[RR](srcSink), pinToSrcSinks)
  }

  def getSrcSinkPrim(pb: PbType, b: TBlock): (Set[RR], Map[Pin, RRSourceSink]) = {
    val (srcSinks, pinToSrcSinks) = b.blockInterface.ports
      .map {
        (id, bp) =>
          {
            if (bp.annotations.contains(AEquivalent)) {
              val pins = (0 until bp.words)
                .map(
                  w => Pin(bp.id, w)
                )
                .toSet
              val srcSink = getSourceSink(pb, id, pins)
              val pinToSrcSink = pins
                .map(
                  pin => (pin -> srcSink)
                )
                .toSeq

              (srcSink, pinToSrcSink) :: Nil
            } else {
              (0 until bp.words).map {
                w =>
                  {
                    val pin = Pin(bp.id, w)
                    val nPb = pb.withName(pb.name + "_" + w)
                    val srcSink = getSourceSink(nPb, id, Set(Pin(bp.id, w)))
                    val pinToSrcSink = Seq(pin -> srcSink)

                    (srcSink, pinToSrcSink)
                  }
              }
            }
          }
      }
      .flatten
      .unzip

    (srcSinks.toSet, pinToSrcSinks.flatten.toMap)
  }

  def getLocalRRG(pb: PbType, b: TBlock, ioPin: Option[PrimLoc]): LocalRRG = {
    val (srcSinks, pinToSrcSinks) = if (ioPin.nonEmpty) {
      getSrcSinkIo(pb, ioPin.get)
    } else {
      getSrcSinkPrim(pb, b)
    }

    val (pins, pinsMap) = if (ioPin.nonEmpty) {
      getPinsIo(pb, ioPin.get)
    } else {
      getPinsPrim(pb, b.blockInterface)
    }

    val pn = pb match {
      case prim: PrimPb => {
        RRGPrim(pb.name, b, pinToSrcSinks, pb.annotations ++ b.annotations, prim.dagIds)
      }

      case other => {
        RRGPrim(pb.name, b, pinToSrcSinks, pb.annotations ++ b.annotations, None)
      }
    }

    val rrs = srcSinks ++ pins

    LocalRRG(pn, rrs.toSet, pinsMap, pinToSrcSinks)
  }

  def instantiateIO(
      pbLoc: PrimLoc,
      ioNames: MMap[PrimLoc, String],
      params: GlobalParamsInst
  ): LocalRRG = {
    val pFlipped = pbLoc.pin.flipped

    val b = if (pbLoc.pin.id.pt == PTInput) {
      val entryP = EntryParams(pbLoc.pin.id.width, Set(pbLoc.pin.id.pmw.pb))
      Entry(entryP).instantiate(params)
    } else {
      val exitP = ExitParams(pbLoc.pin.id.width, Set(pbLoc.pin.id.pmw.pb))
      Exit(exitP).instantiate(params)
    }

    val instName = if (ioNames.contains(pbLoc)) {
      ioNames(pbLoc)
    } else {
      val name = ioBaseName + ioNames.size
      ioNames(pbLoc) = name

      name
    }

    val instPb = pbLoc.pb.withName(instName).withAnnotation(ATileIo)

    getLocalRRG(instPb, b, Some(pbLoc))
  }

  def getCorrespondingRR(
      l: ArchLoc,
      cMuxNames: MMap[Mux, String],
      ioNames: MMap[PrimLoc, String],
      params: GlobalParamsInst
  ): RR = {
    l match {
      case RouteLoc(pb, pbLink) => {
        assert(pbLink.isInstanceOf[Mux])

        val mux = pbLink.asInstanceOf[Mux]

        val cmuxName = if (cMuxNames.contains(mux)) {
          cMuxNames(mux)
        } else {
          val nName = "cmux_" + cMuxNames.size
          cMuxNames(mux) = nName

          nName
        }

        val tId = (pbLink.asInstanceOf[Mux]).dstLoc.pin.id

        RRCMux(pb.name, cmuxName, 1, tId, mux)
      }

      case pbLoc @ PrimLoc(pb, pin) => {
        pb match {
          case prim: PrimPb => {
            val locRRG = getLocalRRG(prim, prim.prim, None)
            val primPin = prim.pinMap(pin)

            if (!locRRG.pinMap.contains(primPin)) {
              println(primPin)
              println(locRRG.pinMap.mkString("\n"))
              println("--")
              println(prim.pinMap.mkString("\n"))
              println(prim.bi)
            }

            locRRG.pinMap(primPin)
          }

          case nonLeaf: NonLeafPb => {
            val localRRG = instantiateIO(pbLoc, ioNames, params)

            // TODO should we check the location?
            if (!localRRG.pinMap.contains(pin)) {
              println(pbLoc)
              println(localRRG.pinMap.mkString("\n"))
            }

            localRRG.pinMap(pin)
          }
        }
      }
    }
  }

  def matchingPins(targetPin: Pin, nextPin: Pin): Boolean = {
    val tId = targetPin.id
    val nId = nextPin.id

    val matchingLocs = targetPin.loc == nextPin.loc

    val matching = (tId.width == nId.width)
      && (tId.pmw == nId.pmw)
      && (tId.dummy == nId.dummy)
      && (tId.pt == nId.pt)
      && matchingLocs

    matching
  }

  def getTargetLoc(link: Link): PbLoc = {
    link match {
      case Direct(srcLoc, dstLoc, delay) => {
        dstLoc
      }

      case Mux(sources, dstLoc, delay) => {
        dstLoc
      }

      case other => ???
    }
  }

  def matchingLinks(rLoc: RouteLoc, candidate: Link): Boolean = {
    candidate match {
      case Direct(srcLoc, sinkLoc, delay) => {
        val targetLoc = getTargetLoc(rLoc.pbLink)

        (targetLoc.pbName == srcLoc.pbName) && matchingPins(targetLoc.pin, srcLoc.pin)
      }

      case Mux(sources, dstLoc, delay) => {
        val targetLoc = getTargetLoc(rLoc.pbLink)

        sources.exists {
          srcLoc =>
            {
              (targetLoc.pbName == srcLoc.pbName) && matchingPins(targetLoc.pin, srcLoc.pin)
            }
        }
      }

      case other => false
    }
  }

  def getWalkingDirection(rLoc: RouteLoc): Direction = {
    rLoc.pbLink match {
      case Direct(srcLoc, dstLoc, delay) => {
        val isDown = (srcLoc.pin.id.pt == PTInput) && (dstLoc.pin.id.pt == PTInput)
          || (srcLoc.pin.id.pt == PTOutput) && (dstLoc.pin.id.pt == PTInput)
        val isUp = (srcLoc.pin.id.pt == PTOutput) && (dstLoc.pin.id.pt == PTOutput)
          || (srcLoc.pin.id.pt == PTInput) && (dstLoc.pin.id.pt == PTOutput)

        if (isUp) {
          Up
        } else if (isDown) {
          Down
        } else {
          ???
        }
      }

      case Mux(sources, dstLoc, delay) => {
        if (dstLoc.pin.id.pt == PTOutput) {
          Up
        } else {
          Down
        }
      }

      case other => ???
    }
  }

  def matchingPb(rLoc: RouteLoc, recPb: PbType): Boolean = {
    rLoc.pbLink match {
      case Direct(srcLoc, dstLoc, delay) => {
        dstLoc.pbName == recPb.name
      }

      case Mux(sources, dstLoc, delay) => {
        dstLoc.pbName == recPb.name
      }

      case other => ???
    }
  }

  def getNextPbs(rLoc: RouteLoc, dir: Direction, pbParents: Map[String, PbType]): Seq[PbType] = {
    dir match {
      case Down => {
        rLoc.pb.subBlocks.filter {
          recPb =>
            {
              matchingPb(rLoc, recPb)
            }
        }
      }

      case Up => {
        pbParents
          .get(rLoc.pb.name)
          .fold(Nil)(
            parent => parent :: Nil
          )
      }
    }
  }

  def getCorrespondingRouteLoc(pbLoc: PrimLoc, pbParents: Map[String, PbType]): Seq[RouteLoc] = {
    val (linkPb, links) = if (pbLoc.pb.links.nonEmpty) {
      (pbLoc.pb, pbLoc.pb.links)
    } else {
      val parent = pbParents(pbLoc.pb.name)
      (parent, parent.links)
    }

    val rs = links
      .filter {
        case Direct(srcLoc, dstLoc, delay) => {
          val srcPin = Pin(srcLoc.pin.id, srcLoc.pin.loc)

          val pinsOk = matchingPins(pbLoc.pin, srcPin)
          val pbOk = pbLoc.pb.name == srcLoc.pbName

          pinsOk && pbOk
        }

        case Mux(sources, dstLoc, delay) => {
          sources.exists {
            srcLoc =>
              {
                val srcPin = Pin(srcLoc.pin.id, srcLoc.pin.loc)

                val pinsOk = matchingPins(pbLoc.pin, srcPin)
                val pbOk = pbLoc.pb.name == srcLoc.pbName

                pinsOk && pbOk
              }
          }
        }

        case other => false
      }
      .map(RouteLoc(linkPb, _))

    if (rs.isEmpty) {
      println(links.mkString("\n"))
      println(linkPb.subBlocks.head.bi)
      println(linkPb.bi)
    }

    assert(rs.size >= 1, pbLoc.toString() + ": " + rs.size)

    rs
  }

  def getNextRLocs(
      aLoc: ArchLoc,
      pbParents: Map[String, PbType],
      log: Boolean
  ): Seq[RouteLoc] = {
    aLoc match {
      case rLoc: RouteLoc => {
        val dir = getWalkingDirection(rLoc)
        val recPbs = getNextPbs(rLoc, dir, pbParents)

        recPbs
          .map(
            pb => (pb, pb.links)
          )
          .map {
            (recPb, links) =>
              {
                if (log) {
                  println(recPb.name)
                  println(links.mkString("\n"))
                }

                (recPb, links.filter(matchingLinks(rLoc, _)))
              }
          }
          .map {
            (recPb, links) =>
              {
                links.map {
                  link =>
                    {
                      RouteLoc(recPb, link)
                    }
                }
              }
          }
          .flatten
      }

      case primLoc: PrimLoc => {
        getCorrespondingRouteLoc(primLoc, pbParents)
      }
    }
  }

  def getTargetLocs(rLoc: RouteLoc, recPbs: Seq[PbType]): Set[PrimLoc] = {
    val validRecs = recPbs.filter {
      case primPb: PrimPb => true
      case r: RootPb      => true
      case other          => false
    }

    validRecs
      .map {
        recPb =>
          {
            rLoc.pbLink match {
              case Direct(srcLoc, dstLoc, delay) => {
                if (dstLoc.pbName == recPb.name) {
                  Some(PrimLoc(recPb, dstLoc.pin))
                } else {
                  None
                }
              }

              case Mux(sources, dstLoc, delay) => {
                if (dstLoc.pbName == recPb.name) {
                  Some(PrimLoc(recPb, dstLoc.pin))
                } else {
                  None
                }
              }

              case other => ???
            }
          }
      }
      .flatten
      .toSet
  }

  // Find all target RR nodes the pin can reach
  // Also returns the set of RRPrims these RR nodes are linked to
  def canReach(aLoc: ArchLoc, pbParents: Map[String, PbType], log: Boolean): Set[ArchLoc] = {
    if (log) {
      println("aLoc: " + aLoc)
      println("--")
    }

    val nextRLocs = getNextRLocs(aLoc, pbParents, log)
      .map {
        nRLoc =>
          {
            nRLoc.pbLink match {
              case m: Mux => {
                Set(nRLoc)
              }

              case d: Direct => {
                canReach(nRLoc, pbParents, log)
              }

              case other => ???
            }
          }
      }
      .flatten
      .toSet

    val targetLocs = aLoc match {
      case rLoc: RouteLoc => {
        val dir = getWalkingDirection(rLoc)
        val recPbs = getNextPbs(rLoc, dir, pbParents)

        if ((dir == Up) && recPbs.isEmpty && (rLoc.pb.isInstanceOf[RootPb])) {
          getTargetLocs(rLoc, Seq(rLoc.pb))
        } else {
          getTargetLocs(rLoc, recPbs)
        }
      }

      case primLoc: PrimLoc => {
        Nil
      }
    }

    nextRLocs ++ targetLocs
  }

  def getPbStartPoints(pb: PbType, dir: PortType): Set[PrimLoc] = {
    pb.bi.ports
      .filter(_._1.pt == dir)
      .map {
        (_, bp) =>
          {
            (0 until bp.words).map {
              w =>
                {
                  PrimLoc(pb, Pin(bp.id, w))
                }
            }
          }
      }
      .flatten
      .toSet
  }

  def getNewStartPoints(aLoc: ArchLoc): Set[ArchLoc] = {
    aLoc match {
      case rLoc @ RouteLoc(pb, pbLink) => {
        assert(pbLink.isInstanceOf[Mux])

        Set(rLoc)
      }

      case PrimLoc(pb, _) => {
        getPbStartPoints(pb, PTOutput).asInstanceOf[Set[ArchLoc]]
      }
    }
  }

  // TODO could memoize this and avoid generation in canReach
  def getLocalRRGs(
      tile: PbType,
      ioNames: MMap[PrimLoc, String],
      params: GlobalParamsInst
  ): Set[LocalRRG] = {
    val ioPrimitives = tile.bi.ports
      .map(_._2)
      .map {
        bp =>
          {
            (0 until bp.words).map {
              w =>
                {
                  val loc = PrimLoc(tile, Pin(bp.id, w))
                  instantiateIO(loc, ioNames, params)
                }
            }
          }
      }
      .flatten

    def rec(pb: PbType): Seq[LocalRRG] = {
      pb match {
        case primPb: PrimPb => {
          Seq(getLocalRRG(primPb, primPb.prim, None))
        }

        case nonLeaf: NonLeafPb => {
          nonLeaf.subBlocks.map(rec(_)).flatten
        }
      }
    }

    (rec(tile) ++ ioPrimitives).toSet
  }

  def findSources(mol: PbType): Set[ArchLoc] = {
    def keepOutputs(pb: PrimPb): Set[ArchLoc] = {
      pb.bi.ports
        .filter(_._1.pt == PTOutput)
        .map {
          (id, bp) =>
            {
              (0 until bp.words).map {
                w =>
                  {
                    PrimLoc(pb, Pin(id, w))
                  }
              }
            }
        }
        .flatten
        .toSet
    }

    def rec(pb: PbType): Set[ArchLoc] = {
      val recLocs = pb.subBlocks.map(rec(_)).flatten.toSet

      pb match {
        case primPb @ PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
          prim.prim match {
            case Source(_)      => keepOutputs(primPb)
            case Entry(_)       => keepOutputs(primPb)
            case SrcConstant(_) => keepOutputs(primPb)
            case others         => recLocs
          }
        }

        case other => recLocs
      }
    }

    rec(mol)
  }

  def findInternalRRG(params: GlobalParamsInst, t: Tile, mol: RootPb): RRG = {
    val prims = MSet[RRGPrim]()
    val rrs = MSet[RR]()
    val preds = MMap[RR, MSet[RR]]()
    val succs = MMap[RR, MSet[RR]]()

    val cMuxNames = MMap[Mux, String]()
    val ioNames = MMap[PrimLoc, String]()

    val pbParents = pbToParent(mol)

    def rec(starts: Set[ArchLoc], seen: Set[ArchLoc]): Unit = {
      val nStarts = starts.toSeq
        .sortBy(_.toString())
        .map {
          sLoc =>
            {
              val targetLocs = canReach(sLoc, pbParents, false)
              assert(targetLocs.nonEmpty, canReach(sLoc, pbParents, true))

              val srcRR = getCorrespondingRR(sLoc, cMuxNames, ioNames, params)
              val targetRRs = targetLocs.toSeq.sortBy(_.pb.name).map {
                tLoc =>
                  {
                    getCorrespondingRR(tLoc, cMuxNames, ioNames, params)
                  }
              }

              rrs ++= targetRRs
              rrs += srcRR

              val nSuccs = succs
                .get(srcRR)
                .fold(MSet[RR]() ++ targetRRs)(
                  set => set ++ targetRRs
                )
              succs(srcRR) = nSuccs

              targetRRs.foreach {
                rr =>
                  {
                    val nPreds = preds
                      .get(rr)
                      .fold(MSet[RR](srcRR))(
                        set => set += srcRR
                      )
                    preds(rr) = nPreds
                  }
              }

              val nStartLocs = targetLocs
                .map(getNewStartPoints(_))
                .flatten
                .filter(
                  sLoc => !seen.contains(sLoc)
                )
                .filter(
                  sLoc => !sLoc.pb.isInstanceOf[RootPb]
                )

              nStartLocs
            }
        }
        .flatten

      val nSeen = seen ++ nStarts

      if (nStarts.nonEmpty) {
        rec(nStarts.toSet, nSeen)
      }
    }

    val ioStarts = getPbStartPoints(mol, PTInput).asInstanceOf[Set[ArchLoc]]
    val sourceStarts = findSources(mol)

    val initStarts = ioStarts ++ sourceStarts
    rec(initStarts, initStarts)

    val localRRGs = getLocalRRGs(mol, ioNames, params)

    localRRGs.foreach {
      case lrrg @ LocalRRG(prim, localRRs, pinMap, srcSinkMap) => {
        prims += prim
        rrs ++= localRRs

        val (lrrgPred, lrrgSuccs) = lrrg.getLocalLinks()

        lrrgPred.foreach {
          (srcRR, predRRs) =>
            {
              val nPreds = preds
                .get(srcRR)
                .fold(MSet[RR]() ++= predRRs)(
                  prevPreds => prevPreds ++= predRRs
                )

              preds(srcRR) = nPreds
            }
        }

        lrrgSuccs.foreach {
          (srcRR, succRRs) =>
            {
              val nSuccs = succs
                .get(srcRR)
                .fold(MSet[RR]() ++= succRRs)(
                  prevSuccs => prevSuccs ++= succRRs
                )

              succs(srcRR) = nSuccs
            }
        }
      }
    }

    val finalPreds = preds
      .map(
        (rr, drrs) => (rr, drrs.toSet)
      )
      .toMap
    val finalSuccs = succs
      .map(
        (rr, drrs) => (rr, drrs.toSet)
      )
      .toMap

    rrs.foreach {
      _ match {
        case src: RRSource => {
          assert(finalPreds.getOrElse(src, Set()).isEmpty)
          assert(finalSuccs(src).nonEmpty)
        }
        case sink: RRSink => {
          assert(finalSuccs.getOrElse(sink, Set()).isEmpty)
          assert(finalPreds(sink).nonEmpty)
        }
        case other => {
          assert(finalSuccs(other).nonEmpty)
          assert(finalPreds(other).nonEmpty)
        }
      }
    }

    val srcSinkToprim = prims.toSet
      .map {
        rrgPrim =>
          {
            rrgPrim.pinToSrcSink
              .map(_._2)
              .map(
                rr => (rr, rrgPrim)
              )
          }
      }
      .flatten
      .toMap

    val pinMap = localRRGs
      .filter(_.prim.isIo)
      .map(
        _.pinMap.map(
          (k, v) => (v, k)
        )
      )
      .flatten
      .toMap

    RRG(mol.name, prims.toSet, rrs.toSet, finalPreds, finalSuccs, srcSinkToprim, t, mol, pinMap)
  }

  def mergePredSuccs(a: Map[RR, Set[RR]], b: Map[RR, Set[RR]]): Map[RR, Set[RR]] = {
    (a.toSeq ++ b.toSeq)
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2))
      )
      .map {
        (rr, drrs) =>
          {
            (rr, drrs.flatten.toSet)
          }
      }
  }

  def compatibleIdentityIds(id0: BlockPortID, id1: BlockPortID): Boolean = {
    (id0.width == id1.width) && (id0.pt == PortType.not(id1.pt)) && (id0.pmw == id1.pmw) && (id0.dummy == id1.dummy)
  }

  def addIdentityEdges(rrg: RRG): RRG = {
    val nSuccs = rrg.prims.toSeq
      .filter(_.annos.contains(ACanIdentity))
      .map {
        prim =>
          {
            val idPins = prim.pinToSrcSink
              .map(_._2)
              .filter(_.rrType.pmw.pm.isInstanceOf[PMData])
              .map {
                case sink: RRSink     => rrg.preds(sink)
                case source: RRSource => rrg.succs(source)
              }
              .flatten

            val (inIdPins, outIdPins) = idPins.partition(_.rrType.pt == PTInput)

            inIdPins.map {
              inIdPin =>
                {
                  (
                    inIdPin,
                    outIdPins
                      .filter(
                        outIdPin => compatibleIdentityIds(inIdPin.rrType, outIdPin.rrType)
                      )
                      .toSet
                  )
                }
            }
          }
      }
      .flatten
      .toMap

    val nPreds = nSuccs
      .map {
        (src, sinks) =>
          {
            sinks.map {
              sink =>
                {
                  (sink, src)
                }
            }
          }
      }
      .flatten
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2).toSet)
      )

    val nRRGPreds = mergePredSuccs(rrg.preds, nPreds)
    val nRRGSuccs = mergePredSuccs(rrg.succs, nSuccs)

    RRG(rrg.molName, rrg.prims, rrg.rrs, nRRGPreds, nRRGSuccs, rrg.srcSinkToPrim, rrg.t, rrg.pb, rrg.pinMap)
  }

  // TODO Add pins, add GI, conect everything together...
  def addExternalInterconnect(rrg: RRG): RRG = {
    val nPins = MSet[RRPin]()
    val nSrcSink = MMap[RRSourceSink, RRGPrim]()
    val nPreds = MMap[RR, Set[RR]]()
    val nSuccs = MMap[RR, Set[RR]]()

    val nIoPrims = rrg.prims.filter(_.annos.contains(ATileIo)).map {
      ioPrim =>
        {
          val nAnnos = ioPrim.annos + ACanIdentity
          val nPinToSrcSink = ioPrim.pinToSrcSink
            .map {
              (pin, srcSink) =>
                {
                  srcSink match {
                    case src @ RRSource(name, capacity, rrType) => {
                      val srcPins = rrg.succs(src)
                      assert(srcPins.size == 1)

                      val srcPin = srcPins.head.asInstanceOf[RRPin]

                      val nPinName = srcPin.name.replace(PTOutput.str, PTInput.str)
                      val nPrimName = srcPin.primName
                      val nPin = RRPin(nPrimName, nPinName, srcPin.capacity, srcPin.rrType.flipped(), srcPin.loc)

                      nPins += nPin

                      val nSinkName = name.replace("source_", "sink_").replace(PTOutput.str, PTInput.str)
                      val nSink = RRSink(nSinkName, capacity, rrType.flipped())

                      nSuccs += (nPin -> Set(nSink))
                      nPreds += (nSink -> Set(nPin))

                      (pin, src) :: (pin.flipped, nSink) :: Nil
                    }

                    case sink @ RRSink(name, capacity, rrType) => {
                      val sinkPins = rrg.preds(sink)
                      assert(sinkPins.size == 1)

                      val sinkPin = sinkPins.head.asInstanceOf[RRPin]

                      val nPinName = sinkPin.name.replace(PTInput.str, PTOutput.str)
                      val nPrimName = sinkPin.primName
                      val nPin = RRPin(nPrimName, nPinName, sinkPin.capacity, sinkPin.rrType.flipped(), sinkPin.loc)

                      nPins += nPin

                      val nSourceName = name.replace("sink_", "source_").replace(PTInput.str, PTOutput.str)
                      val nSource = RRSource(nSourceName, capacity, rrType.flipped())

                      nPreds += (nPin -> Set(nSource))
                      nSuccs += (nSource -> Set(nPin))

                      (pin, sink) :: (pin.flipped, nSource) :: Nil
                    }
                  }
                }
            }
            .flatten
            .toMap

          val nIo = RRGPrim(ioPrim.name, ioPrim.block, nPinToSrcSink, nAnnos, ioPrim.dagIds)

          nPinToSrcSink.map(_._2).map {
            srcSink =>
              {
                nSrcSink += (srcSink -> nIo)
              }
          }

          nIo
        }
    }

    val widths = rrg.prims
      .filter(_.annos.contains(ATileIo))
      .map(_.pinToSrcSink.map(_._1.concreteWidth))
      .flatten
      .toSet

    val gis = widths.map {
      w =>
        {
          (w, RRGI("", w))
        }
    }.toMap

    nPins.foreach {
      rrPin =>
        {
          rrPin.rrType.pt match {
            case PTInput => {
              val w = rrPin.rrType.concreteWidth

              nPreds += (rrPin -> Set(gis(w)))

              if (nSuccs.contains(gis(w))) {
                nSuccs(gis(w)) = nSuccs(gis(w)) + rrPin
              } else {
                nSuccs += (gis(w) -> Set(rrPin))
              }
            }

            case PTOutput => {
              val w = rrPin.rrType.concreteWidth

              nSuccs += (rrPin -> Set(gis(w)))

              if (nPreds.contains(gis(w))) {
                nPreds(gis(w)) = nPreds(gis(w)) + rrPin
              } else {
                nPreds += (gis(w) -> Set(rrPin))
              }
            }

            case PTUndef => scala.sys.error("Expected defined port direction")
          }
        }
    }

    val nPrims = rrg.prims.filter(!_.annos.contains(ATileIo)) ++ nIoPrims
    val nRRs = rrg.rrs ++ gis.map(_._2) ++ nPins ++ nSrcSink.map(_._1)
    val finalPreds = rrg.preds ++ nPreds.toMap
    val finalSuccs = rrg.succs ++ nSuccs.toMap
    val nSrcSinkToPrim = rrg.srcSinkToPrim.filter(!_._2.annos.contains(ATileIo)) ++ nSrcSink.toMap

    RRG(rrg.molName, nPrims, nRRs, finalPreds, finalSuccs, nSrcSinkToPrim, rrg.t, rrg.pb, rrg.pinMap)
  }

  def gen(tile: RootPb, t: Tile, params: GlobalParamsInst): RRG = {
    val internalRRG = findInternalRRG(params, t, tile)
    addIdentityEdges(addExternalInterconnect(internalRRG))
  }
}
