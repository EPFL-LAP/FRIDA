package arch

import scala.collection.mutable
import math.max

object Bypass {
  val bypass = "bypass"
  val bypassNames = Set(bypass, BPOut.str, BPIn.str, BPOutAny.str)

  def isModeBypass(m: AbsMode): Boolean = {
    bypassNames contains m.name
  }

  def isBypassWrapper(m: Mode): Boolean = {
    m.modes.filter(bypassNames contains _.name).nonEmpty
  }

  def getByPass(modeNames: List[String]): Set[Bypass] = {
    if (modeNames contains bypass) {
      Set(BPOut, BPIn) // TODO should this not be removed??
    } else {
      (BPOut :: BPIn :: BPOutAny :: Nil).filter(byp => modeNames.toSet.contains(byp.str)).toSet
    }
  }
}

sealed trait Bypass(val str: String)
case object BPOut extends Bypass("bypassOut")
case object BPOutAny extends Bypass("bypassOutAny")
case object BPIn extends Bypass("bypassIn")

// Because of matchers, only support bypass of primitives at the moment.
// Also maybe supports bypass of general mode if they do not have ambiguous handshake.
// But though of primitive bypass only.

object ByPassHandler {
  def getPortFrom(ports: Map[BlockPortID, BlockPort], canonId: BlockPortID, literal: Boolean): Option[BlockPort] = {
    val canonToBp = ports.map(
      (k, v) => (InterfaceCombiner.canonBlockPort(v, literal).id, v)
    )

    canonToBp.get(canonId)
  }

  def findModeInterface(
    m: PbElem,
    childrenPbs: List[PbType],
    bypassInfo: Set[Bypass],
    literal: Boolean
  ): (BlockInterface, List[PbType]) = {
    assert(childrenPbs.size == 1, childrenPbs.map(_.name))

    val bypassedMode = InterfaceCombiner.canonicalizeInterface(childrenPbs.head.bi, literal)

    val log = false // (m.isInstanceOf[Mode]) && m.asInstanceOf[Mode].uniqueName().contains("BpCst1__0")

    def merge(ports: Seq[Seq[(BlockPortID, BlockPort)]]): Map[BlockPortID, BlockPort] = {
      if (ports.size == 1) {
        ports.head.toMap
      } else {
        ports.flatten.map(_._2).groupBy(_.id).map {
          (id, bps) =>
            {
              if (bps.size == 1) {
                (id, bps.head)
              } else {
                val maxWords = bps.map(_.words).reduce(max)

                (id, BlockPort(id, maxWords, Set()))
              }
            }
        }
      }
    }

    def rec(byp: Bypass): Seq[(BlockPortID, BlockPort)] = {
      val refSide = byp match {
        case BPOut => PTOutput
        case BPIn  => PTInput
        case BPOutAny => PTOutput
      }

      val refModePorts = bypassedMode.ports.filter(_._1.pt == refSide).toSeq
      val oModePorts = bypassedMode.ports.filter(_._1.pt != refSide)

      val oTopPorts = refModePorts.map {
        (id, bp) =>
          {
            val opId = id.flipped()
            val words = oModePorts
              .get(opId)
              .fold(bp.words)(
                modeBp => max(bp.words, modeBp.words)
              )

            val opBp = getPortFrom(oModePorts, opId, literal).fold {
              BlockPort(opId, words, Set())
            } {
              bp => bp
            }

            (opBp.id, opBp)
          }
      }

      val oPorts = merge(oModePorts.toSeq :: oTopPorts.toSeq :: Nil)

      refModePorts ++ oPorts
    }

    val ports = merge(bypassInfo.toSeq.map(rec(_)))

    val nBi = BlockInterface(ports, m.name, bypassedMode.clocked)
    val nBiCanon = InterfaceCombiner.canonicalizeInterface(nBi, literal)

    (nBiCanon, childrenPbs)
  }

  def containsLoc(bi: BlockInterface, loc: PbLoc, literal: Boolean): Boolean = {
    val canonBi = bi.ports.map(
      (k, v) => (InterfaceCombiner.canonBlockPort(v, literal).id, v)
    )
    canonBi
      .get(loc.pin.id)
      .fold(false)(
        bp => bp.words > loc.pin.loc
      )
  }

  def validLocs(
    srcTopLoc: PbLoc,
    bypassed: BlockInterface,
    top: BlockInterface,
    literal: Boolean,
    bpType: Bypass
  ): Set[PbLoc] = {
    val dstTopLoc = PbLoc(srcTopLoc.pbName, srcTopLoc.pin.flipped)
    val srcBypLoc = PbLoc(bypassed.pbName, srcTopLoc.pin)
    val dstBypLoc = PbLoc(bypassed.pbName, srcTopLoc.pin.flipped)

    val tail = if((bpType == BPOutAny) && (srcTopLoc.pin.id.pt == PTInput) && (srcTopLoc.pin.loc > 0)) {
      PbLoc(srcTopLoc.pbName, Pin(srcTopLoc.pin.flipped.id, 0)) :: Nil
    } else {
      Nil
    }

    val biNameMap = Map[String, BlockInterface]((bypassed.pbName -> bypassed), (top.pbName -> top))

    val locs = (srcTopLoc :: dstTopLoc :: srcBypLoc :: dstBypLoc :: tail).filter {
      loc =>
        {
          containsLoc(biNameMap(loc.pbName), loc, literal)
        }
    }

    locs.toSet
  }

  // Only valid if loc0 and loc1 part of the same set created by matchingLocs
  def matchingLocs(loc0: PbLoc, loc1: PbLoc, top: BlockInterface, bypassed: BlockInterface): Boolean = {
    loc0.pin.id.pt match {
      case PTInput => {
        loc1.pin.id.pt match {
          case PTInput  => (loc0.pbName == top.pbName) && (loc1.pbName == bypassed.pbName)
          case PTOutput => (loc0.pbName == top.pbName) && (loc1.pbName == top.pbName)
          case other    => scala.sys.error("Expected defined port direction.")
        }
      }

      case PTOutput => {
        loc1.pin.id.pt match {
          case PTInput  => false
          case PTOutput => (loc0.pbName == bypassed.pbName) && (loc1.pbName == top.pbName)
          case other    => scala.sys.error("Expected defined port direction.")
        }
      }

      case other => scala.sys.error("Expected defined port direction.")
    }
  }

  def getLinks(locs: Set[PbLoc], top: BlockInterface, bypassed: BlockInterface, log: Boolean): Set[Link] = {
    locs.toSeq.map(loc0 => locs.toSeq.map(loc1 => (loc0, loc1)))
      .flatten
      .filter((loc0, loc1) => matchingLocs(loc0, loc1, top, bypassed))
      .groupBy(_._2)
      .map(_._2)
      .map {
        locs =>
          {
            if (locs.size == 1) {
              val (srcLoc, dstLoc) = locs.head

              Direct(srcLoc, dstLoc, None)
            } else if (locs.size == 2) {
              val sources = locs.map(_._1).toList
              val dstLoc = locs.head._2

              Mux(sources, dstLoc, None)
            } else {
              scala.sys.error("In bypass can have at most to links to the same endpoint.")
            }
          }
      }.toSet
  }

  def getAllLinks(
    top: BlockInterface,
    bypassed: BlockInterface,
    literal: Boolean,
    bpType: Bypass,
    log: Boolean
  ): Set[Link] = {
    val linksDup = top.ports.map(_._2.toPins()).flatten.map(pin => PbLoc(top.pbName, pin)).map {
      loc => {
        getLinks(validLocs(loc, bypassed, top, literal, bpType), top, bypassed, log)
      }
    }.flatten.toSet

    linksDup.groupBy {
      case d: Direct => d.dstLoc
      case m: Mux => m.dstLoc
      case others => ???
    }.map(_._2).map {
      commonDst => {
        if(commonDst.size == 1) {
          commonDst.toSeq
        } else {
          assert(commonDst.collect { case m: Mux => m}.size == 1)

          val mux = commonDst.collect{ case m: Mux => m }.head
          assert(mux.delay.isEmpty)

          val nSources = commonDst.collect{ case d: Direct => d}.map(_.srcLoc)

          Mux(mux.sources ++ nSources, mux.dstLoc, None) :: Nil
        }
      }
    }.flatten.toSet
  }

  def findModeInterconnect(
    mode: PbElem,
    top: BlockInterface,
    interfaces: List[BlockInterface],
    literal: Boolean
  ): List[Link] = {
    assert(interfaces.size == 1)

    val log = false // mode.name == "bpOp2"

    val bypassed = interfaces.head
    val bpType = Bypass.getByPass(mode.modes.map(_.name))
    assert(bpType.size == 1)

    val links = getAllLinks(top, bypassed, literal, bpType.head, log)

    if (log) {
      println()
      
      println
      println("final links:")
      println(links.mkString("\n"))

      println(top)
      println(bypassed)

      ???
    }

    links.toList
  }
}
