package arch

import util.Util.log2ceil
import math.pow
import math.min
import core._
import archs.VPRConfig
import analysis.AreaInfo
import analysis.RRGConInfo
import archs.Primitive
import archs.Params

import io.AnsiColor._

sealed trait PbType {
  def bi: BlockInterface
  def links: List[Link]
  def modeLinks: List[Link]
  def c: TileCombinator
  def annotations: Set[Annotation]
  def name: String
  def subBlocks: Seq[PbType]

  def withName(nName: String): PbType
  def withBi(nBi: BlockInterface): PbType
  def withAnnotation(annot: Annotation): PbType

  def getBlocks(): Set[TBlock]

  def nonEmptyUnder(arch: Arch): Boolean = {
    bi.nonEmptyUnder(arch)
  }

  def vprName: String = name.replace("/", "").replace(":", "")

  def hasClockPin(): Boolean = {
    this match {
      case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
        prim.physicalInfo.clocked
      }

      case other =>
        links.collect {
          case clk: CLK => clk
        }.nonEmpty
    }
  }
}

sealed trait NonLeafPb extends PbType

case class RootPb(
    bi: BlockInterface,
    links: List[Link],
    modeLinks: List[Link],
    c: TileCombinator,
    annotations: Set[Annotation],
    name: String,
    subBlocks: Seq[PbType],
    vprConfig: VPRConfig
) extends NonLeafPb
    with AbsTile {
  override def toString(): String = {
    val subC = subBlocks.head.c

    GREEN + "RootPb " + RESET + name
      + YELLOW + " [" + RESET + annotations.mkString(", ") + YELLOW + "]" + RESET
      + YELLOW + " (" + RESET + subBlocks.map(_.name).mkString(" " + subC + " ") + YELLOW + ")" + RESET
  }

  def withName(nName: String): RootPb = {
    RootPb(bi, links, modeLinks, c, annotations, nName, subBlocks, vprConfig)
  }

  def withBi(nBi: BlockInterface): RootPb = {
    RootPb(nBi, links, modeLinks, c, annotations, name, subBlocks, vprConfig)
  }

  def withAnnotation(annot: Annotation): RootPb = {
    RootPb(bi, links, modeLinks, c, annotations + annot, name, subBlocks, vprConfig)
  }

  def getBlocks(): Set[TBlock] = {
    subBlocks.map(_.getBlocks()).flatten.toSet
  }

  def ioInfo(): Map[(PortType, Int), Int] = {
    bi.ports
      .map(_._2)
      .map(_.toPins())
      .flatten
      .map {
        pin =>
          {
            assert(
              (pin.id.pmw.pb == D) || (pin.id.pmw.pb == Vld) || (pin.id.pmw.pb == Rdy),
              "Should only call ioInfo when the tile is lowered.."
            )
            (pin.id.pt, pin.id.concreteWidth)
          }
      }
      .groupBy(
        k => k
      )
      .map(
        (k, v) => (k, v.size)
      )
  }
}

case class InterPb(
    bi: BlockInterface,
    links: List[Link],
    modeLinks: List[Link],
    c: TileCombinator,
    annotations: Set[Annotation],
    name: String,
    subBlocks: Seq[PbType]
) extends NonLeafPb {
  override def toString(): String = {
    val subC = subBlocks.head.c

    GREEN + "InterPb " + RESET + name
      + YELLOW + " [" + RESET + annotations.mkString(", ") + YELLOW + "]" + RESET
      + YELLOW + " (" + RESET + subBlocks.map(_.name).mkString(" " + subC + " ") + YELLOW + ")" + RESET
  }

  def withName(nName: String): InterPb = {
    InterPb(bi, links, modeLinks, c, annotations, nName, subBlocks)
  }

  def withBi(nBi: BlockInterface): InterPb = {
    InterPb(nBi, links, modeLinks, c, annotations, name, subBlocks)
  }

  def withAnnotation(annot: Annotation): InterPb = {
    InterPb(bi, links, modeLinks, c, annotations + annot, name, subBlocks)
  }

  def getBlocks(): Set[TBlock] = {
    subBlocks.map(_.getBlocks()).flatten.toSet
  }
}

case class PrimPb(
    bi: BlockInterface,
    links: List[Link],
    modeLinks: List[Link],
    c: TileCombinator,
    annotations: Set[Annotation],
    name: String,
    prim: TBlock,
    pinMap: Map[Pin, Pin], // Pb pins to TBlock pins
    dagIds: Option[DagId]
) extends PbType {
  override def toString(): String = {
    GREEN + "PrimPb " + RESET + name
      + YELLOW + " [" + RESET + annotations.mkString(", ") + YELLOW + "]" + RESET
      + YELLOW + " ()" + RESET
  }

  def subBlocks: Seq[PbType] = Seq()

  def withName(nName: String): PrimPb = {
    PrimPb(bi, links, modeLinks, c, annotations, nName, prim, pinMap, dagIds)
  }

  def withBi(nBi: BlockInterface): PrimPb = {
    PrimPb(nBi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds)
  }

  def withAnnotation(annot: Annotation): PrimPb = {
    PrimPb(bi, links, modeLinks, c, annotations + annot, name, prim, pinMap, dagIds)
  }

  def getBlocks(): Set[TBlock] = {
    Set(prim)
  }
}

case class PbLoc(pbName: String, pin: Pin)

sealed trait Link {
  def delay: Option[CombTiming]
}

object Direct {
  def defaultWords(src: BlockPort, srcAt: Int, dst: BlockPort, dstAt: Int): Int = {
    min(src.words - srcAt, dst.words - dstAt)
  }

  def genDirects(
      srcLoc: PbLoc,
      dstLoc: PbLoc,
      words: Int
  ): Seq[Direct] = {
    (0 until words).map {
      w =>
        {
          val srcPin = Pin(srcLoc.pin.id, srcLoc.pin.loc + w)
          val dstPin = Pin(dstLoc.pin.id, dstLoc.pin.loc + w)

          Direct(PbLoc(srcLoc.pbName, srcPin), PbLoc(dstLoc.pbName, dstPin), None)
        }
    }
  }
}

case class Direct(
    srcLoc: PbLoc,
    dstLoc: PbLoc,
    delay: Option[CombTiming]
) extends Link {
  override def toString(): String = {
    "d: " + srcLoc + " -> " + dstLoc
  }
}

object Mux {
  def defaultWords(sources: List[(BlockPort, Int)], dst: (BlockPort, Int)): Int = {
    val srcWords = sources.map(_._1.words).reduce(min)
    min(srcWords, dst._2)
  }

  def genMuxs(
      sources: List[PbLoc],
      dst: PbLoc,
      words: Int
  ): Seq[Mux] = {
    (0 until words).map {
      w =>
        {
          val nSources = sources.map {
            case PbLoc(pb, pin) => {
              PbLoc(pb, Pin(pin.id, pin.loc + w))
            }
          }.toList

          val nDst = PbLoc(dst.pbName, Pin(dst.pin.id, dst.pin.loc + w))

          Mux(nSources, nDst, None)
        }
    }
  }
}

// Pack patterns only apply to muxes linked to bypassable components
case class Mux(sources: List[PbLoc], dstLoc: PbLoc, delay: Option[CombTiming]) extends Link {
  override def toString(): String = {
    "Mux: " + sources.mkString(", ") + " -> " + dstLoc
  }
}

case class CLK(srcPb: String, dstPb: String) extends Link {
  val delay: Option[CombTiming] = None

  override def toString(): String = {
    srcPb + ".clk -> " + dstPb + ".clk"
  }
}
