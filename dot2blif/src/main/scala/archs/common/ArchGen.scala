package archs

import arch.PortType
import arch.PortMeaning
import arch.PortBundle
import arch.Global
import arch.Local
import arch.BlockPortID
import arch.LocatedBlockPortID
import arch.PortMeaningWrapper
import arch.TileCombinator
import arch.Mode
import arch.Pin
import arch.PrimMode
import arch.AbsMode
import arch.Tile
import arch.LocType
import arch.TileOr
import arch.TileAndThen
import arch.DagId
import arch.TileAnd
import arch.Regular
import core._
import arch.DagId
import arch.Arch
import arch.PbType
import arch.HSValid
import arch.HSReady
import arch.BlockPort
import arch.PTInput
import arch.PTOutput
import arch.PTUndef
import arch.Impl
import arch.Hs
import arch.D
import arch.Vld
import arch.Rdy
import arch.HSType
import math.ceil
import crkt.PortNodeID

import collection.mutable.{Set => MSet}
import collection.mutable.{Map => MMap}
import math.max
import arch.PbLoc

type ModeInstantiator = (c: TileCombinator) => AbsMode

@FunctionalInterface
trait ArchGenerator[TP <: Params] {
  def apply(baseName: String, modifier: String, tp: TP): ModeInstantiator
}

@FunctionalInterface
trait Namer[T <: Params, TP <: Params] {
  def apply(p: T, primitiveP: Option[PrimitiveParams] = None): ArchGenerator[TP]
}

trait TileGen {
  def apply(): Tile
}

object PrimitiveParams {
  lazy val empty = PrimitiveParams(Map(), Set(), Map())
}

case class PrimitiveParams(
    locConfig: Map[LocatedBlockPortID, LocType],
    annos: Set[Annotation],
    castedPorts: Map[BlockPortID, BlockPortID]
)

sealed trait VPRLoc {
  def layoutPrint(tName: String): String
}
case class Fill(priority: Int) extends VPRLoc {
  def layoutPrint(tName: String) = "<fill type=\"" + tName + "\" priority=\"" + priority + "\"" + "/>\n"
}

case class Perimeter(priority: Int) extends VPRLoc {
  def layoutPrint(tName: String) = "<perimeter type=\"" + tName + "\" priority=\"" + priority + "\"" + "/>\n"
}

case class Corners(priority: Int) extends VPRLoc {
  def layoutPrint(tName: String) = "<corners type=\"" + tName + "\" priority=\"" + priority + "\"" + "/>\n"
}

case class Single(priority: Int, xLoc: String, yLoc: String) extends VPRLoc {
  def layoutPrint(tName: String) = "<single type=\"" + tName + "\" priority=\"" + priority + "\""
    + " x=\"" + xLoc + "\""
    + " y=\"" + yLoc + "\"" + "/>\n"
}

case class Col(priority: Int, startx: String, repeatx: String, starty: String, incry: String) extends VPRLoc {
  def layoutPrint(tName: String) = "<col type=\"" + tName + "\" priority=\"" + priority + "\""
    + " startx=\"" + startx + "\""
    + " repeatx=\"" + repeatx + "\""
    + " starty=\"" + starty + "\""
    + " incry=\"" + incry + "\"" + "/>\n"
}

case class Row(priority: Int, startx: String, incrx: String, starty: String, repeaty: String) extends VPRLoc {
  def layoutPrint(tName: String) = "<row type=\"" + tName + "\" priority=\"" + priority + "\""
    + " starty=\"" + starty + "\""
    + " repeaty=\"" + repeaty + "\""
    + " startx=\"" + startx + "\""
    + " incrx=\"" + incrx + "\"" + "/>\n"
}

case class Region(
    priority: Int,
    startx: String,
    endx: String,
    repeatx: String,
    incrx: String,
    starty: String,
    endy: String,
    repeaty: String,
    incry: String
) extends VPRLoc {
  def layoutPrint(tName: String) = "<region type=\"" + tName + "\" priority=\"" + priority + "\""
    + " startx=\"" + startx + "\""
    + " endx=\"" + endx + "\""
    + " repeatx=\"" + repeatx + "\""
    + " incrx=\"" + incrx + "\""
    + " starty=\"" + starty + "\""
    + " endy=\"" + endy + "\""
    + " repeaty=\"" + repeaty + "\""
    + " incry=\"" + incry + "\"" + "/>\n"
}

sealed trait VPRFCType(val str: String)
case object Frac extends VPRFCType("frac")
case object Abs extends VPRFCType("abs")

case class VPRFC(t: VPRFCType, value: Double) {
  override def toString(): String = {
    t match {
      case Frac => ("f" + value)
      case Abs  => ("a" + value)
    }
  }

  def getFc(chanWidth: Int): Int = {
    t match {
      case Frac => ceil(value * chanWidth).toInt
      case Abs  => value.toInt
    }
  }
}

sealed trait VPRPinLocation(val str: String) {
  def pinLocXML(t: PbType, arch: Arch): xml.Elem

  def printBp(t: PbType, bp: BlockPort, arch: Arch): Seq[String] = {
    printBp(t, bp, arch, 0, bp.words)
  }

  def printBp(t: PbType, bp: BlockPort, arch: Arch, from: Int, to: Int): Seq[String] = {
    assert(from >= 0, "" + bp + ": " + from + ", " + to)
    assert(to <= bp.words, "" + bp + ": " + from + ", " + to)

    if (arch.contains(bp.id)) {
      (from until to).map {
        w =>
          {
            Namer(PbLoc(t.name, Pin(bp.id, w)))
          }
      }
    } else {
      Nil
    }
  }
}

case object Spread extends VPRPinLocation("spread") {
  def pinLocXML(t: PbType, arch: Arch): xml.Elem = {
    <pinlocations pattern="spread">
    </pinlocations>
  }
}

case object Perimeter extends VPRPinLocation("perimeter") {
  def pinLocXML(t: PbType, arch: Arch): xml.Elem = {
    <pinlocations pattern="perimeter">
    </pinlocations>
  }
}

case object Split extends VPRPinLocation("custom") {
  case class BPRange(bp: BlockPort, from: Int, to: Int)

  def bucketSize(bucket: MSet[BPRange], arch: Arch): Int = {
    bucket.map {
      bpr =>
        {
          if (arch.contains(bpr.bp.id)) {
            bpr.to - bpr.from
          } else {
            0
          }
        }
    }.sum
  }

  def updateBucket(low: MMap[Int, MSet[BPRange]], high: MMap[Int, MSet[BPRange]], arch: Arch, bp: BlockPort): Unit = {
    val bpPhysWidth = bp.id.concreteWidth

    val numLow = bucketSize(low(bpPhysWidth), arch)
    val numHigh = bucketSize(high(bpPhysWidth), arch)

    val diff = numLow - numHigh

    val startLow = 0
    val endLow = max(1, diff + max(0, (bp.words - diff) / 2))

    assert(endLow >= 0, "failed to find bounds on " + bp + " with : (" + numLow + ", " + numHigh + "): " + endLow)
    assert(
      endLow <= bp.words,
      "failed to find bounds on " + bp + " with : (" + numLow + ", " + numHigh + "): " + endLow
    )

    low(bpPhysWidth) += BPRange(bp, startLow, endLow)

    if (endLow < bp.words) {
      val startHigh = endLow
      val endHigh = bp.words

      assert(
        startHigh >= 0,
        "failed to find bounds on " + bp
          + " with : (" + numLow + ", " + numHigh + "): " + startHigh
      )
      assert(
        startHigh <= bp.words,
        "failed to find bounds on " + bp
          + " with : (" + numLow + ", " + numHigh + "): " + startHigh
      )

      assert(
        endHigh >= 0,
        "failed to find bounds on " + bp
          + " with : (" + numLow + ", " + numHigh + "): " + endHigh
      )
      assert(
        endHigh <= bp.words,
        "failed to find bounds on " + bp
          + " with : (" + numLow + ", " + numHigh + "): " + endHigh
      )

      high(bpPhysWidth) += BPRange(bp, startHigh, endHigh)
    }
  }

  def updateBuckets(
      low: MMap[Int, MSet[BPRange]],
      high: MMap[Int, MSet[BPRange]],
      t: PbType,
      arch: Arch,
      bp: BlockPort
  ): Unit = {
    val bpPhysWidth = bp.id.concreteWidth

    if (low.get(bpPhysWidth).isEmpty) {
      low += ((bpPhysWidth, MSet[BPRange]()))
    }

    if (high.get(bpPhysWidth).isEmpty) {
      high += ((bpPhysWidth, MSet[BPRange]()))
    }

    val numLow = bucketSize(low(bpPhysWidth), arch)
    val numHigh = bucketSize(high(bpPhysWidth), arch)

    if (numHigh > numLow) {
      updateBucket(low, high, arch, bp)
    } else {
      updateBucket(high, low, arch, bp)
    }
  }

  def printBPR(t: PbType, arch: Arch, bpr: BPRange): Seq[String] = {
    (bpr.from until bpr.to).map {
      i =>
        {
          val loc = PbLoc(t.name, Pin(bpr.bp.id, i))
          Namer(loc)
        }
    }
  }

  def pinLocXML(t: PbType, arch: Arch): xml.Elem = {
    val topPorts = MMap[Int, MSet[BPRange]]() // Output Ports
    val rightPorts = MMap[Int, MSet[BPRange]]() // Output Ports
    val bottomPorts = MMap[Int, MSet[BPRange]]() // Input Ports
    val leftPorts = MMap[Int, MSet[BPRange]]() // Input Ports

    t.bi.ports
      .filter(
        (id, bp) => bp.nonEmptyUnder(arch)
      )
      .map(_._2)
      .foreach {
        bp =>
          {
            val bpPhysWidth = bp.id.concreteWidth

            bp.pt match {
              case PTInput => {
                updateBuckets(bottomPorts, leftPorts, t, arch, bp)
              }

              case PTOutput => {
                updateBuckets(topPorts, rightPorts, t, arch, bp)
              }

              case PTUndef => scala.sys.error("Enexpected Port Type")
            }
          }
      }

    val clkPort = if (t.bi.clocked) t.name + ".clk" else ""

    val topPortsStr = topPorts.toSeq.map(_._2).flatten.map(printBPR(t, arch, _)).flatten.mkString(" ") + " " + clkPort
    val rightPortsStr = rightPorts.toSeq.map(_._2).flatten.map(printBPR(t, arch, _)).flatten.mkString(" ")
    val bottomPortsStr = bottomPorts.toSeq.map(_._2).flatten.map(printBPR(t, arch, _)).flatten.mkString(" ")
    val leftPortsStr = leftPorts.toSeq.map(_._2).flatten.map(printBPR(t, arch, _)).flatten.mkString(" ")

    <pinlocations pattern="custom">
      <loc side="left">{leftPortsStr}</loc>
      <loc side="right">{rightPortsStr}</loc>
      <loc side="top">{topPortsStr}</loc>
      <loc side="bottom">{bottomPortsStr}</loc>
    </pinlocations>
  }
}

case object InTopOutBottom extends VPRPinLocation("custom") {
  def pinLocXML(t: PbType, arch: Arch): xml.Elem = {
    val ports = t.bi.ports
      .filter(
        (id, bp) => bp.nonEmptyUnder(arch)
      )
      .map(_._2)
    val clkPort = if (t.bi.clocked) t.name + ".clk" else ""

    val (inPorts, outPorts) = ports.partition(_.id.pt == PTInput)
    val inPortsStr = inPorts.map(printBp(t, _, arch)).flatten.mkString(" ") + " " + clkPort
    val outPortsStr = outPorts.map(printBp(t, _, arch)).flatten.mkString(" ")

    <pinlocations pattern="custom">
      <loc side="left"></loc>
      <loc side="right"></loc>
      <loc side="top">{inPortsStr}</loc>
      <loc side="bottom">{outPortsStr}</loc>
    </pinlocations>
  }
}

sealed trait VPRSide(val str: String)
case object STop extends VPRSide("top")
case object SBottom extends VPRSide("bottom")
case object SRight extends VPRSide("right")
case object SLeft extends VPRSide("left")

case class OneSided(side: VPRSide) extends VPRPinLocation("custom") {
  def getSide(pSide: VPRSide, portsStr: String): xml.Elem = {
    val sideStr = pSide.str

    if (pSide == side) {
      <loc side={sideStr}>{portsStr}</loc>
    } else {
      <loc side={sideStr}></loc>
    }
  }

  def pinLocXML(tile: PbType, arch: Arch): xml.Elem = {
    val ports = tile.bi.ports
      .filter(
        (id, bp) => bp.nonEmptyUnder(arch)
      )
      .map(_._2)
      .map(printBp(tile, _, arch))
      .flatten
      .mkString(" ")

    val clkPort = if (tile.bi.clocked) tile.name + ".clk" else ""
    val allPorts = ports + " " + clkPort

    val l = getSide(SLeft, allPorts)
    val r = getSide(SRight, allPorts)
    val t = getSide(STop, allPorts)
    val b = getSide(SBottom, allPorts)

    <pinlocations pattern="custom">
      {l}
      {r}
      {t}
      {b}
    </pinlocations>
  }
}

case object Dup extends VPRPinLocation("custom") {
  def pinLocXML(t: PbType, arch: Arch): xml.Elem = {
    val ports = t.bi.ports
      .filter(
        (id, bp) => bp.nonEmptyUnder(arch)
      )
      .map(_._2)
      .map(printBp(t, _, arch))
      .flatten
      .mkString(" ")

    val clkPort = if (t.bi.clocked) t.name + ".clk" else ""
    val allPorts = ports + " " + clkPort

    <pinlocations pattern="custom">
      <loc side="left">{allPorts}</loc>
      <loc side="right">{allPorts}</loc>
      <loc side="top">{allPorts}</loc>
      <loc side="bottom">{allPorts}</loc>
    </pinlocations>
  }
}

// TODO we do not support custom switch block patterns yet
sealed trait SwitchPattern(val str: String)
case object SWExtFullIntStraight extends SwitchPattern("external_full_internal_straight")
case object SWAll extends SwitchPattern("all")
case object SWExternal extends SwitchPattern("external")
case object SWInternal extends SwitchPattern("internal")
case object SWNone extends SwitchPattern("none")

trait VPRConfig {
  def capacity: Int
  def loc: VPRLoc
  def pinLocation: VPRPinLocation
  def height: Int
  def width: Int
  def switchblockPattern: SwitchPattern

  // TODO also print FCtype
  override def toString(): String = {
    val scap = "capacity = " + capacity
    val sloc = "location = " + loc.toString()
    val sh = "height = " + height
    val sw = "width = " + width
    val sPinLoc = "pinLocation = " + pinLocation.str
    val sSW = "switchblock pattern = " + switchblockPattern.str

    (scap :: sloc :: sh :: sw :: sPinLoc :: sSW :: Nil).mkString("\n  ")
  }
}

object EmptyVPRConfig extends VPRConfig {
  override val capacity = 1
  override val loc = Fill(100000)
  override val pinLocation = Spread
  override val height = 1
  override val width = 1
  override val switchblockPattern = SWExtFullIntStraight
}

object ArchGeneratorUtils {
  // val blockNameToBlock = (
  //   Branch :: Fork :: Merge :: Mux :: CntrlMerge :: Entry :: Exit :: Source :: Select
  //     :: Constant :: Sink :: TEHB :: OEHB :: EB :: Comparator(AnyComp) :: Mult :: Operator(AnyOp) :: End :: Nil
  // ).map(b => (b.typeString -> b)).toMap

  val keywords = Set("bypassOut", "bypassIn", "bypass")

  def getLoc(
      width: Int,
      pt: PortType,
      pm: PortMeaning,
      pb: PortBundle,
      loc: Int,
      locString: String
  ): (LocatedBlockPortID, LocType) = {
    val locType = locString match {
      case "glob" => Global
      case other  => Local(other)
    }

    val id = BlockPortID(width, pt, PortMeaningWrapper(pm, pb), Regular)
    (LocatedBlockPortID(id, loc) -> locType)
  }

  def getId(width: Int, pt: PortType, pm: PortMeaning, pb: PortBundle): BlockPortID = {
    BlockPortID(width, pt, PortMeaningWrapper(pm, pb), Regular)
  }

  def getPrimId(id: String) = Map[NamedAttribute, String]((NAPrimID -> id))

  def name(compName: String, modifier: String): String = compName + "__" + modifier

  def combine(
      name: String,
      modes: List[ModeInstantiator],
      c: TileCombinator,
      topC: TileCombinator,
      annos: Set[Annotation]
  ): Mode = {
    val childModes = modes.map(
      m => m(c)
    )

    Mode(name, childModes, topC, Map(), annos, Map(), None)
  }

  def or(name: String, modes: List[ModeInstantiator], topC: TileCombinator, annos: Set[Annotation] = Set()): Mode = {
    combine(name, modes, TileOr, topC, annos)
  }

  def chain(name: String, modes: List[ModeInstantiator], topC: TileCombinator, annos: Set[Annotation] = Set()): Mode = {
    combine(name, modes, TileAndThen, topC, annos)
  }

  def and(name: String, modes: List[ModeInstantiator], topC: TileCombinator, annos: Set[Annotation] = Set()): Mode = {
    combine(name, modes, TileAnd, topC, annos)
  }

  def overrideDagId(m: AbsMode, dagId: DagId): AbsMode = {
    if (m.modes.isEmpty) {
      m.withDagId(dagId, Nil)
    } else {
      val recModes = m.modes.map(overrideDagId(_, dagId))
      m.withDagId(dagId, recModes)
    }
  }

  def repeat[TP <: Params](
      dagId: String,
      modifier: String,
      n: Int,
      mode: ArchGenerator[TP],
      tp: TP
  ): ModeInstantiator = {
    val modes = (0 until n).map {
      i =>
        {
          val dId = DagId(dagId, i)
          overrideDagId(mode(dagId, i.toString() + modifier, tp)(TileAnd), dId)
        }
    }.toList

    (c: TileCombinator) => Mode(dagId + modifier + "rep", modes, c, Map(), Set(), Map(), None)
  }

  val bypassOutMode = (c: TileCombinator) => {
    assert(c == TileOr)
    Mode("bypassOut", Nil, c, Map(), Set(), Map(), None)
  }

  val bypassOutAnyMode = (c: TileCombinator) => {
    assert(c == TileOr)
    Mode("bypassOutAny", Nil, c, Map(), Set(), Map(), None)
  }

  val bypassInMode = (c: TileCombinator) => {
    assert(c == TileOr)
    Mode("bypassIn", Nil, c, Map(), Set(), Map(), None)
  }

  val bypassMode = (c: TileCombinator) => {
    assert(c == TileOr)
    Mode("bypass", Nil, c, Map(), Set(), Map(), None)
  }
}
