package arch

import crkt.Node
import crkt.TNode
import crkt.Port

import core._

import archs.Params
import archs.Primitive
import archs.ConfigurationBits
import archs.ConfParams

import math.max
import math.min
import math.floor
import io.AnsiColor._
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.RegexParsers
import core.ACanIdentity
import core.AAllowPartialUsage
import frontend.GlobalParamsInst

sealed trait HSType(val str: String)
case object HSValid extends HSType("_vld")
case object HSReady extends HSType("_rdy")
// case object HSData extendsd HSType

object PortType {
  def not(pt: PortType): PortType = {
    pt match {
      case PTInput  => PTOutput
      case PTOutput => PTInput
      case PTUndef  => PTUndef
    }
  }
}

sealed trait PortType(val str: String) {
  def flipped: PortType
}

case object PTInput extends PortType("In") {
  def flipped: PortType = PTOutput
}

case object PTOutput extends PortType("Out") {
  def flipped: PortType = PTInput
}

// TODO maybe get rid of this...
case object PTUndef extends PortType("Undef") {
  def flipped: PortType = ???
}

// Only use to build the tiles in a simpler way
// GlobalNetId is localNet if need to be routed internally and a unique id if need to be routed externally

object LocationInfo {
  val unassigned = 0
}

sealed trait PortMeaning(val str: String) {
  def matcher: Option[String]
  def canonicalize(): PortMeaning
  def withMatcher(m: String): PortMeaning
}

case class PMData(matcher: Option[String]) extends PortMeaning("PMData") {
  def canonicalize(): PortMeaning = PMData(None)
  def withMatcher(m: String): PortMeaning = {
    PMData(Some(m))
  }

  override def toString(): String = str
}

case class PMCond(matcher: Option[String]) extends PortMeaning("PMCond") {
  def canonicalize(): PortMeaning = PMCond(None)
  def withMatcher(m: String): PortMeaning = {
    PMCond(Some(m))
  }

  override def toString(): String = str
}

case class PMAddr(matcher: Option[String]) extends PortMeaning("PMAddr") {
  def canonicalize(): PortMeaning = PMAddr(None)
  def withMatcher(m: String): PortMeaning = {
    PMAddr(Some(m))
  }

  override def toString(): String = str
}

case class PMConf(matcher: Option[String]) extends PortMeaning("PMConf") {
  def canonicalize(): PortMeaning = PMConf(None)
  def withMatcher(m: String): PortMeaning = {
    ???
  }

  override def toString(): String = str
}

object PortMeaning {
  val defLocation = 0
  val localUID = 0

  val all = PMData :: PMCond :: PMAddr :: Nil
}

// TODO could be simpler with better refactoring
sealed trait PortBundle {
  def str: String
  def isHandshake(): Boolean
  def isData(): Boolean
  def isImpl(): Boolean
  def contains: Set[PortBundle]
}

case object Impl extends PortBundle {
  val str = "Impl"
  val contains = Set(Impl, D, Hs, Vld, Rdy)

  def isHandshake(): Boolean = false
  def isData(): Boolean = false
  def isImpl(): Boolean = true
}

case object D extends PortBundle {
  val str = "D"
  val contains = Set(D)

  def isHandshake(): Boolean = false
  def isData(): Boolean = true
  def isImpl(): Boolean = false
}

case object Hs extends PortBundle {
  val str = "Hs"
  val contains = Set(Hs, Vld, Rdy)

  def isHandshake(): Boolean = true
  def isData(): Boolean = false
  def isImpl(): Boolean = false
}

case object Vld extends PortBundle {
  val str = "vld"
  val contains = Set(Vld)

  def isHandshake(): Boolean = true
  def isData(): Boolean = false
  def isImpl(): Boolean = false
}

case object Rdy extends PortBundle {
  val str = "rdy"
  val contains = Set(Rdy)

  def isHandshake(): Boolean = true
  def isData(): Boolean = false
  def isImpl(): Boolean = false
}

//sealed case class PortMeaningWrapper(pm: PortMeaning, implicitHanshake: Boolean, bundleType: HSType)
sealed case class PortMeaningWrapper(pm: PortMeaning, pb: PortBundle) {
  def canonicalize(): PortMeaningWrapper = {
    PortMeaningWrapper(pm.canonicalize(), pb)
  }
}

object PortMeaningWrapper {
  def empty = PortMeaningWrapper(PMData(None), Impl)
}

object BlockPortID {
  val empty: BlockPortID = BlockPortID(0, PTUndef, PortMeaningWrapper.empty, Regular)

  trait BPParser[T] extends RegexParsers {
    def expr: Parser[T]

    def portMeaning: Parser[PortMeaning] = (
      (PMData(None).str ^^^ PMData(None))
        | (PMCond(None).str ^^^ PMCond(None))
        | (PMAddr(None).str ^^^ PMAddr(None))
    )

    def portBundle: Parser[PortBundle] = {
      ((D.str ^^^ D)
        | (Vld.str ^^^ Vld)
        | (Rdy.str ^^^ Rdy))
    }

    def portType: Parser[PortType] = (
      (PTInput.str ^^^ PTInput)
        | (PTOutput.str ^^^ PTOutput)
        | (PTUndef.str ^^^ PTUndef)
    )

    def hsType: Parser[HSType] = (
      (HSValid.str ^^^ HSValid)
        | (HSReady.str ^^^ HSReady)
    )

    def dummy: Parser[DummyType] = (
      (Dummy.str ^^^ Dummy)
        | (Regular.str ^^^ Regular)
    )

    def matcher: Parser[String] = ???

    def number: Parser[String] = """\d+""".r

    def apply(input: String): T = parseAll(expr, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }

  object PortInstanceParser extends BPParser[Pin] {
    def expr: Parser[Pin] = portBundle ~ portType ~ portMeaning ~ number ~ dummy ~ "_" ~ number ^^ {
      case pb ~ pt ~ pm ~ width ~ dum ~ "_" ~ word => {
        val pmw = PortMeaningWrapper(pm, pb)
        val id = BlockPortID(width.toInt, pt, pmw, dum)

        Pin(id, word.toInt)
      }

      case other => scala.sys.error("Cannot parse PortInstance")
    }
  }
}

case class BlockPortID(width: Int, pt: PortType, pmw: PortMeaningWrapper, dummy: DummyType) {
  override def toString(): String = {
    "[" + RED + width + RESET + ", "
      + BLUE + pt.str + ", " + pmw.pm.str + ", " + pmw.pb + ", " + dummy.str + ", " + pmw.pm.matcher + RESET + "]"
  }

  def isDummy = dummy == Dummy

  def concreteWidth: Int = {
    pmw.pb match {
      case Impl => width
      case D    => width
      case Hs   => 2
      case Vld  => 1
      case Rdy  => 1
    }
  }

  def detailedString(): String = {
    val pmS = pmw.pm.str
    val isHs = pmw.pb
    val ptS = if (pt == PTInput) "in" else "out"
    val dum = dummy.str
    val matcher = pmw.pm.matcher
    "(" + BLUE + width + ", " + ptS + ", " + pmS + ", " + isHs + ", " + dum + ", " + matcher + RESET + ")"
  }

  def detailedStringBland(): String = {
    val pmS = pmw.pm.str
    val isHs = pmw.pb
    val ptS = if (pt == PTInput) "in" else "out"
    val dum = dummy.str
    val matcher = pmw.pm.matcher
    "(" + width + ", " + ptS + ", " + pmS + ", " + isHs + ", " + dum + ", " + matcher + ")"
  }

  def withMatcher(m: String): BlockPortID = {
    val nPm = pmw.pm.withMatcher(m)
    val nPmw = PortMeaningWrapper(nPm, pmw.pb)
    BlockPortID(width, pt, nPmw, dummy)
  }

  def canonicalize(): BlockPortID = {
    val nPmw = PortMeaningWrapper(pmw.pm.canonicalize(), pmw.pb)
    val nId = BlockPortID(width, pt, nPmw, dummy)

    nId
  }

  def flipped(): BlockPortID = {
    BlockPortID(width, PortType.not(pt), pmw, dummy)
  }

  def toDummy: BlockPortID = BlockPortID(width, pt, pmw, Dummy)
  def toRegular: BlockPortID = BlockPortID(width, pt, pmw, Regular)

  // TODO is a bit loosy because we don't check the implicit / explicit but should be fine for now
  def gt(that: BlockPortID): Boolean = {
    (this.width > that.width) || ((this.width == that.width) && (this.pmw.pm == that.pmw.pm))
  }
}

object BlockPort {
  val empty: BlockPort = BlockPort(BlockPortID.empty, 0, Set())
}

sealed abstract class ArchPort

// TODO remove this thing...
case class PortInstance(id: BlockPortID, word: Int, hsType: Option[HSType]) extends ArchPort {
  val width = id.width
  val pt = id.pt
  val pmw = id.pmw
  val pm = pmw.pm

  def hsString = hsType match {
    case Some(HSValid) => ".v"
    case Some(HSReady) => ".r"
    case None          => ""
  }

  override def toString(): String = {
    val hs = hsString

    id.toString() + "[" + YELLOW + word + RESET + "]" + hs
  }

  def physicalString(): String = {
    "(" + id.width + ", " + id.pt.str + ", " + id.pmw.pm + ", " + id.dummy.str + ")" + "[" + word + "]" + hsString
  }

  def getMatcher(): Option[String] = {
    pmw.pm.matcher
  }

  def toDummy: PortInstance = {
    val nId = BlockPortID(id.width, id.pt, id.pmw, Dummy)
    PortInstance(nId, word, hsType)
  }

  def toRegular: PortInstance = {
    val nId = BlockPortID(id.width, id.pt, id.pmw, Regular)
    PortInstance(nId, word, hsType)
  }

  def isDummy: Boolean = {
    id.dummy == Dummy
  }

  def toPin: Pin = hsType match {
    case None          => Pin(id, word)
    case Some(HSValid) => Pin(BlockPortID(id.width, id.pt, PortMeaningWrapper(id.pmw.pm, Vld), id.dummy), word)
    case Some(HSReady) => Pin(BlockPortID(id.width, id.pt, PortMeaningWrapper(id.pmw.pm, Rdy), id.dummy), word)
  }
}

case class Pin(id: BlockPortID, loc: Int) {
  override def toString(): String = {
    id.toString() + "[" + loc + "]"
  }

  def flipped: Pin = {
    Pin(id.flipped(), loc)
  }

  def canonicalize: Pin = {
    Pin(id.canonicalize(), loc)
  }

  def plain: Pin = {
    val cPin = this.canonicalize

    val pPmw = PortMeaningWrapper(PMData(None), cPin.id.pmw.pb)
    val pId = BlockPortID(cPin.id.width, cPin.id.pt, pPmw, cPin.id.dummy)
    Pin(pId, cPin.loc)
  }

  def concreteWidth: Int = id.concreteWidth

  def asDummy: Pin = {
    val nId = BlockPortID(id.width, id.pt, id.pmw, Dummy)
    Pin(nId, loc)
  }
}

// LocMapping is non empty for architecture primitives
// Port with matchers go into different BlockPort object, each containing a subset of the original words
// For each local word, we have the mapping from local word to original location
// We need this to be consistent with original location in PatternPort
case class BlockPort(
    id: BlockPortID,
    words: Int,
    annotations: Set[Annotation]
) extends ArchPort {
  val width = id.width
  val pt = id.pt
  val pmw = id.pmw
  val pm = pmw.pm
  val dummy = id.dummy

  override def toString(): String = {
    val pmS = id.pmw.pm.str
    val isHs = this.pmw.pb
    val ptS = pt.str
    val dum = dummy.str
    // val loc = pm.locInfo
    val matcher = pm.matcher
    val annos = annotations.mkString(", ")

    RED + id.width + RESET +
      "(" + BLUE + ptS + ", " + pmS + ", " + isHs + ", " + dum + ", " + matcher + ", " + annos + RESET + ") * "
      + MAGENTA + words + RESET
  }

  def isEmpty = id == BlockPortID.empty
  def flipped = BlockPort(id.flipped(), words, annotations)

  def canonicalize(): BlockPort = {
    val nId = id.canonicalize()
    val nBp = BlockPort(nId, words, annotations)

    nBp
  }

  def toPins(): List[Pin] = (0 until words).map(Pin(id, _)).toList

  def withoutAnnotations: BlockPort = BlockPort(id, words, Set())

  def withAnnotation(annos: Annotation): BlockPort = {
    val nAnnos = annotations + annos
    BlockPort(id, words, nAnnos)
  }

  def getMatcher(): String = {
    pmw.pm.matcher.fold("")(
      m => m
    )
  }

  def isDataPort(): Boolean = {
    this.pmw.pb.isImpl() || this.pmw.pb.isData()
  }

  def isHandshake(): Boolean = {
    this.pmw.pb.isImpl() || this.pmw.pb.isHandshake()
  }

  // TODO this seems weird...
  def widerThan(o: BlockPort): Boolean = {
    this.words >= o.words
  }

  def nonEmptyUnder(arch: Arch): Boolean = {
    arch.contains(this.id)
  }
}

object BlockInterface {
  def join(
      p0: Map[BlockPortID, BlockPort],
      p1: Map[BlockPortID, BlockPort]
  ): Map[BlockPortID, Either[BlockPort, (BlockPort, BlockPort)]] = {
    (p0.toSeq ++ p1.toSeq)
      .groupBy(_._1)
      .map(_._2.map(_._2))
      .map {
        (bps: Seq[BlockPort]) =>
          bps match {
            case bp0 :: bp1 :: Nil => (bp0.id, Right((bp0, bp1)))
            case bp0 :: Nil        => (bp0.id, Left(bp0))
          }
      }
      .toMap
  }
}

// TODO remove the clocked attribute....
case class BlockInterface(ports: Map[BlockPortID, BlockPort], pbName: String, clocked: Boolean) {
  def inPorts(): Map[BlockPortID, BlockPort] = {
    ports.filter(
      (k, v) => k.pt == PTInput
    )
  }

  def outPorts(): Map[BlockPortID, BlockPort] = {
    ports.filter(
      (k, v) => k.pt == PTOutput
    )
  }

  def nonEmptyUnder(arch: Arch): Boolean = {
    ports
      .map(_._2)
      .exists(
        bp => arch.contains(bp.id)
      )
  }

  def sameLocAs(o: BlockPort): Option[BlockPort] = {
    ports.get(o.id)
  }

  def withName(nBlockName: String): BlockInterface = {
    BlockInterface(ports, nBlockName, clocked)
  }

  override def toString(): String = {
    pbName + " interface:\n    "
      + ports.filter(_._1.pt == PTInput).mkString("\n    ") + "\n    "
      + ports.filter(_._1.pt == PTOutput).mkString("\n    ") + "\n"
  }
}

type TBlock = Block

case class Block(
    prim: Primitive,
    blockInterface: BlockInterface,
    physicalInfo: PhysicalInfo,
    annotations: Set[Annotation],
    namedAttrs: Map[NamedAttribute, String] // TODO remove this
) {
  val p = prim.p

  lazy val name: String = prim.name
  lazy val vprName: String = prim.vprNameString

  def getPins: Seq[Pin] = {
    blockInterface.ports
      .map(_._2)
      .map {
        bp =>
          {
            (0 until bp.words).map {
              w =>
                {
                  Pin(bp.id, w)
                }
            }
          }
      }
      .flatten
      .toSeq
  }

  def canIdentity(): Boolean = {
    annotations contains ACanIdentity
  }

  def canWire(): Boolean = {
    blockInterface.ports.map(_._2).exists(_.annotations.contains(AWirePort))
  }

  def allowPartial(): Boolean = {
    annotations contains AAllowPartialUsage
  }

  def nonEmptyUnder(arch: Arch): Boolean = {
    blockInterface.nonEmptyUnder(arch)
  }

  def canonicalize(): Block = {
    val bi = blockInterface
    val nPorts = bi.ports.toList
      .map {
        (bId, bp) =>
          {
            bp.canonicalize()
          }
      }
      .groupBy(_.id)
      .map {
        (bId, bps) =>
          {
            val bp = bps.head
            val words = bps.map(_.words).reduce(_ + _)
            (bId, BlockPort(bId, words, bp.annotations))
          }
      }

    val nBi = BlockInterface(nPorts, bi.pbName, bi.clocked)
    val nBlock = Block(prim, nBi, physicalInfo, annotations, namedAttrs)

    nBlock
  }

  def ports(): List[BlockPort] = {
    blockInterface.ports.values.toList
  }

  def withTimings(nPhys: PhysicalInfo): Block = {
    val i = blockInterface
    val nInterface = BlockInterface(i.ports, name, Timing.clocked(nPhys.timings))
    Block(prim, nInterface, nPhys, annotations, namedAttrs)
  }

  override def toString(): String = {
    GREEN + prim.name + RESET + ": " + blockInterface.toString() + "\n"
  }
}
