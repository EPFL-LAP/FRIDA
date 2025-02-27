package arch

import math.max
import math.min
import math.floor
import io.AnsiColor._
import archs.VPRConfig

import core.Annotation
import core.NamedAttribute
import core.NAPrimID
import archs.Primitive

trait AbsTile // TODO is this required?

sealed trait PbElem {
  def name: String
  def modes: List[AbsMode]
}

case class Tile(
    name: String,
    modes: List[AbsMode],
    annotations: Set[Annotation],
    vprConfig: VPRConfig
) extends PbElem
    with AbsTile {
  override def toString(): String = {
    val tileName = name + ": \n"
    val modesStr = modes.map(_.toString("  "))

    tileName + "  " + modesStr.mkString("")
  }

  def asMode: Mode = Mode(name, modes, TileAnd, Map(), annotations, Map(), None)
}

object LocType {
  val defMatcher = "default"
}

sealed trait LocType
case class Local(matcher: String) extends LocType
case object Global extends LocType

case class LocatedBlockPortID(id: BlockPortID, loc: Int) {
  override def toString(): String = {
    "(" + id.detailedStringBland() + ", " + loc + ")"
  }
}

// TODO Rename to SymInfo or something similar
// str is a unique string identifying the symetry
// loc is the repetition
case class DagId(str: String, loc: Int) {
  def stringRep: String = str + "[" + loc + "]"
}

sealed trait AbsMode extends PbElem {
  def toString(indent: String): String

  def combinator: TileCombinator
  def annotations: Set[Annotation]
  def dagId: Option[DagId]
  def withDagId(dId: DagId, recMode: List[AbsMode]): AbsMode
}

// TODO Remove castedPorts
case class Mode(
    name: String,
    modes: List[AbsMode],
    combinator: TileCombinator,
    locConfig: Map[LocatedBlockPortID, LocType],
    annotations: Set[Annotation],
    castedPorts: Map[BlockPortID, BlockPortID],
    dagId: Option[DagId]
) extends AbsMode
    with AbsTile {
  def withDagId(dagId: DagId, recModes: List[AbsMode]): Mode = {
    Mode(name, recModes, combinator, locConfig, annotations, castedPorts, Some(dagId))
  }

  def toString(indent: String): String = {
    val rec = modes.map(_.toString(indent + "  ")).mkString(" " + modes.head.combinator.toString() + " ")
    name + ":\n" + indent + "  " + rec + "\n"
  }
}

case class PrimMode(
    name: String,
    combinator: TileCombinator,
    locConfig: Map[LocatedBlockPortID, LocType],
    annotations: Set[Annotation],
    castedPorts: Map[BlockPortID, BlockPortID],
    dagId: Option[DagId],
    prim: Primitive
) extends AbsMode {
  def modes: List[AbsMode] = Nil

  def withDagId(dagId: DagId, recModes: List[AbsMode]): PrimMode = {
    PrimMode(name, combinator, locConfig, annotations, castedPorts, Some(dagId), prim)
  }

  def toString(indent: String): String = {
    if (combinator == TileAndThen) {
      combinator.toString() + name
    } else {
      name
    }
  }

}

sealed trait TileCombinator
// sealed trait TileOr extends TileCombinator

case object TileOr extends TileCombinator {
  override def toString(): String = "|"
}

// TileOrOver
// case object TileOrOver extends TileOr {
//   override def toString(): String = "||"
// }

case object TileAndThen extends TileCombinator {
  override def toString(): String = "->"
}
case object TileAnd extends TileCombinator {
  override def toString(): String = "&"
}

sealed trait AndThenLocation
case object AndThenLeft extends AndThenLocation
case object AndThenRight extends AndThenLocation
case object AndThenCenter extends AndThenLocation
case object Undef extends AndThenLocation
