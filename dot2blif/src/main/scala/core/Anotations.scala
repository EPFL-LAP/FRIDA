package core

object Annotation {
  def fromStr(str: String): Annotation = str match {
    case ACanIdentity.str       => ACanIdentity
    case ADataOnly.str          => ADataOnly
    case AEquivalent.str        => AEquivalent
    case AMoleculeRoot.str      => AMoleculeRoot
    case ABypassable.str        => ABypassable
    case AAllowPartialUsage.str => AAllowPartialUsage
    case AWirePort.str          => AWirePort
    case AWireizeFirst.str      => AWireizeFirst
    case AWireizeLast.str       => AWireizeLast
    case AOptionalPort.str      => AOptionalPort
    case AImpl.str              => AImpl
    case AIo.str                => AIo
    case ATileIo.str            => ATileIo
    case ALogicalMode.str       => ALogicalMode
    case other                  => scala.sys.error("Unknown annotation")
  }
}

object NamedAttribute {
  def fromStr(str: String): NamedAttribute = str match {
    case NAPrimID.str    => NAPrimID
    case NABlifModel.str => NABlifModel
    case other           => scala.sys.error("Unknown Named Attribute")
  }
}

sealed trait Annotation(val str: String)

// Block annotations
case object ACanIdentity extends Annotation("canIdentity")
case object AIdentityCrkt extends Annotation("idCrkt")

case class AForkNumExits(val num: Int) extends Annotation("AForkNumExits")

// BlockPort annotations
case object ADataOnly extends Annotation("noHs")
case object AEquivalent extends Annotation("equ")
case object AAllowPartialUsage extends Annotation("partial")
case object AWirePort extends Annotation("wire")
case object AExternalPort extends Annotation("ext")
case object AOptionalPort extends Annotation("opt")

// Mode annotations
// case object AMolecule extends Annotation("Molecule")
case object AMoleculeRoot extends Annotation("MolRoot")
case object ABypassable extends Annotation("bypassable")
case object AWireizeFirst extends Annotation("WireizeFirst")
case object AWireizeLast extends Annotation("WireizeLast")
case object AImpl extends Annotation("Impl")
case object AIo extends Annotation("IO")
case object ANoColor extends Annotation("bland")
case object ALogicalMode extends Annotation("Logical")
case object ATileIo extends Annotation("TIO")
case object ASpanning extends Annotation("ASpan")

sealed trait NamedAttribute(val str: String)

// Mode named attributes
case object NAPrimID extends NamedAttribute("PrimID")
case object NABlifModel extends NamedAttribute("blif_model")


// ElasticGraph Node Annotations
case object ADontTouch extends Annotation("donttouch")
