package archs

import arch._
import core._
import crkt.TNode
import crkt.Node
import crkt.Port
import util.Util
import readers.TimingReader
import frontend.GlobalParamsInst
import frontend.GlobalParams
import packerv2.RRGPrim

import math.log
import math.ceil
import archs.Mux
import util.Util.log2ceil
import mlir.MLIRType

// Make sure this is a stable identifier with the tailing op ?

enum ALUOperation:
  case add, sub, and, or, shl, xor, ashr, shrsi, shrui, lshr, ult, ule, slt, sle, eqop, neop, ugt, uge, sgt, sge, anyop

case object ComparatorOperation {
  def fromStr(str: String): ComparatorOperation = {
    str match {
      case CmpUlt.str => CmpUlt
      case CmpUle.str => CmpUle
      case CmpSlt.str => CmpSlt
      case CmpSle.str => CmpSle
      case CmpEq.str => CmpEq
      case CmpNe.str => CmpNe
      case CmpUgt.str => CmpUgt
      case CmpUge.str => CmpUge
      case CmpSgt.str => CmpSgt
      case CmpSge.str => CmpSge
      case CmpAnyComp.str => CmpAnyComp
    }
  }
}

sealed trait ComparatorOperation(val str: String)
case object CmpUlt extends ComparatorOperation("ult")
case object CmpUle extends ComparatorOperation("ule")
case object CmpSlt extends ComparatorOperation("slt")
case object CmpSle extends ComparatorOperation("sle")
case object CmpEq extends ComparatorOperation("eq")
case object CmpNe extends ComparatorOperation("ne")
case object CmpUgt extends ComparatorOperation("ugt")
case object CmpUge extends ComparatorOperation("uge")
case object CmpSgt extends ComparatorOperation("sgt")
case object CmpSge extends ComparatorOperation("sge")
case object CmpAnyComp extends ComparatorOperation("anycomp")
trait ParamForRTLConfig {
  def equivalentDynamaticName: String
  def paramType: String
  def ub: Option[Int]
  def lb: Option[Int]
  def eq: Option[Either[Int, String]]

  // lets you specify additional configurations with this parameter set to each of the provided values
  def fixedValues: Seq[String]

  assert(!ub.isDefined || !lb.isDefined || ub.get >= lb.get)
  assert(!ub.isDefined || !eq.isDefined || eq.get.fold(ub.get >= _, _ => true))
  assert(!eq.isDefined || !lb.isDefined || eq.get.fold(lb.get <= _, _ => true))
}

trait SingleParam {
  // for handshake.fork for instance W
  def name: String

  def isOptional: Boolean
  def defaultValue: Option[Either[Int, String]]

  // optional params must have a default
  assert(!(isOptional ^ defaultValue.isDefined))

  def toLibRep(value: Int): String = name + value
  def toLibRep(value: String): String = name + value
}

case class SingleDynamaticConfigurableParam (
  // for handshake.fork for instance W
  name: String,

  // for handshake.fork for instance DATA_WIDTH, some modules don't have a dynamatic equivalent
  // in that case can leave this blank
  equivalentDynamaticName: String,
  isOptional: Boolean,
  defaultValue: Option[Either[Int, String]] = None,
  paramType: String = "unsigned",
  ub: Option[Int] = None,
  lb: Option[Int] = None,
  eq: Option[Either[Int, String]] = None,

  // lets you specify additional configurations with this parameter set to each of the provided values
  fixedValues: Seq[String] = Seq() 
) extends SingleParam with ParamForRTLConfig

case class SingleNonDynamaticParam (
  name: String,
  isOptional: Boolean,
  defaultValue: Option[Either[Int, String]] = None
) extends SingleParam

trait ParamsConfig {
  def params: Seq[SingleParam] = Seq(SingleNonDynamaticParam("RECEXTPREF", true, Some(Right("false"))))

  def dynamaticConfigurableParams: Seq[SingleDynamaticConfigurableParam] = {
    params.collect{ case dp: SingleDynamaticConfigurableParam => dp }
  }

  private def assertStringsDisambiguable(a: String, b: String) = assert (
    !a.startsWith(b) && !b.startsWith(a),
    s"$a and $b cannot be disambiguated because one's a prefix of the other"
  )

  private def assertAllStringsDisambiguable(s: Seq[String]) = {
    s.combinations(2)
      .foreach {
        c => assertStringsDisambiguable(c(0), c(1))
      }
  }

  lazy val allParamNames = params.map(_.name)
  lazy val mandatoryParamNames = params.filter(!_.isOptional).map(_.name).toSet
  assertAllStringsDisambiguable(allParamNames)

  lazy val paramNamesToParams = params.map(p => (p.name, p)).toMap
  lazy val optionalParamMapping = {
    params.filter(_.isOptional).map{
      p => {
        (p.name, (p.defaultValue.get.fold(_.toString, _.toString), p))
      }
    }.toMap
  }

  def checkMandatoryParamsProvided(definedParamNames: Iterable[String]): Unit = {
    assert(
      mandatoryParamNames.subsetOf(definedParamNames.toSet),
      s"provided $definedParamNames don't include all mandatory ones: $mandatoryParamNames"
    )
  }

  def checkMappingValid(definedParamNames: Iterable[String]): Unit = {
    checkMandatoryParamsProvided(definedParamNames)
    assert(definedParamNames.toSet.subsetOf(allParamNames.toSet))
  }

  // takes a string and maps the internal name to the value in the string + parameter itself
  def unpackLibRepArguments(s: String): Map[String, (String, SingleParam)] = {
    val prms = s.split("_")

    for (nameValue <- prms) do
      if !allParamNames.exists(nameValue.startsWith(_)) then
        scala.sys.error(s"$nameValue doesn't start with a recognised parameter name")

    val mappedNotYetSplit = prms.map {
      nameValue =>
        (
          nameValue,
          paramNamesToParams.find(cand => nameValue.startsWith(cand._1)).get
        )
    }

    val mapWithoutDefaults = mappedNotYetSplit.map {
      case (nameValue, (paramName, param)) => {
        (paramName, (nameValue.takeRight(nameValue.length() - paramName.length()), param))
      }
    }.toMap

    optionalParamMapping ++ mapWithoutDefaults
  }

  def unpackLibRepArgumentsGetInt(s: String)(k: String): Int = unpackLibRepArguments(s)(k)._1.toInt
  def unpackLibRepArgumentsGetBoolean(s: String)(k: String): Boolean = unpackLibRepArguments(s)(k)._1.toBoolean
  def unpackLibRepArgumentsGetString(s: String)(k: String): String = unpackLibRepArguments(s)(k)._1

  // def paramsLibRep(mapping: Map[String, String]) = {
  //   checkMappingValid(mapping.keys)
  //   mapping.toSeq
  //     .sortWith {
  //       (a, b) => {
  //         val ak = Params.getKeyLoc(a._1)
  //         val bk = Params.getKeyLoc(b._1)

  //         ak < bk
  //       }
  //     }
  //     .map(
  //       (paramName, v) => s"$paramName$v"
  //     )
  // }

  // use alreadyDefinedParams to hardcode any parameter
  // basically paramsLibRep but passes string substitutions with dynamatic-usable naming
  def paramsLibRepDynamatic(alreadyDefinedParams: Map[String, String]): String = {
    // dynamatic-configurable mandatory parameters get string substituted
    // all others are not added, only via alreadyDefinedParams

    assert(
      alreadyDefinedParams.keySet.subsetOf(allParamNames.toSet),
      "don't try to define parameters not among the recognised ones"
    )

    val mandatoryConfigurables = params.collect {
      case SingleDynamaticConfigurableParam(internalName, equivalentDynamaticName, _, _, _, _, _, _, _) =>
        (internalName, s"$$${equivalentDynamaticName}")
    }.toMap

    val mergedMap = (mandatoryConfigurables ++ alreadyDefinedParams).map {
      (pName, pValue) => {
        if((pName == "PRED") && (pValue == "eqop")) {
          (pName, "eq")
        } else if((pName == "PRED") && (pValue == "neop")) {
          (pName, "neop")
        } else {
          (pName, pValue)
        }
      }
    }
    val paramSub = Params.makeStringRep(mergedMap, false)

    paramSub.replace("/", "").replace(":", "") // .mkString("_")
  }

  def libRepTemplateRTLConfig(primTypeString: String, alreadyDefinedParams: Map[String, String]) = {
    primTypeString + paramsLibRepDynamatic(alreadyDefinedParams)
  }
}

object Params {
  val defaultModuleNamePrefixEnabled: Boolean = false

  protected def mkPair(k: String, v: String): String = k + "/" + v
  protected def mkString(ps: Iterable[String]): String = ps.mkString("_")

  def getKeyLoc(s: String): Int = {
    s match  {
      case "W" => 0
      case "N" => 1
      case "T" => 2
      case "L" => 3
      case "D" => 4
      case "IW" => 5
      case "OW" => 6
      case "OP" => 7
      case "PRED" => 8
      case "RECEXTPREF" => 9
      case "INDW" => 10
      case "HASCMP" => 11
      case "V" => 12
    }
  }

  // Sort according to old naming scheme for now to not have to recompute everything
  def makeStringRep(kv: Map[String, String], modeling: Boolean): String = {
    mkString(kv.toSeq.sortWith {
      (a, b) => {
        val ak = getKeyLoc(a._1)
        val bk = getKeyLoc(b._1)

        ak < bk
      }
    }// .filter {
    //   // TODO remove this and rename the library by hand for now ?
    //   // TODO Library assumes anyop/anycomp but it should be easy to add...
    //   (s0, s1) => {
    //     !(s0 == "OP") && !(s0 == "PRED")
    //   }
    // }
      .map(mkPair.tupled(_)))
  }

  def isModuleNamePrefixEnabledInStrAttrs(attrs: Map[String, String]) = {
    (attrs.withDefaultValue(Params.defaultModuleNamePrefixEnabled.toString())("RECEXTPREF")).toBoolean
  }
}

trait Params {
  def recursivelyExtendPrefix: Boolean
  protected def config: ParamsConfig

  protected def mapping: Map[String, String] = Map("RECEXTPREF" -> recursivelyExtendPrefix.toString())

  private def a = mapping.partition(
    (k, v) => config.paramNamesToParams(k).isOptional
  )
  private def mappingOptionals = a._1
  private def mappingMandatories = a._2

  private def mappingNonDefaultOptionals = mappingOptionals.filter(
    (paramName, v) =>
      val param = config.paramNamesToParams(paramName)
      config.optionalParamMapping(paramName)._1 != v
  )
  private def mappingsRequiringSpecification = mappingMandatories ++ mappingNonDefaultOptionals

  // only adds prefixer param if relevant
  def stringRep: String = {
    if(recursivelyExtendPrefix) {
      Params.makeStringRep(mapping, true)
    } else {
      Params.makeStringRep(mappingsRequiringSpecification, true)
    }
  }
}

trait ParamsWithWidth extends Params {
  def width: Int
}

case object EmptyParams extends Params with ParamsConfig {
  protected def config: ParamsConfig = this
  override def recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
  override def params: Seq[SingleParam] = super.params
}

type TPrim = Primitive

trait PrimitiveStaticInfo[+P <: Params] {
  def typeString: String
  def getParams(attrs: Map[String, String]): P
  def clocked: Boolean // for clock tree synthesis, whether we need it or not
  def unpackLibRep(s: String): P // TODO remove except for CMux
  def isIo: Boolean // TODO Should be a trait
  def isBuf: Boolean // TODO Should be a trait
  def defaultConfigs: List[P]
  def params: ParamsConfig

  protected def getAttrsWithOptionalParams(attrs: Map[String, String]): Map[String, String] = {
    attrs ++ params.optionalParamMapping.map {
      case (paramName, (defaultParamValue, _)) => {
        (paramName, defaultParamValue)
      }
    }.toMap
  }
}

trait Primitive {
  type P <: Params

  def p: P
  def staticInfo: PrimitiveStaticInfo[P]

  def typeString = staticInfo.typeString
  def clocked = staticInfo.clocked
  def isIo = staticInfo.isIo
  def isBuf = staticInfo.isBuf
  def getParams(attrs: Map[String, String]): P = staticInfo.getParams(attrs)
  def defaultConfigs: List[P] = staticInfo.defaultConfigs
  def unpackLibRep(s: String): P = staticInfo.unpackLibRep(s)

  def instantiate(p: P): Block
  def canMap(n: TNode, p: P): Boolean
  def numConfBits(p: P): Int
  def slots(p: P): Int
  def annos: Set[Annotation]

  def name: String = staticInfo.typeString + ":" + p.stringRep
  def vprNameString: String = Util.validVprName(name)
  def libName: String = name.replace("/", "").replace(":", "")

  def getTimings(params: GlobalParamsInst): PhysicalInfo = {
    val libCompName = GlobalParams.hwLib + "/" + libName + ".cfg"
    TimingReader(libCompName)
  }

  def instantiate(params: GlobalParamsInst): Block = {
    val timings = getTimings(params)
    this.instantiate(p).withTimings(timings)
  }

  def log2(x: Double): Double = log(x) / log(2.0)
}

case class IdentityInfo(pn: RRGPrim, fromP: Port, toPs: List[Port]) {
  override def toString(): String = {
    pn.name + ": " + fromP.nodeID() + " -> " + "[" + toPs.map(_.nodeID()).mkString(", ") + "]"
  }
}

// TODO move this to its own file...
trait IdentityPrimitive extends Primitive {
  // TODO Should probably map to a list of Ports
  def getConstantSubCrkt(toPD: Port, toPHs: Port, idName: String): List[TNode] = {
    val outSrcId = BlockPortID(0, PTOutput, PortMeaningWrapper(PMData(None), Hs), Regular)

    val inCstId = BlockPortID(0, PTInput, PortMeaningWrapper(PMData(None), Hs), Regular)

    val outCstIdHs = BlockPortID(toPD.id.width, PTOutput, PortMeaningWrapper(PMData(None), Hs), Regular)
    val outCstIdD = BlockPortID(toPD.id.width, PTOutput, PortMeaningWrapper(PMData(None), D), Regular)

    val srcName = idName + "_src"
    val cstName = idName + "_cst"

    val outSrcPort = Port(outSrcId, "", Map(), srcName, Map(), 0)
    val inCstPort = Port(inCstId, "", Map(), cstName, Map(), 0)

    val outCstDPort = Port(outCstIdD, "", Map(), cstName, Map(), 0)
    val outCstHsPort = Port(outCstIdHs, "", Map(), cstName, Map(), 0)

    outSrcPort.distPorts = Map((inCstPort.nodeID() -> inCstPort))
    inCstPort.distPorts = Map((outSrcPort.nodeID() -> outSrcPort))

    outCstDPort.distPorts = Map((toPD.nodeID() -> toPD))
    toPD.distPorts = Map((outCstDPort.nodeID() -> outCstDPort))

    outCstHsPort.distPorts = Map((toPHs.nodeID() -> toPHs))
    toPHs.distPorts = Map((outCstHsPort.nodeID() -> outCstHsPort))

    val srcPorts = (outSrcPort :: Nil).groupBy(_.id)
    val src = Node(srcName, Source(SourceParams()), Map(), Map(), Set(), srcPorts)

    val cstPorts = (inCstPort :: outCstDPort :: outCstHsPort :: Nil).groupBy(_.id)
    val cstP = ConstantParams(toPD.id.width)
    val cst = Node(cstName, Constant(cstP), Map(), Map(), Set(), cstPorts)

    src :: cst :: Nil
  }

  def getIdentitySubCrktWithCond(
      idInfo: List[IdentityInfo],
      idName: String,
      prim: IdentityPrimitive
  ): List[TNode] = {
    val ports = idInfo.map {
      case IdentityInfo(_, fromP, toPs) => {
        assert(toPs.forall(_.id.pmw.pb == fromP.id.pmw.pb))
        val pb = fromP.id.pmw.pb
        val width = prim match {
          case Branch(p) => p.width
          case Mux(p)    => p.width
          case Select(p) => p.width
          case Fork(p)   => p.width
          case other     => ???
        }

        val inDid = BlockPortID(width, PTInput, PortMeaningWrapper(PMData(None), pb), Regular)
        val outDid = BlockPortID(width, PTOutput, PortMeaningWrapper(PMData(None), pb), Regular)

        val inP = Port(inDid, "", Map(), idName, Map((fromP.nodeID() -> fromP)), 0)

        val toPsIds = toPs.map(_.nodeID()).toSet
        val nFromPDPs = fromP.distPorts.filter(
          (_, dp) => !toPsIds.contains(dp.nodeID())
        ) + (inP.nodeID() -> inP)
        fromP.distPorts = nFromPDPs

        val dps = toPs.map(dp => (dp.nodeID() -> dp)).toMap
        val outP = Port(outDid, "", Map(), idName, dps, 0)

        toPs.foreach {
          p =>
            {
              p.distPorts = Map((outP.nodeID() -> outP))
            }
        }

        (inP :: outP :: Nil)
      }
    }.flatten

    val inCDId = BlockPortID(1, PTInput, PortMeaningWrapper(PMCond(None), D), Regular)
    val inCHsId = BlockPortID(1, PTInput, PortMeaningWrapper(PMCond(None), Hs), Regular)

    val inDCP = Port(inCDId, "", Map(), idName, Map(), 0)
    val inHsCP = Port(inCHsId, "", Map(), idName, Map(), 0)

    val cstSubCrkt = getConstantSubCrkt(inDCP, inHsCP, idName)

    val portsMap = (inDCP :: inHsCP :: ports).groupBy(_.id)

    // val nP = prim.toIdentity(prim.p)
    val n = Node(idName, prim, Map(), Map(), Set(), portsMap)

    n :: cstSubCrkt
  }

  def getIdentityWithoutCond(
      idInfo: List[IdentityInfo],
      idName: String,
      prim: IdentityPrimitive
  ): List[TNode] = {
    val ports = idInfo.map {
      case IdentityInfo(_, fromP, toPs) => {
        assert(toPs.forall(_.id.pmw.pb == fromP.id.pmw.pb))
        val pb = fromP.id.pmw.pb
        val width = prim match {
          case Branch(p) => p.width
          case Mux(p)    => p.width
          case Select(p) => p.width
          case Fork(p)   => p.width
          case Merge(p)  => p.width
          case other     => { println(other); ??? }
        }

        val inDid = BlockPortID(width, PTInput, PortMeaningWrapper(PMData(None), pb), Regular)
        val outDid = BlockPortID(width, PTOutput, PortMeaningWrapper(PMData(None), pb), Regular)

        val inP = Port(inDid, "", Map(), idName, Map((fromP.nodeID() -> fromP)), 0)

        val toPsIds = toPs.map(_.nodeID()).toSet
        val nFromPDPs = fromP.distPorts.filter(
          (_, dp) => !toPsIds.contains(dp.nodeID())
        ) + (inP.nodeID() -> inP)
        fromP.distPorts = nFromPDPs

        val dps = toPs
          .map(
            dp => (dp.nodeID() -> dp)
          )
          .toMap

        val outP = Port(outDid, "", Map(), idName, dps, 0)

        toPs.foreach {
          p =>
            {
              p.distPorts = Map((outP.nodeID() -> outP))
            }
        }

        (inP :: outP :: Nil)
      }
    }.flatten

    val portsMap = ports.groupBy(_.id)

    // val nP = ??? // prim.toIdentity(p) This
    val n = Node(idName, prim, Map(), Map(), Set(), portsMap)

    n :: Nil
  }

  def getIdentitySubCrkt(p: P, idInfo: List[IdentityInfo], idName: String): List[TNode]
  def isIdentity(): Boolean
  def toIdentity(p: P): P
  def requiredBundles(p: P): Set[(Int, PortBundle)]
}

val typeNameToStaticInfo = Map[String, PrimitiveStaticInfo[Params]](
  (BranchStaticInfo.typeString -> BranchStaticInfo),
  (ForkStaticInfo.typeString -> ForkStaticInfo),
  ("LazyFork" -> ForkStaticInfo),
  (MergeStaticInfo.typeString -> MergeStaticInfo),
  (MuxStaticInfo.typeString -> MuxStaticInfo),
  (MuxConfigStaticInfo.typeString -> MuxConfigStaticInfo),
  (SelectStaticInfo.typeString -> SelectStaticInfo),
  (CntrlMergeStaticInfo.typeString -> CntrlMergeStaticInfo),
  (EntryStaticInfo.typeString -> EntryStaticInfo),
  ("Entry" -> EntryStaticInfo), // Harcode to get different string output later
  (ExitStaticInfo.typeString -> ExitStaticInfo),
  ("Exit" -> ExitStaticInfo), // Harcode to get different string output later
  (SourceStaticInfo.typeString -> SourceStaticInfo),
  (ConstantStaticInfo.typeString -> ConstantStaticInfo),
  (SinkStaticInfo.typeString -> SinkStaticInfo),
  (EBStaticInfo.typeString -> EBStaticInfo),
  (TEHBStaticInfo.typeString -> TEHBStaticInfo),
  (OEHBStaticInfo.typeString -> OEHBStaticInfo),
  (ComparatorStaticInfo.typeString -> ComparatorStaticInfo),
  (MultStaticInfo.typeString -> MultStaticInfo),
  (ExtsiStaticInfo.typeString -> ExtsiStaticInfo),
  (ExtuiStaticInfo.typeString -> ExtuiStaticInfo),
  (TruncStaticInfo.typeString -> TruncStaticInfo),
  (OperatorStaticInfo.typeString -> OperatorStaticInfo),
  (JoinStaticInfo.typeString -> JoinStaticInfo)
)

object JoinParams extends ParamsConfig {
  override def params: Seq[SingleParam] = Seq (
    SingleDynamaticConfigurableParam("N", equivalentDynamaticName = "SIZE", false, fixedValues = Seq("1"), lb = Some(1))
  ) ++ super.params
}

case class JoinParams(num: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends Params {
  protected def config: ParamsConfig = JoinParams
  override protected def mapping: Map[String, String] = Map("N" -> num.toString()) ++ super.mapping
}

case object JoinStaticInfo extends PrimitiveStaticInfo[JoinParams] {
  def params: ParamsConfig = JoinParams

  val typeString = "Join"
  val clocked = false
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): JoinParams = {
    JoinParams(JoinParams.unpackLibRepArgumentsGetInt(s)("N"))
  }

  def getParams(attrs: Map[String, String]): JoinParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    JoinParams(attrs2("N").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  lazy val defaultConfigs: List[JoinParams] = {
    JoinParams(2) :: Nil
  }
}

case class Join(p: JoinParams) extends Primitive {
  type P = JoinParams
  val annos = Set()

  val staticInfo = JoinStaticInfo

  def slots(p: JoinParams): Int = 0
  def numConfBits(p: JoinParams) = 0

  def instantiate(p: JoinParams): Block = {
    val inDid = BlockPortID(0, PTInput, PortMeaningWrapper(PMData(None), Hs), Regular)
    val outDid = BlockPortID(0, PTOutput, PortMeaningWrapper(PMData(None), Hs), Regular)

    val ind = BlockPort(inDid, p.num, Set(AWirePort, AEquivalent))
    val outd = BlockPort(outDid, 1, Set(AWirePort))

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), Set(), Map())
  }

  def canMap(n: TNode, p: JoinParams): Boolean = {
    n.nType match {
      case Join(np) => np.num <= p.num
      case other    => false
    }
  }
}

object BranchParams extends ParamsConfig {
  override def params: Seq[SingleParam] = {
    Seq(SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow")) ++ super.params
  }
}

case class BranchParams(width: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends ParamsWithWidth {
  protected def config: ParamsConfig = BranchParams
  override protected def mapping: Map[String, String] = Map("W" -> width.toString()) ++ super.mapping
}

object BranchStaticInfo extends PrimitiveStaticInfo[BranchParams] {
  def params: ParamsConfig = BranchParams

  val typeString = "Branch"
  val clocked = false
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): BranchParams = {
    val unpacked = BranchParams.unpackLibRepArgumentsGetInt(s)
    BranchParams(unpacked("W"), BranchParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): BranchParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    BranchParams.checkMappingValid(attrs2.keys)

    BranchParams(attrs2("W").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  lazy val defaultConfigs: List[BranchParams] = {
    BranchParams(32) :: BranchParams(1) :: BranchParams(0) :: Nil
  }
}

case class Branch(p: BranchParams) extends IdentityPrimitive {
  type P = BranchParams
  val annos = Set(ACanIdentity)
  val staticInfo = BranchStaticInfo

  def slots(p: BranchParams): Int = 0
  def numConfBits(p: BranchParams): Int = 0

  def instantiate(p: BranchParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val inCid = BlockPortID(1, PTInput, PortMeaningWrapper(PMCond(None), Impl), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, 1, Set(AWirePort))
    val outd = BlockPort(outDid, 2, Set(AWirePort, AEquivalent))
    val inc = BlockPort(inCid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd),
        (inCid -> inc)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: BranchParams): Boolean = {
    n.nType match {
      case Branch(np) => np.width == p.width
      case other      => false
    }
  }

  // Can only be artificially created
  def isIdentity(): Boolean = false
  def toIdentity(p: BranchParams): BranchParams = p
  def requiredBundles(p: BranchParams): Set[(Int, PortBundle)] = {
    if (p.width == 0) {
      Set((0, Hs))
    } else {
      Set((p.width, Hs), (p.width, D))
    }
  }

  def getIdentitySubCrkt(p: BranchParams, idInfo: List[IdentityInfo], idName: String): List[TNode] = {
    val idPrim = Branch(toIdentity(p))
    getIdentitySubCrktWithCond(idInfo, idName, idPrim)
  }
}

object ForkVariants {
  def withName(str: String): ForkVariants = {
    str match {
      case "E" => EagerFork
      case "L" => LazyFork
      case "S" => SwitchableFork
      case other => scala.sys.error("Unexpected String: " + str)
    }
  }
}

sealed trait ForkVariants(val str: String)
case object EagerFork extends ForkVariants("E")
case object LazyFork extends ForkVariants("L")
case object SwitchableFork extends ForkVariants("S")


object ForkParams extends ParamsConfig {
  override def params: Seq[SingleParam] = {
    Seq (
      SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow"),
      SingleDynamaticConfigurableParam("N", equivalentDynamaticName = "SIZE", false),
      SingleNonDynamaticParam("V", false)
    ) ++ super.params
  }
}

case class ForkParams (
  width: Int,
  num: Int,
  variant: ForkVariants,
  recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
) extends ParamsWithWidth {
  protected def config: ParamsConfig = ForkParams
  override protected def mapping: Map[String, String] = {
    Map("N" -> num.toString(), "W" -> width.toString(), "V" -> variant.str) ++ super.mapping
  }
}

object ForkStaticInfo extends PrimitiveStaticInfo[ForkParams] {
  def params: ParamsConfig = ForkParams
  val typeString = "Fork"
  val clocked = true

  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): ForkParams = {
    val unpacked = ForkParams.unpackLibRepArgumentsGetInt(s)
    val unpackedBools = ForkParams.unpackLibRepArgumentsGetBoolean(s)

    ForkParams(
      unpacked("W"),
      unpacked("N"),
      ForkVariants.withName(ForkParams.unpackLibRepArguments(s)("V")(0)),
      unpackedBools("RECEXTPREF")
    )
  }

  def getParams(attrs: Map[String, String]): ForkParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    ForkParams.checkMappingValid(attrs2.keys)

    ForkParams(
      attrs2("W").toInt,
      attrs2("N").toInt,
      ForkVariants.withName(attrs2("V")),
      Params.isModuleNamePrefixEnabledInStrAttrs(attrs2)
    )
  }

  lazy val defaultConfigs: List[ForkParams] = {
    (1 to 10).map {
      num =>
        {
          (0 :: 1 :: 32 :: Nil).map {
            width => {
              ForkParams(width, num, LazyFork)
                :: ForkParams(width, num, EagerFork)
                :: ForkParams(width, num, SwitchableFork)
                :: Nil
            }
          }
        }
    }.flatten.flatten.toList
  }
}

case class Fork(p: ForkParams) extends IdentityPrimitive {
  type P = ForkParams
  val annos = Set(ACanIdentity)
  val staticInfo = ForkStaticInfo

  def slots(p: ForkParams): Int = 0
  def numConfBits(p: ForkParams): Int = 0

  def instantiate(p: ForkParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, 1, Set(AWirePort))
    val outAnnos = Set[Annotation](AWirePort, AEquivalent, AAllowPartialUsage)
    val outd = BlockPort(outDid, p.num, outAnnos)

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: ForkParams): Boolean = {
    n.nType match {
      case Fork(np) => {
        val rightVariant = p.variant match {
          case EagerFork => (p.variant == np.variant)
          case LazyFork => (p.variant == np.variant)
          case SwitchableFork => true
        }

        (np.width == p.width) && (np.num <= p.num) && rightVariant
      } 
      case other    => false
    }
  }

  def isIdentity(): Boolean = p.num == 1
  def toIdentity(p: ForkParams): ForkParams = ForkParams(p.width, 1, p.variant)

  def requiredBundles(p: ForkParams): Set[(Int, PortBundle)] = {
    if (p.width == 0) {
      Set((0, Hs))
    } else {
      Set((p.width, Hs), (p.width, D))
    }
  }

  def getIdentitySubCrkt(p: ForkParams, idInfo: List[IdentityInfo], idName: String): List[TNode] = {
    val idPrim = Fork(toIdentity(p))
    getIdentityWithoutCond(idInfo, idName, idPrim)
  }
}

object MergeParams extends ParamsConfig {
  override def params: Seq[SingleParam] = {
    Seq(
      SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow"),
      SingleDynamaticConfigurableParam("N", equivalentDynamaticName = "SIZE", false)
    ) ++ super.params
  }
}

case class MergeParams(width: Int, num: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends ParamsWithWidth {
  protected def config: ParamsConfig = MergeParams
  override protected def mapping: Map[String, String] = {
    Map("W" -> width.toString(), "N" -> num.toString()) ++ super.mapping
  }
}

object MergeStaticInfo extends PrimitiveStaticInfo[MergeParams] {
  def params: ParamsConfig = MergeParams
  val typeString = "Merge"
  val clocked = true
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): MergeParams = {
    val unpacked = MergeParams.unpackLibRepArgumentsGetInt(s)
    MergeParams(unpacked("W"), unpacked("N"), MergeParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): MergeParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    MergeParams.checkMappingValid(attrs2.keys)
    MergeParams(attrs2("W").toInt, attrs("N").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  lazy val defaultConfigs: List[MergeParams] = {
    MergeParams(32, 2) :: MergeParams(1, 2) :: MergeParams(0, 2) :: Nil
  }
}

case class Merge(p: MergeParams) extends IdentityPrimitive {
  type P = MergeParams
  val annos = Set(ACanIdentity)
  val staticInfo = MergeStaticInfo

  def slots(p: MergeParams): Int = 0
  def numConfBits(p: MergeParams): Int = 0

  def instantiate(p: MergeParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, p.num, Set(AEquivalent))
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface (
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), Set(ACanIdentity), Map())
  }

  def canMap(n: TNode, p: MergeParams): Boolean = {
    n.nType match {
      case Merge(np) => (np.width == p.width) && (np.num <= p.num)
      case other     => false
    }
  }

  def isIdentity(): Boolean = p.num == 1
  def toIdentity(p: MergeParams): MergeParams = MergeParams(p.width, 1)

  def requiredBundles(p: MergeParams): Set[(Int, PortBundle)] = {
    if (p.width == 0) {
      Set((0, Hs))
    } else {
      Set((p.width, Hs), (p.width, D))
    }
  }

  def getIdentitySubCrkt(p: MergeParams, idInfo: List[IdentityInfo], idName: String): List[TNode] = {
    val idPrim = Merge(toIdentity(p))
    getIdentityWithoutCond(idInfo, idName, idPrim)
  }
}


object MuxConfigParams extends ParamsConfig {
  override def params: Seq[SingleParam] =
    Seq(
      SingleNonDynamaticParam("W", false),
      SingleNonDynamaticParam("N", false),
      SingleNonDynamaticParam("T", false),
      SingleNonDynamaticParam("L", false)
    ) ++ super.params
}

// tileSideLength in um
// wireLength is an integer, expressed a number of tiles
case class MuxConfigParams(
    width: Int,
    num: Int,
    tileSideLength: Int,
    wireLength: Int,
    recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
) extends Params {
  protected def config: ParamsConfig = MuxConfigParams
  override protected def mapping: Map[String, String] = {
    Map(
      "W" -> width.toString(),
      "N" -> num.toString(),
      "T" -> tileSideLength.toString(),
      "L" -> wireLength.toString()
    ) ++ super.mapping
  }
}

object MuxConfigStaticInfo extends PrimitiveStaticInfo[MuxConfigParams] {
  def params: ParamsConfig = MuxConfigParams

  val typeString = "CMux"
  val clocked = false
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): MuxConfigParams = {
    val unpacked = MuxConfigParams.unpackLibRepArgumentsGetInt(s)
    MuxConfigParams(
      unpacked("W"),
      unpacked("N"),
      unpacked("T"),
      unpacked("L"),
      MuxConfigParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF")
    )
  }

  def getParams(attrs: Map[String, String]): MuxConfigParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    MuxConfigParams.checkMappingValid(attrs2.keys)
    MuxConfigParams(
      attrs2("W").toInt,
      attrs("N").toInt,
      attrs("T").toInt,
      attrs("L").toInt,
      Params.isModuleNamePrefixEnabledInStrAttrs(attrs2)
    )
  }

  lazy val defaultConfigs: List[MuxConfigParams] = {
    (1 :: 32 :: Nil).map {
      width =>
        {
          (2 until 45).map {
            num =>
              {
                val noLoad = MuxConfigParams(width, num, 0, 0)

                noLoad :: (1 :: 2 :: Nil).map {
                  length =>
                    {
                      (100 until 180 by 10).map {
                        tile =>
                          {
                            MuxConfigParams(width, num, tile, length)
                          }
                      }
                    }
                }.flatten
              }
          }.flatten
        }
    }.flatten
  }
}

case class MuxConfig(p: MuxConfigParams) extends Primitive {
  val annos = Set()
  type P = MuxConfigParams
  val staticInfo = MuxConfigStaticInfo

  def slots(p: MuxConfigParams): Int = 0
  def numConfBits(p: MuxConfigParams): Int = ceil(log2(p.num)).toInt

  def getBestTimings(p: MuxConfigParams, params: GlobalParamsInst): PhysicalInfo = {
    val candidates = (p.num until (p.num + 4))
      .map {
        nNum =>
          {
            (p.tileSideLength until (p.tileSideLength + 40) by 10).map {
              tileSide =>
                {
                  (p.wireLength until (p.wireLength + 1)).map {
                    wireLength =>
                      {
                        val nP = MuxConfigParams(p.width, nNum, tileSide, wireLength)
                        val timings = MuxConfig(nP).getTimings(params)
                        // val timings = getTimings(nP, params)
                        if ((timings.area == 0) && (p.num > 1)) {
                          None
                        } else {
                          Some(timings)
                        }
                      }
                  }
                }
            }.flatten
          }
      }
      .flatten
      .flatten
      .sortBy(_.area) // .map(pi => PhysicalInfo(10000, pi.timings, pi.attrs))

    assert(candidates.nonEmpty, "Did not find mux for:" + p)

    candidates.head
  }

  override def instantiate(params: GlobalParamsInst): Block = {
    val timings = getBestTimings(p, params)
    this.instantiate(p).withTimings(timings)
  }

  def instantiate(p: MuxConfigParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), D), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), D), Regular)

    val ind = BlockPort(inDid, p.num, Set(AEquivalent))
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), Set(), Map())
  }

  // Should never be in the circuit
  def canMap(n: TNode, p: MuxConfigParams): Boolean = {
    ???
  }
}

object MuxParams extends ParamsConfig {
  override def params: Seq[SingleParam] =
    Seq(
      SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow"),
      SingleDynamaticConfigurableParam("N", equivalentDynamaticName = "SIZE", false)
    ) ++ super.params
}

case class MuxParams(width: Int, num: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends ParamsWithWidth {
  protected def config: ParamsConfig = MuxParams
  override protected def mapping: Map[String, String] =
    Map("W" -> width.toString(), "N" -> num.toString()) ++ super.mapping
}

object MuxStaticInfo extends PrimitiveStaticInfo[MuxParams] {
  def params: ParamsConfig = MuxParams

  val typeString = "Mux"
  val clocked = true
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): MuxParams = {
    val unpacked = MuxParams.unpackLibRepArgumentsGetInt(s)
    MuxParams(unpacked("W"), unpacked("N"), MuxParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): MuxParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    MuxParams.checkMappingValid(attrs2.keys)
    MuxParams(attrs2("W").toInt, attrs("N").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  lazy val defaultConfigs: List[MuxParams] = {
    MuxParams(32, 2) :: MuxParams(1, 2) :: MuxParams(0, 2) :: Nil
  }
}

case class Mux(p: MuxParams) extends IdentityPrimitive {
  type P = MuxParams
  val annos = Set(ACanIdentity)
  val staticInfo = MuxStaticInfo

  def slots(p: MuxParams): Int = 0
  def numConfBits(p: MuxParams): Int = 0

  def instantiate(p: MuxParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val inCid = BlockPortID(1, PTInput, PortMeaningWrapper(PMCond(None), Impl), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, p.num, Set(AEquivalent))
    val inc = BlockPort(inCid, 1, Set())
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (inCid -> inc),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: MuxParams): Boolean = {
    n.nType match {
      case Mux(np) => (np.width == p.width) && (np.num <= p.num)
      case other   => false
    }
  }

  // Can only be created artificially
  def isIdentity(): Boolean = false
  def toIdentity(p: MuxParams): MuxParams = MuxParams(p.width, 1)

  def requiredBundles(p: MuxParams): Set[(Int, PortBundle)] = {
    if (p.width == 0) {
      Set((0, Hs))
    } else {
      Set((p.width, Hs), (p.width, D))
    }
  }

  def getIdentitySubCrkt(p: MuxParams, idInfo: List[IdentityInfo], idName: String): List[TNode] = {
    val idPrim = Mux(toIdentity(p))
    getIdentitySubCrktWithCond(idInfo, idName, idPrim)
  }
}

object SelectParams extends ParamsConfig {
  override def params: Seq[SingleParam] =
    Seq(
      SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow"),
      SingleDynamaticConfigurableParam("N", equivalentDynamaticName = "SIZE", false, fixedValues = Seq("2"))
    ) ++ super.params
}

case class SelectParams(width: Int, num: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends ParamsWithWidth {
  protected def config: ParamsConfig = SelectParams
  override protected def mapping: Map[String, String] = {
    Map("W" -> width.toString(), "N" -> num.toString()) ++ super.mapping
  }
}

object SelectStaticInfo extends PrimitiveStaticInfo[SelectParams] {
  def params: ParamsConfig = SelectParams

  val typeString = "Select"
  val clocked = true
  val isIo = false
  val isBuf = false

  lazy val defaultConfigs: List[SelectParams] = {
    SelectParams(32, 2) :: SelectParams(1, 2) :: SelectParams(0, 2) :: Nil
  }

  def unpackLibRep(s: String): SelectParams = {
    val unpacked = SelectParams.unpackLibRepArgumentsGetInt(s)
    SelectParams(unpacked("W"), unpacked("N"), SelectParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): SelectParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    SelectParams.checkMappingValid(attrs2.keys)
    SelectParams(attrs2("W").toInt, attrs("N").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }
}

case class Select(p: SelectParams) extends IdentityPrimitive {
  type P = SelectParams
  // val annos = Set(ACanIdentity)
  val annos = Set() // otherwise breaks the Ancillary architecture
  val staticInfo = SelectStaticInfo

  def slots(p: SelectParams): Int = 0
  def numConfBits(p: SelectParams): Int = 0

  def instantiate(p: SelectParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val inCid = BlockPortID(1, PTInput, PortMeaningWrapper(PMCond(None), Impl), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, p.num, Set(AEquivalent))
    val inc = BlockPort(inCid, 1, Set())
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (inCid -> inc),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: SelectParams): Boolean = {
    n.nType match {
      case Select(np) => (np.width == p.width) && (np.num <= p.num)
      case other      => false
    }
  }

  def isIdentity(): Boolean = p.num == 1
  def toIdentity(p: SelectParams): SelectParams = SelectParams(p.width, 1)

  def requiredBundles(p: SelectParams): Set[(Int, PortBundle)] = {
    if (p.width == 0) {
      Set((0, Hs))
    } else {
      Set((p.width, Hs), (p.width, D))
    }
  }

  def getIdentitySubCrkt(p: SelectParams, idInfo: List[IdentityInfo], idName: String): List[TNode] = {
    val idPrim = Select(toIdentity(p))
    getIdentitySubCrktWithCond(idInfo, idName, idPrim)
  }
}

object CntrlMergeParams extends ParamsConfig {
  override def params: Seq[SingleParam] =
    Seq(
      SingleDynamaticConfigurableParam("N", equivalentDynamaticName = "SIZE", false),
      SingleDynamaticConfigurableParam("INDW", "INDEX_TYPE", true, Some(Left(1)), paramType="dataflow")
    ) ++ super.params
}

case class CntrlMergeParams(
  num: Int,
  indexWidth: Int = 1,
  recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
) extends ParamsWithWidth {
  assert(indexWidth == log2ceil(num))

  protected def config: ParamsConfig = CntrlMergeParams
  val width = 0
  override protected def mapping: Map[String, String] = {
    Map("N" -> num.toString(), "INDW" -> indexWidth.toString()) ++ super.mapping
  }
}

object CntrlMergeStaticInfo extends PrimitiveStaticInfo[CntrlMergeParams] {
  def params: ParamsConfig = CntrlMergeParams
  val typeString = "CntrlMerge"
  val clocked = true
  val isIo = false
  val isBuf = false

  lazy val defaultConfigs: List[CntrlMergeParams] = {
    CntrlMergeParams(2) :: Nil
  }

  def unpackLibRep(s: String): CntrlMergeParams = {
    val unpacked = CntrlMergeParams.unpackLibRepArgumentsGetInt(s)
    CntrlMergeParams(unpacked("N"), unpacked("INDW"), CntrlMergeParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): CntrlMergeParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    CntrlMergeParams.checkMappingValid(attrs2.keys)
    CntrlMergeParams(attrs2("N").toInt, attrs2("INDW").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }
}

case class CntrlMerge(p: CntrlMergeParams) extends Primitive {
  val annos = Set()
  type P = CntrlMergeParams
  val staticInfo = CntrlMergeStaticInfo

  def slots(p: CntrlMergeParams): Int = 0
  def numConfBits(p: CntrlMergeParams): Int = 0

  def instantiate(p: CntrlMergeParams): Block = {
    val inDid = BlockPortID(0, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(0, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outCid = BlockPortID(Util.log2ceil(p.num).toInt, PTOutput, PortMeaningWrapper(PMCond(None), Impl), Regular)

    val ind = BlockPort(inDid, p.num, Set(AEquivalent))
    val outd = BlockPort(outDid, 1, Set())
    val outc = BlockPort(outCid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd),
        (outCid -> outc)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), Set(), Map())
  }

  def canMap(n: TNode, p: CntrlMergeParams): Boolean = {
    n.nType match {
      case CntrlMerge(np) => (np.num <= p.num)
      case other          => false
    }
  }
}

object EntryParams extends ParamsConfig {
  override def params: Seq[SingleParam] = Seq(SingleNonDynamaticParam("W", false)) ++ super.params
}

case class EntryParams (
  width: Int,
  pbs: Set[PortBundle],
  mlirOrigType: Option[MLIRType] = None,
  recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
) extends ParamsWithWidth {
  protected def config: ParamsConfig = EntryParams
  override protected def mapping: Map[String, String] = Map("W" -> width.toString()) ++ super.mapping
}

object EntryStaticInfo extends PrimitiveStaticInfo[EntryParams] {
  def params: ParamsConfig = EntryParams
  val typeString = "Input"
  val clocked = false // TODO Do not have a Chisel io implementation yet
  val isIo = true
  val isBuf = false

  def unpackLibRep(s: String): EntryParams = {
    val unpacked = EntryParams.unpackLibRepArgumentsGetInt(s)
    EntryParams(unpacked("W"), Set(Impl))
  }

  def getParams(attrs: Map[String, String]): EntryParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    EntryParams.checkMappingValid(attrs2.keys)
    EntryParams(attrs2("W").toInt, Set(Impl))
  }

  // We do not have a Chisel model of IO ports
  lazy val defaultConfigs: List[EntryParams] = {
    Nil
  }
}

case class Entry(p: EntryParams) extends Primitive {
  type P = EntryParams
  val annos = Set(AIo, AImpl, ACanIdentity)
  val staticInfo = EntryStaticInfo

  def slots(p: EntryParams): Int = 0
  def numConfBits(p: EntryParams): Int = 0

  def instantiate(p: EntryParams): Block = {
    val ports = p.pbs.map {
      pb =>
        {
          val inId = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), pb), Regular)
          val in = BlockPort(inId, 1, Set())

          (inId, in)
        }
    }.toMap

    val bi = BlockInterface(ports, name, true)

    Block(this, bi, PhysicalInfo.empty(), annos, Map((NABlifModel -> ".input")))
  }

  def canMap(n: TNode, p: EntryParams): Boolean = {
    n.nType match {
      case Entry(np) => (np.width == p.width) && (np.pbs.forall(p.pbs.contains(_)))
      case other     => false
    }
    // lazy val rightWidth = n.ports.forall((id, ps) => id.width == p.width)

    // val acceptedPBs = p.pbs.map(_.contains).flatten
    // lazy val rightPb = n.ports.map(_._2).flatten.forall(port => acceptedPBs.contains(port.id.pmw.pb))

    // (n.nType == Entry) && rightWidth && rightPb
  }
}

object ExitParams extends ParamsConfig {
  override def params: Seq[SingleParam] = Seq(SingleNonDynamaticParam("W", false)) ++ super.params
}

case class ExitParams(
    width: Int,
    pbs: Set[PortBundle],
    recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
) extends ParamsWithWidth {
  protected def config: ParamsConfig = ExitParams
  override protected def mapping: Map[String, String] = Map("W" -> width.toString()) ++ super.mapping
}

object ExitStaticInfo extends PrimitiveStaticInfo[ExitParams] {
  def params: ParamsConfig = ExitParams
  val typeString = "Output"
  val clocked = false // TODO Do not have a Chisel io implementation yet
  val isIo = true
  val isBuf = false

  def unpackLibRep(s: String): ExitParams = {
    val unpacked = ExitParams.unpackLibRepArgumentsGetInt(s)
    ExitParams(unpacked("W"), Set(Impl))
  }

  def getParams(attrs: Map[String, String]): ExitParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    ExitParams.checkMappingValid(attrs2.keys)
    ExitParams(attrs2("W").toInt, Set(Impl))
  }

  val defaultConfigs: List[ExitParams] = Nil
}

case class Exit(p: ExitParams) extends Primitive {
  val annos = Set(AIo, AImpl, ACanIdentity)
  type P = ExitParams
  val staticInfo = ExitStaticInfo

  def slots(p: ExitParams): Int = 0
  def numConfBits(p: ExitParams): Int = 0

  def instantiate(p: ExitParams): Block = {
    val ports = p.pbs.map {
      pb =>
        {
          val outId = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), pb), Regular)
          val out = BlockPort(outId, 1, Set())

          (outId, out)
        }
    }.toMap

    val bi = BlockInterface(ports, name, true)

    Block(this, bi, PhysicalInfo.empty(), annos, Map((NABlifModel -> ".output")))
  }

  def canMap(n: TNode, p: ExitParams): Boolean = {
    n.nType match {
      case Exit(np) => (np.width == p.width) && (np.pbs.forall(p.pbs.contains(_)))
      case other    => false
    }
    // lazy val rightWidth = n.ports.forall((id, ps) => id.width == p.width)

    // val acceptedPBs = p.pbs.map(_.contains).flatten
    // lazy val rightPb = n.ports.map(_._2).flatten.forall(port => acceptedPBs.contains(port.id.pmw.pb))

    // (n.nType == Exit) && rightWidth && rightPb
  }
}

object ConstantParams extends ParamsConfig {
  override def params: Seq[SingleParam] = {
    Seq(SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow")) ++ super.params
  }
}

// TODO add constant value as a parameter ?
case class ConstantParams(width: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends Params() {
  protected def config: ParamsConfig = ConstantParams
  override protected def mapping: Map[String, String] = Map("W" -> width.toString()) ++ super.mapping
}

object ConstantStaticInfo extends PrimitiveStaticInfo[ConstantParams] {
  def params: ParamsConfig = ConstantParams
  val typeString = "Constant"
  val clocked = false
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): ConstantParams = {
    val unpacked = ConstantParams.unpackLibRepArgumentsGetInt(s)
    ConstantParams(unpacked("W"), ConstantParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): ConstantParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    ConstantParams.checkMappingValid(attrs2.keys)
    ConstantParams(attrs2("W").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  lazy val defaultConfigs: List[ConstantParams] = {
    ConstantParams(1) :: ConstantParams(32) :: Nil
  }
}

case class Constant(p: ConstantParams) extends Primitive {
  val annos = Set()
  type P = ConstantParams
  val staticInfo = ConstantStaticInfo

  def slots(p: ConstantParams): Int = 0
  def numConfBits(p: ConstantParams): Int = p.width

  def instantiate(p: ConstantParams): Block = {
    val inCid = BlockPortID(0, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val inc = BlockPort(inCid, 1, Set())
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inCid -> inc),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: ConstantParams): Boolean = {
    n.nType match {
      case Constant(np) => np.width == p.width
      case other        => false
    }
  }
}

case class SrcConstant(p: ConstantParams) extends Primitive {
  val annos = Set()
  type P = ConstantParams
  val staticInfo = ConstantStaticInfo

  def slots(p: ConstantParams): Int = 0
  def numConfBits(p: ConstantParams): Int = p.width

  def instantiate(p: ConstantParams): Block = {
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: ConstantParams): Boolean = {
    n.nType match {
      case SrcConstant(np) => np.width == p.width
      case other           => false
    }
  }
}

object SinkParams extends ParamsConfig {
  override def params: Seq[SingleParam] = {
    Seq(SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow")) ++ super.params
  }
    
}

case class SinkParams(width: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends Params() {
  protected def config: ParamsConfig = SinkParams
  override protected def mapping: Map[String, String] = Map("W" -> width.toString()) ++ super.mapping
}

object SinkStaticInfo extends PrimitiveStaticInfo[SinkParams] {
  def params: ParamsConfig = SinkParams
  val typeString = "Sink"
  val clocked = false
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): SinkParams = {
    val unpacked = SinkParams.unpackLibRepArgumentsGetInt(s)
    SinkParams(unpacked("W"), SinkParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): SinkParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    SinkParams.checkMappingValid(attrs2.keys)
    SinkParams(attrs2("W").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  lazy val defaultConfigs: List[SinkParams] = {
    SinkParams(32) :: SinkParams(1) :: SinkParams(0) :: Nil
  }
}

case class Sink(p: SinkParams) extends Primitive {
  val annos = Set()
  type P = SinkParams
  val staticInfo = SinkStaticInfo

  def slots(p: SinkParams): Int = 0
  def numConfBits(p: SinkParams): Int = 0

  def instantiate(p: SinkParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val ind = BlockPort(inDid, 1, Set(AWirePort))

    val bi = BlockInterface(
      Map(
        (inDid -> ind)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: SinkParams): Boolean = {
    n.nType match {
      case Sink(np) => np.width == p.width
      case other    => false
    }
  }
}

object SourceParams extends ParamsConfig {
  override def params: Seq[SingleParam] = super.params
}

case class SourceParams(recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled) extends Params {
  protected def config: ParamsConfig = SourceParams
  override protected def mapping: Map[String, String] = Map() ++ super.mapping
}

object SourceStaticInfo extends PrimitiveStaticInfo[SourceParams] {
  def params: ParamsConfig = SourceParams
  val typeString = "Source"
  val clocked = false
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): SourceParams = {
    val unpacked = SourceParams.unpackLibRepArgumentsGetInt(s)
    SourceParams(SourceParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): SourceParams = SourceParams()

  lazy val defaultConfigs: List[SourceParams] = {
    Nil
  }
}

case class Source(p: SourceParams) extends Primitive {
  val annos = Set()
  type P = SourceParams
  val staticInfo = SourceStaticInfo

  def slots(p: SourceParams): Int = 0
  def numConfBits(p: SourceParams): Int = 0

  def instantiate(p: SourceParams): Block = {
    val outDid = BlockPortID(0, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: SourceParams): Boolean = {
    n.nType.isInstanceOf[Source]
  }
}

object EBParams extends ParamsConfig {
  override def params: Seq[SingleParam] =
    Seq(SingleNonDynamaticParam("W", false), SingleNonDynamaticParam("D", false)) ++ super.params
}

case class EBParams(width: Int, depth: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends Params() {
  protected def config: ParamsConfig = EBParams
  override protected def mapping: Map[String, String] =
    Map("W" -> width.toString(), "D" -> depth.toString()) ++ super.mapping
}

object EBStaticInfo extends PrimitiveStaticInfo[EBParams] {
  def params: ParamsConfig = EBParams
  val typeString = "EB"
  val clocked = true
  val isIo = false
  val isBuf = true

  def unpackLibRep(s: String): EBParams = {
    val unpacked = EBParams.unpackLibRepArgumentsGetInt(s)
    EBParams(unpacked("W"), unpacked("D"), EBParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): EBParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    EBParams.checkMappingValid(attrs2.keys)
    EBParams(attrs2("W").toInt, attrs("D").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  lazy val defaultConfigs: List[EBParams] = {
    EBParams(0, 1) :: EBParams(1, 1) :: EBParams(32, 1) :: Nil
  }
}

case class EB(p: EBParams) extends Primitive {
  val annos = Set()
  type P = EBParams
  val staticInfo = EBStaticInfo

  def slots(p: EBParams) = p.depth
  def numConfBits(p: EBParams): Int = 0

  def instantiate(p: EBParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, 1, Set())
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: EBParams): Boolean = {
    n.nType match {
      case EB(np) => (np.width == p.width) && (np.depth <= p.depth)
      case other  => false
    }
  }
}

object TEHBParams extends ParamsConfig {
  override def params: Seq[SingleParam] =
    Seq(
      SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow"),
      SingleDynamaticConfigurableParam("D", equivalentDynamaticName = "NUM_SLOTS", false, lb=Some(1))
    ) ++ super.params
}

case class TEHBParams(width: Int, depth: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends Params {
  protected def config: ParamsConfig = TEHBParams
  override protected def mapping: Map[String, String] =
    Map("W" -> width.toString(), "D" -> depth.toString()) ++ super.mapping
}

object TEHBStaticInfo extends PrimitiveStaticInfo[TEHBParams] {
  def params: ParamsConfig = TEHBParams
  val typeString = "TEHB"
  val clocked = true
  val isIo = false
  val isBuf = true

  def unpackLibRep(s: String): TEHBParams = {
    val unpacked = TEHBParams.unpackLibRepArgumentsGetInt(s)
    TEHBParams(unpacked("W"), unpacked("D"), TEHBParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): TEHBParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    TEHBParams.checkMappingValid(attrs2.keys)
    TEHBParams(attrs2("W").toInt, attrs("D").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  val defaultConfigs: List[TEHBParams] = {
    TEHBParams(0, 1) :: TEHBParams(1, 1) :: TEHBParams(32, 1) :: Nil
  }
}

case class TEHB(p: TEHBParams) extends Primitive {
  val annos = Set[Annotation]()
  type P = TEHBParams
  val staticInfo = TEHBStaticInfo

  def slots(p: TEHBParams): Int = { assert(p == this.p); p.depth }
  def numConfBits(p: TEHBParams): Int = { assert(p == this.p); 0 }

  def instantiate(p: TEHBParams): Block = {
    assert(p == this.p)

    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, 1, Set())
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(TEHB(p), bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: TEHBParams): Boolean = {
    n.nType match {
      case TEHB(np) => (np.width == p.width) && (np.depth <= p.depth)
      case other    => false
    }
  }
}

object OEHBParams extends ParamsConfig {
  override def params: Seq[SingleParam] =
    Seq(
      SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow"),
      SingleDynamaticConfigurableParam("D", equivalentDynamaticName = "NUM_SLOTS", false, lb=Some(1))
    ) ++ super.params
}

case class OEHBParams(width: Int, depth: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends ParamsWithWidth
    with Params {
  protected def config: ParamsConfig = OEHBParams
  override protected def mapping: Map[String, String] =
    Map("W" -> width.toString(), "D" -> depth.toString()) ++ super.mapping
}

object OEHBStaticInfo extends PrimitiveStaticInfo[OEHBParams] {
  def params: ParamsConfig = OEHBParams
  val typeString = "OEHB"
  val clocked = true
  val isBuf = true
  val isIo = false

  def unpackLibRep(s: String): OEHBParams = {
    val unpacked = OEHBParams.unpackLibRepArgumentsGetInt(s)
    OEHBParams(unpacked("W"), unpacked("D"), OEHBParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): OEHBParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    OEHBParams.checkMappingValid(attrs2.keys)
    OEHBParams(attrs2("W").toInt, attrs("D").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  lazy val defaultConfigs: List[OEHBParams] = {
    OEHBParams(0, 1) :: OEHBParams(1, 1) :: OEHBParams(32, 1) :: Nil
  }
}

// TODO Maybe this is the interface we want for all the components...
case class OEHB(p: OEHBParams) extends Primitive {
  val annos = Set[Annotation]()
  type P = OEHBParams
  val staticInfo = OEHBStaticInfo

  def slots(p: OEHBParams): Int = { assert(p == this.p); p.depth }
  def numConfBits(p: OEHBParams): Int = { assert(p == this.p); 0 }

  def instantiate(params: OEHBParams): Block = {
    assert(p == params)

    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, 1, Set())
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(OEHB(p), bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: OEHBParams): Boolean = {
    n.nType match {
      case OEHB(np) => (np.width == p.width) && (np.depth <= p.depth)
      case other    => false
    }
  }
}

object ComparatorParams extends ParamsConfig {
  override def params: Seq[SingleParam] = Seq(
    SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow"),
    SingleDynamaticConfigurableParam("PRED", "PREDICATE", true, Some(Right("")), "string")
  ) ++ super.params
}

case class ComparatorParams(
    width: Int,
    predicate: ComparatorOperation = CmpAnyComp,
    recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
) extends ParamsWithWidth
    with Params {
  protected def config: ParamsConfig = ComparatorParams
  override protected def mapping: Map[String, String] =
    Map("W" -> width.toString(), "PRED" -> predicate.str) ++ super.mapping
}

object ComparatorStaticInfo extends PrimitiveStaticInfo[ComparatorParams] {
  def params: ParamsConfig = ComparatorParams
  val typeString = "Comparator"
  val clocked = false
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): ComparatorParams = {
    val predString = ComparatorParams.unpackLibRepArgumentsGetString(s)("PRED")
    val predValue = ComparatorOperation.fromStr(predString)

    val unpacked = ComparatorParams.unpackLibRepArgumentsGetInt(s)
    ComparatorParams(
      unpacked("W"),
      predValue,
      ComparatorParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF")
    )
  }

  def getParams(attrs: Map[String, String]): ComparatorParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    ComparatorParams.checkMappingValid(attrs2.keys)
    ComparatorParams(
      attrs2("W").toInt,
      ComparatorOperation.fromStr(attrs("PRED")),
      Params.isModuleNamePrefixEnabledInStrAttrs(attrs2)
    )
  }

  lazy val defaultConfigs: List[ComparatorParams] = {
    ComparatorParams(32, CmpAnyComp) :: Nil
  }
}

case class Comparator(p: ComparatorParams) extends Primitive {
  val annos = Set()
  type P = ComparatorParams
  val staticInfo = ComparatorStaticInfo

  val op = p.predicate

  def slots(p: ComparatorParams): Int = 0
  def numConfBits(p: ComparatorParams): Int = 4

  def instantiate(p: ComparatorParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(1, PTOutput, PortMeaningWrapper(PMCond(None), Impl), Regular)

    val ind = BlockPort(inDid, 2, Set(AEquivalent))
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: ComparatorParams): Boolean = {
    n.nType match {
      case Comparator(np) => {
        (np.width == p.width) && ((p.predicate == CmpAnyComp) || (p.predicate == np.predicate))
      }
      case other          => false
    }
  }
}

object MultParams extends ParamsConfig {
  override def params: Seq[SingleParam] =
    Seq(SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow")) ++ super.params
}

case class MultParams(width: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends ParamsWithWidth
    with Params {
  protected def config: ParamsConfig = MultParams
  override protected def mapping: Map[String, String] = Map("W" -> width.toString()) ++ super.mapping
}

object MultStaticInfo extends PrimitiveStaticInfo[MultParams] {
  def params: ParamsConfig = MultParams
  val typeString = "Mult"
  val clocked = false // TODO could depend on parameters, eg pipeline parameter
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): MultParams = {
    val unpacked = MultParams.unpackLibRepArgumentsGetInt(s)
    MultParams(unpacked("W"), MultParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): MultParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    MultParams.checkMappingValid(attrs2.keys)
    MultParams(attrs2("W").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  lazy val defaultConfigs: List[MultParams] = {
    MultParams(32) :: Nil
  }
}

case class Mult(p: MultParams) extends Primitive {
  val annos = Set()
  type P = MultParams
  val staticInfo = MultStaticInfo

  def slots(p: MultParams): Int = 0
  def numConfBits(p: MultParams): Int = 0

  def instantiate(p: MultParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, 2, Set(AEquivalent))
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: MultParams): Boolean = {
    n.nType match {
      case Mult(np) => (np.width == p.width)
      case other    => false
    }
  }
}

object DivParams extends ParamsConfig {
  override def params: Seq[SingleParam] =
    Seq(SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow")) ++ super.params
}

case class DivParams(width: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends ParamsWithWidth
    with Params {
  protected def config: ParamsConfig = DivParams
  override protected def mapping: Map[String, String] = Map("W" -> width.toString()) ++ super.mapping
}

object DivStaticInfo extends PrimitiveStaticInfo[DivParams] {
  def params: ParamsConfig = DivParams
  val typeString = "Div"
  val clocked = false // TODO could depend on parameters, eg pipeline parameter
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): DivParams = {
    val unpacked = DivParams.unpackLibRepArgumentsGetInt(s)
    DivParams(unpacked("W"), DivParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): DivParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    DivParams.checkMappingValid(attrs2.keys)
    DivParams(attrs2("W").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  lazy val defaultConfigs: List[DivParams] = {
    DivParams(32) :: Nil
  }
}

case class Div(p: DivParams) extends Primitive {
  val annos = Set()
  type P = DivParams
  val staticInfo = DivStaticInfo

  def slots(p: DivParams): Int = 0
  def numConfBits(p: DivParams): Int = 0

  def instantiate(p: DivParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, 2, Set(AEquivalent))
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: DivParams): Boolean = {
    n.nType match {
      case Div(np) => (np.width == p.width)
      case other    => {
        false
      }
    }
  }
}

object OperatorParams extends ParamsConfig {
  override def params: Seq[SingleParam] = Seq(
    SingleDynamaticConfigurableParam("W", "DATA_TYPE", false, paramType = "dataflow"),
    SingleNonDynamaticParam("OP", true, Some(Right(""))),
    SingleNonDynamaticParam("HASCMP", true, Some(Right("true")))
  ) ++ super.params
}

case class OperatorParams (
  width: Int,
  op: ALUOperation = ALUOperation.anyop,
  withCmp: Boolean = true,
  recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
) extends ParamsWithWidth
    with Params {
  protected def config: ParamsConfig = OperatorParams
  override protected def mapping: Map[String, String] =
    Map("W" -> width.toString(), "OP" -> op.toString(), "HASCMP" -> withCmp.toString()) ++ super.mapping
}

object OperatorStaticInfo extends PrimitiveStaticInfo[OperatorParams] {
  def params: ParamsConfig = OperatorParams
  val typeString = "Operator"
  val clocked = false
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): OperatorParams = {
    val unpacked = OperatorParams.unpackLibRepArgumentsGetInt(s)
    OperatorParams(
      unpacked("W"),
      ALUOperation.valueOf(OperatorParams.unpackLibRepArgumentsGetString(s)("OP")),
      OperatorParams.unpackLibRepArgumentsGetBoolean(s)("HASCMP"),
      OperatorParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF")
    )
  }

  def getParams(attrs: Map[String, String]): OperatorParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    OperatorParams.checkMappingValid(attrs2.keys)
    OperatorParams(
      attrs2("W").toInt,
      ALUOperation.valueOf(attrs2("OP")),
      attrs2("HASCMP").toBoolean,
      Params.isModuleNamePrefixEnabledInStrAttrs(attrs2)
    )
  }

  lazy val defaultConfigs: List[OperatorParams] = {
    OperatorParams(32, ALUOperation.anyop) :: Nil
  }
}

case class Operator(p: OperatorParams) extends Primitive {
  val annos = Set()
  type P = OperatorParams
  val staticInfo = OperatorStaticInfo

  def slots(p: OperatorParams): Int = 0
  def numConfBits(p: OperatorParams): Int = 5

  def instantiate(p: OperatorParams): Block = {
    val inDid = BlockPortID(p.width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(p.width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, 2, Set(AEquivalent))
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: OperatorParams): Boolean = {
    n.nType match {
      case Operator(np) => {
        (np.width == p.width) && ((p.op == ALUOperation.anyop) || (p.op == np.op))
      }

      case other => false
    }
  }
}

object ExtsiParams extends ParamsConfig {
  override def params: Seq[SingleParam] = Seq(
    SingleDynamaticConfigurableParam("IW", equivalentDynamaticName = "INPUT_WIDTH", false),
    SingleDynamaticConfigurableParam("OW", equivalentDynamaticName = "OUTPUT_WIDTH", false)
  ) ++ super.params
}

case class ExtsiParams(
    inWidth: Int,
    outWidth: Int,
    recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
) extends Params {
  protected def config: ParamsConfig = ExtsiParams
  override protected def mapping: Map[String, String] =
    Map("IW" -> inWidth.toString(), "OW" -> outWidth.toString()) ++ super.mapping
}

object ExtsiStaticInfo extends PrimitiveStaticInfo[ExtsiParams] {
  def params: ParamsConfig = ExtsiParams
  val typeString = "Extsi"
  val clocked = false
  val isIo = false
  val isBuf = false

  lazy val defaultConfigs: List[ExtsiParams] = Nil

  def unpackLibRep(s: String): ExtsiParams = {
    val unpacked = ExtsiParams.unpackLibRepArgumentsGetInt(s)
    ExtsiParams(unpacked("IW"), unpacked("OW"), ExtsiParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): ExtsiParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    ExtsiParams.checkMappingValid(attrs2.keys)
    ExtsiParams(attrs2("IW").toInt, attrs("OW").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }
}

case class Extsi(p: ExtsiParams) extends IdentityPrimitive {
  val annos = Set(ACanIdentity)
  type P = ExtsiParams
  val staticInfo = ExtsiStaticInfo

  def slots(p: ExtsiParams): Int = 0
  def numConfBits(p: ExtsiParams): Int = 0

  def instantiate(p: ExtsiParams): Block = {
    val inDid = BlockPortID(p.inWidth, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(p.outWidth, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, 1, Set())
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: ExtsiParams): Boolean = {
    n.nType match {
      case Extsi(np) => (np.inWidth == p.inWidth) && (np.outWidth == p.outWidth)
      case other     => false
    }
  }

  def isIdentity(): Boolean = p.inWidth == p.outWidth
  def toIdentity(p: ExtsiParams): ExtsiParams = p
  def requiredBundles(p: ExtsiParams): Set[(Int, PortBundle)] = ??? // Should never be called

  def getIdentitySubCrkt(p: ExtsiParams, idInfo: List[IdentityInfo], idName: String): List[TNode] = {
    val idPrim = Extsi(toIdentity(p))
    getIdentityWithoutCond(idInfo, idName, idPrim)
  }
}

object ExtuiParams extends ParamsConfig {
  override def params: Seq[SingleParam] = Seq(
    SingleDynamaticConfigurableParam("IW", equivalentDynamaticName = "INPUT_WIDTH", false),
    SingleDynamaticConfigurableParam("OW", equivalentDynamaticName = "OUTPUT_WIDTH", false)
  ) ++ super.params
}

case class ExtuiParams (
    inWidth: Int,
    outWidth: Int,
    recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
) extends Params {
  protected def config: ParamsConfig = ExtuiParams
  override protected def mapping: Map[String, String] =
    Map("IW" -> inWidth.toString(), "OW" -> outWidth.toString()) ++ super.mapping
}

object ExtuiStaticInfo extends PrimitiveStaticInfo[ExtuiParams] {
  def params: ParamsConfig = ExtuiParams
  val typeString = "Extui"
  val clocked = false
  val isIo = false
  val isBuf = false

  lazy val defaultConfigs: List[ExtuiParams] = Nil

  def unpackLibRep(s: String): ExtuiParams = {
    val unpacked = ExtuiParams.unpackLibRepArgumentsGetInt(s)
    ExtuiParams(unpacked("IW"), unpacked("OW"), ExtuiParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): ExtuiParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    ExtuiParams.checkMappingValid(attrs2.keys)
    ExtuiParams(attrs2("IW").toInt, attrs("OW").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }
}

case class Extui(p: ExtuiParams) extends IdentityPrimitive {
  val annos = Set(ACanIdentity)
  type P = ExtuiParams
  val staticInfo = ExtuiStaticInfo

  def slots(p: ExtuiParams): Int = 0
  def numConfBits(p: ExtuiParams): Int = 0

  def instantiate(p: ExtuiParams): Block = {
    val inDid = BlockPortID(p.inWidth, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(p.outWidth, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, 1, Set())
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: ExtuiParams): Boolean = {
    n.nType match {
      case Extui(np) => (np.inWidth == p.inWidth) && (np.outWidth == p.outWidth)
      case other     => false
    }
  }

  def isIdentity(): Boolean = p.inWidth == p.outWidth
  def toIdentity(p: ExtuiParams): ExtuiParams = p
  def requiredBundles(p: ExtuiParams): Set[(Int, PortBundle)] = ??? // Should never be called

  def getIdentitySubCrkt(p: ExtuiParams, idInfo: List[IdentityInfo], idName: String): List[TNode] = {
    val idPrim = Extui(toIdentity(p))
    getIdentityWithoutCond(idInfo, idName, idPrim)
  }
}

object TruncParams extends ParamsConfig {
  override def params: Seq[SingleParam] = Seq(
    SingleDynamaticConfigurableParam("IW", equivalentDynamaticName = "INPUT_WIDTH", false),
    SingleDynamaticConfigurableParam("OW", equivalentDynamaticName = "OUTPUT_WIDTH", false)
  ) ++ super.params
}

case class TruncParams (
    inWidth: Int,
    outWidth: Int,
    recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
) extends Params {
  protected def config: ParamsConfig = TruncParams
  override protected def mapping: Map[String, String] =
    Map("IW" -> inWidth.toString(), "OW" -> outWidth.toString()) ++ super.mapping
}

object TruncStaticInfo extends PrimitiveStaticInfo[TruncParams] {
  def params: ParamsConfig = TruncParams
  val typeString = "Trunc"
  val clocked = false
  val isIo = false
  val isBuf = false

  lazy val defaultConfigs: List[TruncParams] = Nil

  def unpackLibRep(s: String): TruncParams = {
    val unpacked = TruncParams.unpackLibRepArgumentsGetInt(s)
    TruncParams(unpacked("IW"), unpacked("OW"), TruncParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): TruncParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    TruncParams.checkMappingValid(attrs2.keys)
    TruncParams(attrs2("IW").toInt, attrs("OW").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }
}

case class Trunc(p: TruncParams) extends IdentityPrimitive {
  val annos = Set(ACanIdentity)
  type P = TruncParams
  val staticInfo = TruncStaticInfo

  def slots(p: TruncParams): Int = 0
  def numConfBits(p: TruncParams): Int = 0

  def instantiate(p: TruncParams): Block = {
    val inDid = BlockPortID(p.inWidth, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
    val outDid = BlockPortID(p.outWidth, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

    val ind = BlockPort(inDid, 1, Set())
    val outd = BlockPort(outDid, 1, Set())

    val bi = BlockInterface(
      Map(
        (inDid -> ind),
        (outDid -> outd)
      ),
      name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: TruncParams): Boolean = {
    n.nType match {
      case Trunc(np) => (np.inWidth == p.inWidth) && (np.outWidth == p.outWidth)
      case other     => false
    }
  }

  def isIdentity(): Boolean = p.inWidth == p.outWidth
  def toIdentity(p: TruncParams): TruncParams = p
  def requiredBundles(p: TruncParams): Set[(Int, PortBundle)] = ??? // Should never be called

  def getIdentitySubCrkt(p: TruncParams, idInfo: List[IdentityInfo], idName: String): List[TNode] = {
    val idPrim = Trunc(toIdentity(p))
    getIdentityWithoutCond(idInfo, idName, idPrim)
  }
}

object ConfParams extends ParamsConfig {
  override def params: Seq[SingleParam] = Seq(SingleNonDynamaticParam("N", false)) ++ super.params
}

case class ConfParams(num: Int, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends Params {
  protected def config: ParamsConfig = ConfParams
  override protected def mapping: Map[String, String] = Map("N" -> num.toString()) ++ super.mapping
}

object ConfigurationBitsStaticInfo extends PrimitiveStaticInfo[ConfParams] {
  def params: ParamsConfig = ConfParams
  val typeString = "ConfBits"
  val clocked = true
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): ConfParams = {
    val unpacked = ConfParams.unpackLibRepArgumentsGetInt(s)
    ConfParams(unpacked("N"), ConfParams.unpackLibRepArgumentsGetBoolean(s)("RECEXTPREF"))
  }

  def getParams(attrs: Map[String, String]): ConfParams = {
    val attrs2 = getAttrsWithOptionalParams(attrs)
    ConfParams.checkMappingValid(attrs2.keys)
    ConfParams(attrs2("N").toInt, Params.isModuleNamePrefixEnabledInStrAttrs(attrs2))
  }

  lazy val defaultConfigs: List[ConfParams] = {
    ConfParams(32) :: Nil
  }
}

case class ConfigurationBits(p: ConfParams) extends Primitive {
  val annos = Set()
  type P = ConfParams
  val staticInfo = ConfigurationBitsStaticInfo

  def slots(p: ConfParams): Int = 0
  def numConfBits(p: ConfParams): Int = p.num

  def instantiate(p: ConfParams): Block = {
    val emptyInterface = BlockInterface(Map(), "", true)
    Block(this, emptyInterface, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: ConfParams): Boolean = {
    scala.sys.error("Configuration bits cannot be instantiated in the circuit / arch for now.")
  }
}

object BlackBoxParams extends ParamsConfig {
  override def params: Seq[SingleParam] = super.params
}

// All ports are data ports. Pairs specify width and num each time.
case class BlackBoxParams (
  ins: List[(Int, Int)], // (width, num)
  outs: List[(Int, Int)], // (width, num)
  name: String,
  dontTouch: Boolean,
  recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled
) extends Params {
  protected def config: ParamsConfig = BlackBoxParams
  override def stringRep = name
}

object BlackBoxStaticInfo extends PrimitiveStaticInfo[BlackBoxParams] {
  def params: ParamsConfig = EmptyParams

  val typeString = "BlackBox"
  val clocked = false
  val isIo = false
  val isBuf = false

  def unpackLibRep(s: String): BlackBoxParams = {
    ???
  }

  def getParams(attrs: Map[String, String]): BlackBoxParams = {
    ???
  }

  lazy val defaultConfigs: List[BlackBoxParams] = {
    Nil
  }
}

case class BlackBox(p: BlackBoxParams) extends Primitive {
  val annos = Set()
  type P = BlackBoxParams
  val staticInfo = BlackBoxStaticInfo

  def slots(p: BlackBoxParams): Int = 0
  def numConfBits(p: BlackBoxParams): Int = 0

  def instantiate(p: BlackBoxParams): Block = {
    val inBps = p.ins.map {
      (width, num) =>
        {
          val inid = BlockPortID(width, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
          BlockPort(inid, num, Set())
        }
    }

    val outBps = p.outs.map {
      (width, num) =>
        {
          val outid = BlockPortID(width, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)
          BlockPort(outid, num, Set())
        }
    }

    val bi = BlockInterface(
      (inBps ++ outBps).groupBy(_.id).map((id, bps) => (id, BlockPort(id, bps.size, Set()))).toMap,
      p.name,
      true
    )

    Block(this, bi, PhysicalInfo.empty(), annos, Map())
  }

  def canMap(n: TNode, p: BlackBoxParams): Boolean = {
    scala.sys.error("Blackboxes cannot be in the architecture.")
  }
}

object DummyTypeParams extends ParamsConfig {
  override def params: Seq[SingleParam] = super.params
}

// TODO do something like the BlackBox here?
case class DummyTypeParams(sRep: String, recursivelyExtendPrefix: Boolean = Params.defaultModuleNamePrefixEnabled)
    extends Params {
  protected def config: ParamsConfig = DummyTypeParams
  override def stringRep = sRep
}

object DummyStaticInfo extends PrimitiveStaticInfo[DummyTypeParams] {
  def params: ParamsConfig = DummyTypeParams
  val isIo = false
  val isBuf = false
  val typeString = "Dummy"
  val clocked = false

  def unpackLibRep(s: String): DummyTypeParams = ???
  def getParams(attrs: Map[String, String]): DummyTypeParams =
    scala.sys.error("is an empty block")

  lazy val defaultConfigs: List[DummyTypeParams] = {
    Nil
  }
}

// The dummy block type is only here to give something to the block
// Not intended to be used by any pass for analysis
case class DummyType(p: DummyTypeParams) extends Primitive {
  val annos = Set()
  type P = DummyTypeParams
  val staticInfo = DummyStaticInfo

  def slots(p: DummyTypeParams): Int = 0

  def instantiate(p: DummyTypeParams): Block = {
    scala.sys.error("Cannot instantiate a dummy block with just the parameters.")
  }

  def numConfBits(p: DummyTypeParams): Int = 0

  def canMap(n: TNode, p: DummyTypeParams) = {
    n.nType.isInstanceOf[DummyType]
  }
}
