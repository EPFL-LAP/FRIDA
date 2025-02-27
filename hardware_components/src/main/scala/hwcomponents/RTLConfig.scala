package components

import printers.DynamaticTimingPrinter.compNameToDynCompName
import archs._
// import archs.Blocks$package.typeNameToParamsConfig
import archs.Blocks$package.typeNameToStaticInfo
import java.io.FileWriter
import util.Util
import frontend.GlobalParams

import scala.io.AnsiColor._

object RTLConfigParameterTypes extends Enumeration {
  type RTLConfigParameterType = String
  val Unsigned = "unsigned"
  val String = "string"
  val Timing = "timing"
  val Dataflow = "dataflow"
}

sealed abstract class JSONValue extends Product with Serializable {
  override def toString(): String = ???
}

final case class JSONString(value: String) extends JSONValue {
  override def toString(): String = s"\"$value\""
}

final case class JSONInteger(value: Int) extends JSONValue {
  override def toString(): String = value.toString()
}

final case class JSONBoolean(value: Boolean) extends JSONValue {
  override def toString(): String = value.toString()
}

final case class JSONList(value: Seq[JSONValue]) extends JSONValue {
  override def toString(): String = "[\n" + value.mkString(",\n") + "\n]"
}

object JSONList {
  def fromStrings(l: Seq[String]): JSONList = JSONList(l.map(JSONString(_)))
}

final case class JSONObject[K <: JSONValue, V <: JSONValue](m: Map[K, V])
    extends JSONValue {
  override def toString(): String = {
    val mappings = m.map { case (k, v) =>
      s"$k: $v"
    }
    "{\n" + mappings.mkString(",\n") + "\n}"
  }
}

object JSONObject {
  def fromStringJSONMap(
    m: Map[String, JSONValue]
  ): JSONObject[JSONString, JSONValue] = {
    JSONObject[JSONString, JSONValue](m.map { case (k, v) =>
      (JSONString(k), v)
    })
  }
  def fromStringMap(
      m: Map[String, String]
  ): JSONObject[JSONString, JSONString] = {
    JSONObject[JSONString, JSONString](m.map { case (k, v) =>
      (JSONString(k), JSONString(v))
    })
  }
}

case class RTLConfigParameter(
    name: String,
    paramType: RTLConfigParameterTypes.RTLConfigParameterType,
    generic: Option[Boolean] = None,
    ub: Option[Int] = None,
    lb: Option[Int] = None,
    eq: Option[Either[Int, String]] = None,
    readyLatEq: Option[Int] = None,
    validLatEq: Option[Int] = None,
    dataLatEq: Option[Int] = None
) {
  def toJSONObject() = {
    assert(
      eq.isEmpty
        || (eq.get.isRight && paramType == RTLConfigParameterTypes.String)
        || (eq.get.isLeft && paramType == RTLConfigParameterTypes.Unsigned)
        || (eq.get.isLeft && paramType == RTLConfigParameterTypes.Dataflow)
    ) // ensures eq is of right type

    val eqName = paramType match {
      case RTLConfigParameterTypes.Dataflow => "data-eq"
      case others => "eq"
    }

    val ubName = paramType match {
      case RTLConfigParameterTypes.Dataflow => "data-ub"
      case others => "ub"
    }

    val lbName = paramType match {
      case RTLConfigParameterTypes.Dataflow => "data-lb"
      case others => "lb"
    }

    val completeMap: Map[String, Option[JSONValue]] = Map(
      "name" -> Some(JSONString(name)),
      "type" -> Some(JSONString(paramType.toString())),
      "generic" -> generic.map(JSONBoolean(_)),
      ubName -> ub.map(JSONInteger(_)),
      lbName -> lb.map(JSONInteger(_)),
      eqName -> eq.map {
        case Left(v)  => JSONInteger(v)
        case Right(v) => JSONString(v)
      },
      "ready-lat-eq" -> readyLatEq.map(JSONInteger(_)),
      "valid-lat-eq" -> validLatEq.map(JSONInteger(_)),
      "data-lat-eq" -> dataLatEq.map(JSONInteger(_))
    )
    JSONObject(completeMap.collect { case (k, Some(v)) =>
      (JSONString(k), v)
    })
  }
}

object RTLConfigParameter {
  def apply(p: ParamForRTLConfig): RTLConfigParameter = {
    RTLConfigParameter(
      p.equivalentDynamaticName,
      p.paramType,
      ub = p.ub,
      lb = p.lb,
      eq = p.eq.map(e => e)
    )
  }
}

case class RTLConfigModule(
    name: String,
    parameters: Option[Seq[RTLConfigParameter]],
    generic: Option[String] = None,
    generator: Option[String] = None,
    otherAttrs: Map[String, JSONValue] = Map()
) {
  def toJSONObject() = {
    assert(
      generic.isDefined ^ generator.isDefined
    ) // exactly one of the two should be defined

    val mainMap: Map[String, Option[JSONValue]] = Map(
      "name" -> Some(JSONString(name)),
      "parameters" -> parameters.map(ps => JSONList(ps.map(_.toJSONObject()))),
      "generic" -> generic.map(JSONString(_)),
      "generator" -> generator.map(JSONString(_))
    )

    val mainMapFiltered = mainMap.collect { case (k, Some(v)) =>
      (JSONString(k), v)
    }

    val otherAttrsJSON = otherAttrs.map { case (k, v) =>
      (JSONString(k), v)
    }

    JSONObject(mainMapFiltered ++ otherAttrsJSON)
  }
}

object RTLConfigModule {
  def apply(mlirOpName: String, spec: RTLConfigModuleSpec): RTLConfigModule = {
    val m = spec.getOtherAttrs

    spec.parameters match {
      case Left(genericModuleParams) => {
        RTLConfigModule(
          mlirOpName,
          parameters =
            if (genericModuleParams.nonEmpty) Some(genericModuleParams)
            else None,
          generic = spec.generic,
          generator = spec.generator,
          otherAttrs = m
        )
      }

      case Right(generatedModuleParams) => {
        scala.sys.error(
          "generated modules may have multiple RTLConfigModules so not supported"
        )
      }
    }
  }
}

case class RTLConfigModuleSpec(
  hdl: String,
  parameters: Either[
    Seq[RTLConfigParameter],
    Seq[SingleDynamaticConfigurableParam]
  ] = Left(Seq()),
  generic: Option[String] = None,
  generator: Option[String] = None,
  useJSONConfig: Option[String] = None,
  ioKind: Option[String] = None,
  ioMap: Option[Map[String, String]] = None,
  ioSignals: Option[Map[String, String]] = None,
  dependencies: Option[Seq[String]] = None,
  archName: Option[String] = None,
  moduleName: Option[String] = None,

  // use me to indicate extra parameters to give to the chisel generator
  alreadyDefinedParams: Map[String, String] = Map(),

  // can provide multiple variants of fixed dynamatic-only parameters here
  // if it's empty it's ignored else the same module variants are varied with these combinations too
  dynamaticOnlyParamCombis: Seq[Seq[RTLConfigParameter]] = Seq() 
) {
  def getOtherAttrs = {
    Map[String, Option[JSONValue]](
      "hdl" -> Some(JSONString(hdl)),
      "use-json-config" -> useJSONConfig.map(JSONString(_)),
      "io-kind" -> ioKind.map(JSONString(_)),
      "io-map" -> ioMap.map(m =>
        JSONList(m.map { case (k, v) =>
          JSONObject(Map(JSONString(k) -> JSONString(v)))
        }.toSeq)
      ),
      "module-name" -> moduleName.map(JSONString(_)),
      "io-signals" -> ioSignals.map(JSONObject.fromStringMap(_)),
      "dependencies" -> dependencies.map(deps => JSONList.fromStrings(deps)),
      "arch-name" -> archName.map(JSONString(_))
    ).collect { case (k, Some(v)) =>
      (k, v)
    }
  }
}

case class RTLConfig(
    modules: Seq[RTLConfigModule],
    implicit val externalPrimitivesType: String
) {
  def toJSONList = {
    val regularModules: Seq[JSONValue] = modules.map(_.toJSONObject())
    JSONList(regularModules ++ RTLConfig.extraRTLConfigObjects)
  }

  def writeToFile = {
    val outFP = if(externalPrimitivesType == "vhdl") {
      GlobalParams.dynRTLConfigVHDL
    } else {
      GlobalParams.dynRTLConfigVerilog
    }

    println(YELLOW + "Printing: " + outFP + RESET)
      
    val writer: FileWriter = Util.writeOpen(
      outFP
    )

    writer.write(toJSONList.toString())
    writer.close()
  }
}

object RTLConfig {
  private def generatorCommand(
      typeString: String,
      params: String
  ): String = {
    s"cd \\\"$$ELASTIC_ROOT\\\" && java -jar \\\"./hardware_components/target/scala-2.13/hw-assembly-0.1.0-SNAPSHOT.jar\\\" genrtlcomponent \\\"$$OUTPUT_DIR/$$MODULE_NAME.v\\\" \\\"$typeString\\\" \\\"$params\\\""
  }

  def generateConfigurationPermutations[A, B](
      remainingConfigs: List[(A, List[B])],
      acc: List[(A, B)] = Nil
  ): List[List[(A, B)]] = {
    remainingConfigs match {
      case Nil => List(acc)
      case (paramName, options) :: xs =>
        options.flatMap(o =>
          generateConfigurationPermutations(xs, (paramName, o) :: acc)
        )
    }
  }

  private def generatedModulesParamVariants(
      params: Seq[SingleDynamaticConfigurableParam],
      primType: PrimitiveStaticInfo[Params],
      alreadyDefinedParams: Map[String, String] = Map()
  ): List[(String, List[RTLConfigParameter], String)] = {
    // param name and then list with param + each fixed value and then module name
    val paramOptions: List[
      (String, List[Either[SingleDynamaticConfigurableParam, String]])
    ] = params
      .map(p => {
        (
          p.name,
          List(Left(p)) ++ p.fixedValues.map(v => Right(v))
        )
      })
      .toList

    // all possible combinations of parameters
    val paramCombis = generateConfigurationPermutations(paramOptions.reverse)

    paramCombis.map(combi => {
      val fixedValues = combi.collect { case (paramName, Right(v)) =>
        (paramName, v)
      }

      val confValues = combi.collect { case (paramName, Left(v)) =>
        (paramName, v)
      }

      val confValuesRTLConfig = confValues.map { case (paramName, sp) =>
        RTLConfigParameter(sp)
      }

      val pconfig = primType.params
      val allParams = fixedValues.toMap ++ alreadyDefinedParams.updated("RECEXTPREF", "true")
      val librep = pconfig.paramsLibRepDynamatic(allParams)
      val moduleName = pconfig.libRepTemplateRTLConfig(primType.typeString, allParams)
      val genCommand = generatorCommand(primType.typeString, librep)

      (genCommand, confValuesRTLConfig, moduleName)
    })
  }

  private def genModuleVariants(
      mlirOpName: String,
      spec: RTLConfigModuleSpec,
      primStaticInfo: Option[PrimitiveStaticInfo[Params]] = None
  ) = {
    spec.parameters match {
      case Left(params) => {
        // non-chisel module
        Seq(RTLConfigModule(mlirOpName, spec))
      }

      case Right(params) => {
        // generated module
        assert(primStaticInfo.isDefined)

        // here can add in fixed dynamatic-only parameters like timing
        val variants = generatedModulesParamVariants(
          params,
          primStaticInfo.get,
          spec.alreadyDefinedParams
        )

        variants.flatMap {
          case (genCommand, paramCombi, moduleName) => {
            def moduleWithExtraParams(params: Seq[RTLConfigParameter]) = {
              val wholeParamCombi = paramCombi ++ params

              RTLConfigModule (
                mlirOpName,
                if (wholeParamCombi.nonEmpty) Some(wholeParamCombi) else None,
                generator = Some(genCommand),
                otherAttrs = spec.getOtherAttrs ++ Map(
                  "module-name" -> JSONString(moduleName)
                )
              )
            }

            if (spec.dynamaticOnlyParamCombis.isEmpty) {
              Seq(moduleWithExtraParams(Seq()))
            } else {
              spec.dynamaticOnlyParamCombis.map(moduleWithExtraParams(_))
            }
          }
        }
      }
    }
  }

  val operatorSpec = (
    OperatorStaticInfo,
    RTLConfigModuleSpec(
      "verilog",
      parameters = Right(archs.OperatorParams.dynamaticConfigurableParams),
      ioKind = Some("flat"),
      ioMap = Some(
        Map(
          "clk" -> "clock",
          "rst" -> "reset",
          "lhs" -> "a",
          "rhs" -> "b",
          "result" -> "res"
        )
      ),
      ioSignals = Some(Map("data" -> "_bits")),
      alreadyDefinedParams = Map("HASCMP" -> "false")
    )
  )

  def join_dep(implicit externalPrimitivesType: String) = {
    if (externalPrimitivesType == "vhdl") "join" else "join_type"
  }

  def mlirOpToImpls(
    implicit externalPrimitivesType: String
  ): Map[
    String,
    (
        Seq[(PrimitiveStaticInfo[Params], RTLConfigModuleSpec)],
        Seq[RTLConfigModuleSpec]
    )
  ] =
    Map(
      "handshake.cond_br" -> (
        Seq(
          (
            BranchStaticInfo,
            RTLConfigModuleSpec(
              "verilog",
              parameters = Right(archs.BranchParams.dynamaticConfigurableParams),
              ioKind = Some("flat"),
              ioMap = Some(
                Map(
                  "clk" -> "clock",
                  "rst" -> "reset",
                  "condition" -> "condIn",
                  "falseOut" -> "dOutFalse",
                  "trueOut" -> "dOutTrue",
                  "data" -> "dIn"
                )
              ),
              ioSignals = Some(Map("data" -> "_bits"))
            )
          )
        ),
        Seq(
          RTLConfigModuleSpec(
            externalPrimitivesType,
            parameters = Left(
              Seq(
                RTLConfigParameter(
                  "DATA_TYPE",
                  RTLConfigParameterTypes.Dataflow,
                  eq = Some(Left(0)),
                  generic = Some(false)
                )
              )
            ),
            generic = Some(
              s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/cond_br" + externalPrimitivesFExt
            ),
            moduleName = Some("cond_br_dataless"),
            dependencies = Some(Seq(join_dep))
          ),
          RTLConfigModuleSpec(
            externalPrimitivesType,
            parameters = Left(
              Seq(
                RTLConfigParameter(
                  "DATA_TYPE",
                  RTLConfigParameterTypes.Dataflow,
                  lb = Some(1)
                )
              )
            ),
            generic = Some(
              s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/cond_br" + externalPrimitivesFExt
            ),
            dependencies = Some(Seq("cond_br_dataless"))
          )
        )
      ),
      "handshake.control_merge" -> (
        Seq(
          (
            CntrlMergeStaticInfo,
            RTLConfigModuleSpec(
              "verilog",
              parameters =
                Right(archs.CntrlMergeParams.dynamaticConfigurableParams),
              ioKind = Some("flat"),
              ioMap = Some(
                Map(
                  "clk" -> "clock",
                  "rst" -> "reset",
                  "ins_*" -> "bbIn_*",
                  "outs" -> "bb",
                  "index" -> "from"
                )
              ),
              ioSignals = Some(Map("data" -> "_bits"))
            )
          )
        ),
        Seq(
          RTLConfigModuleSpec(
            externalPrimitivesType,
            parameters = Left(
              Seq(
                RTLConfigParameter(
                  "SIZE",
                  RTLConfigParameterTypes.Unsigned,
                  lb = Some(1)
                ),
                RTLConfigParameter(
                  "DATA_TYPE",
                  RTLConfigParameterTypes.Dataflow,
                  eq = Some(Left(0)),
                  generic = Some(false)
                ),
                RTLConfigParameter(
                  "INDEX_TYPE",
                  RTLConfigParameterTypes.Dataflow,
                  lb = Some(1)
                )
              )
            ),
            // TOOD this should be handled by the same control merge no ? Why have a specific dataless one?
            // TODO remove this RTLConfigModuleSpec
            generic = Some(
              s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/control_merge" + externalPrimitivesFExt
            ),
            moduleName = Some("control_merge_dataless"),
            dependencies =
              Some(Seq("merge_notehb_dataless", "tehb", "fork_dataless"))
          ),
          RTLConfigModuleSpec(
            externalPrimitivesType,
            parameters = Left(
              Seq(
                RTLConfigParameter(
                  "SIZE",
                  RTLConfigParameterTypes.Unsigned,
                  lb = Some(1)
                ),
                RTLConfigParameter(
                  "DATA_TYPE",
                  RTLConfigParameterTypes.Dataflow,
                  lb = Some(1)
                ),
                RTLConfigParameter(
                  "INDEX_TYPE",
                  RTLConfigParameterTypes.Dataflow,
                  lb = Some(1)
                )
              )
            ),
            generic = Some(
              s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/control_merge" + externalPrimitivesFExt
            ),
            dependencies = Some(Seq("control_merge_dataless") ++ {
              if (externalPrimitivesType == "vhdl") Seq("types") else Seq()
            })
          )
        )
      ),
      "handshake.cmpi" -> (
        Seq(
          (
            ComparatorStaticInfo,
            RTLConfigModuleSpec(
              "verilog",
              parameters =
                Right(archs.ComparatorParams.dynamaticConfigurableParams),
              ioKind = Some("flat"),
              ioMap = Some(
                Map(
                  "clk" -> "clock",
                  "rst" -> "reset",
                  "lhs" -> "a",
                  "rhs" -> "b",
                  "result" -> "condOut"
                )
              ),
              ioSignals = Some(Map("data" -> "_bits"))
            )
          )
        ),
        Seq(
          RTLConfigModuleSpec(
            externalPrimitivesType,
            parameters = Left(
              Seq(
                RTLConfigParameter(
                  "PREDICATE",
                  RTLConfigParameterTypes.String
                ),
                RTLConfigParameter(
                  "DATA_TYPE",
                  RTLConfigParameterTypes.Dataflow,
                  generic = Some(true)
                )
              )
            ),
            generator = Some(
              s"\\\"$$DYNAMATIC/bin/generators/rtl-cmpi-generator\\\" \\\"$$DYNAMATIC/data/$externalPrimitivesType/arith/cmpi$externalPrimitivesFExt\\\" \\\"$$OUTPUT_DIR/$$MODULE_NAME$externalPrimitivesFExt\\\" $$MODULE_NAME $$PREDICATE $externalPrimitivesType"
            ),
            dependencies = Some(Seq(join_dep))
          )
        )
      ),
      "handshake.buffer" -> (Seq(
        (
          TEHBStaticInfo,
          RTLConfigModuleSpec(
            "verilog",
            parameters = Right(archs.TEHBParams.dynamaticConfigurableParams),
            ioKind = Some("flat"),
            ioMap = Some(
              Map(
                "clk" -> "clock",
                "rst" -> "reset",
                "ins" -> "dIn",
                "outs" -> "dOut"
              )
            ),
            ioSignals = Some(Map("data" -> "_bits")),
            dynamaticOnlyParamCombis = Seq(
              Seq(
                RTLConfigParameter(
                  "TIMING",
                  RTLConfigParameterTypes.Timing,
                  generic = Some(false),
                  readyLatEq = Some(1)
                )
              )
            )
          )
        ),
        (
          OEHBStaticInfo,
          RTLConfigModuleSpec(
            "verilog",
            parameters = Right(archs.OEHBParams.dynamaticConfigurableParams),
            ioKind = Some("flat"),
            ioMap = Some(
              Map(
                "clk" -> "clock",
                "rst" -> "reset",
                "ins" -> "dIn",
                "outs" -> "dOut"
              )
            ),
            ioSignals = Some(Map("data" -> "_bits")),
            dynamaticOnlyParamCombis = Seq(
              Seq(
                RTLConfigParameter(
                  "TIMING",
                  RTLConfigParameterTypes.Timing,
                  generic = Some(false),
                  dataLatEq = Some(1),
                  validLatEq = Some(1)
                )
              )
            )
          )
        )
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "NUM_SLOTS",
                RTLConfigParameterTypes.Unsigned,
                eq = Some(Left(1)),
                generic = Some(false)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                eq = Some(Left(0)),
                generic = Some(false)
              ),
              RTLConfigParameter(
                "TIMING",
                RTLConfigParameterTypes.Timing,
                generic = Some(false),
                dataLatEq = Some(1),
                validLatEq = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/oehb" + externalPrimitivesFExt
          ),
          moduleName = Some("oehb_dataless")
        ),
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "NUM_SLOTS",
                RTLConfigParameterTypes.Unsigned,
                eq = Some(Left(1)),
                generic = Some(false)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                lb = Some(1)
              ),
              RTLConfigParameter(
                "TIMING",
                RTLConfigParameterTypes.Timing,
                generic = Some(false),
                dataLatEq = Some(1),
                validLatEq = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/oehb" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq("oehb_dataless"))
        ),
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "NUM_SLOTS",
                RTLConfigParameterTypes.Unsigned,
                lb = Some(2)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                eq = Some(Left(0)),
                generic = Some(false)
              ),
              RTLConfigParameter(
                "TIMING",
                RTLConfigParameterTypes.Timing,
                generic = Some(false),
                dataLatEq = Some(1),
                validLatEq = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/ofifo" + externalPrimitivesFExt
          ),
          moduleName = Some("ofifo_dataless"),
          dependencies =
            Some(Seq("tehb_dataless", "elastic_fifo_inner_dataless"))
        ),
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "NUM_SLOTS",
                RTLConfigParameterTypes.Unsigned,
                lb = Some(2)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                lb = Some(1)
              ),
              RTLConfigParameter(
                "TIMING",
                RTLConfigParameterTypes.Timing,
                generic = Some(false),
                dataLatEq = Some(1),
                validLatEq = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/ofifo" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq("tehb", "elastic_fifo_inner"))
        ),
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "NUM_SLOTS",
                RTLConfigParameterTypes.Unsigned,
                eq = Some(Left(1)),
                generic = Some(false)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                eq = Some(Left(0)),
                generic = Some(false)
              ),
              RTLConfigParameter(
                "TIMING",
                RTLConfigParameterTypes.Timing,
                generic = Some(false),
                readyLatEq = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/tehb" + externalPrimitivesFExt
          ),
          moduleName = Some("tehb_dataless")
        ),
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "NUM_SLOTS",
                RTLConfigParameterTypes.Unsigned,
                eq = Some(Left(1)),
                generic = Some(false)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                lb = Some(1)
              ),
              RTLConfigParameter(
                "TIMING",
                RTLConfigParameterTypes.Timing,
                generic = Some(false),
                readyLatEq = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/tehb" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq("tehb_dataless"))
        ),
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "NUM_SLOTS",
                RTLConfigParameterTypes.Unsigned,
                lb = Some(2)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                eq = Some(Left(0)),
                generic = Some(false)
              ),
              RTLConfigParameter(
                "TIMING",
                RTLConfigParameterTypes.Timing,
                generic = Some(false),
                readyLatEq = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/tfifo" + externalPrimitivesFExt
          ),
          moduleName = Some("tfifo_dataless"),
          dependencies = Some(Seq("elastic_fifo_inner_dataless"))
        ),
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "NUM_SLOTS",
                RTLConfigParameterTypes.Unsigned,
                lb = Some(2)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                lb = Some(1)
              ),
              RTLConfigParameter(
                "TIMING",
                RTLConfigParameterTypes.Timing,
                generic = Some(false),
                readyLatEq = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/tfifo" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq("elastic_fifo_inner"))
        )
      )),
      "handshake.mux" -> (
        Seq(
          (
            MuxStaticInfo,
            RTLConfigModuleSpec(
              "verilog",
              parameters = Right(archs.MuxParams.dynamaticConfigurableParams),
              ioKind = Some("flat"),
              ioMap = Some(
                Map(
                  "clk" -> "clock",
                  "rst" -> "reset",
                  "ins_*" -> "dIn_*",
                  "outs" -> "dOut",
                  "index" -> "condIn"
                )
              ),
              ioSignals = Some(Map("data" -> "_bits"))
            )
          )
        ),
        Seq(
          RTLConfigModuleSpec(
            externalPrimitivesType,
            parameters = Left(
              Seq(
                RTLConfigParameter(
                  "SIZE",
                  RTLConfigParameterTypes.Unsigned,
                  lb = Some(1)
                ),
                RTLConfigParameter(
                  "DATA_TYPE",
                  RTLConfigParameterTypes.Dataflow,
                  eq = Some(Left(0)),
                  generic = Some(false)
                ),
                RTLConfigParameter(
                  "SELECT_TYPE",
                  RTLConfigParameterTypes.Dataflow,
                  lb = Some(1)
                )
              )
            ),
            generic = Some(
              s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/mux" + externalPrimitivesFExt
            ),
            moduleName = Some("mux_dataless"),
            dependencies = Some(Seq("tehb_dataless"))
          ),
          RTLConfigModuleSpec(
            externalPrimitivesType,
            parameters = Left(
              Seq(
                RTLConfigParameter(
                  "SIZE",
                  RTLConfigParameterTypes.Unsigned,
                  lb = Some(1)
                ),
                RTLConfigParameter(
                  "DATA_TYPE",
                  RTLConfigParameterTypes.Dataflow,
                  lb = Some(1)
                ),
                RTLConfigParameter(
                  "SELECT_TYPE",
                  RTLConfigParameterTypes.Dataflow,
                  lb = Some(1)
                )
              )
            ),
            generic = Some(
              s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/mux" + externalPrimitivesFExt
            ),
            dependencies = Some(Seq("tehb") ++ {
              if (externalPrimitivesType == "vhdl") Seq("types") else Seq()
            })
          )
        )
      ),
      "handshake.fork" -> (Seq(
        (
          ForkStaticInfo,
          RTLConfigModuleSpec(
            "verilog",
            parameters = Right(archs.ForkParams.dynamaticConfigurableParams),
            ioKind = Some("flat"),
            ioMap = Some(
              Map(
                "clk" -> "clock",
                "rst" -> "reset",
                "ins" -> "dIn",
                "outs_*" -> "dOut_*"
              )
            ),
            ioSignals = Some(Map("data" -> "_bits")),
            alreadyDefinedParams = Map("V" -> "E")
          )
        )
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "SIZE",
                RTLConfigParameterTypes.Unsigned,
                lb = Some(1)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                eq = Some(Left(0)),
                generic = Some(false)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/fork" + externalPrimitivesFExt
          ),
          moduleName = Some("fork_dataless"),
          dependencies = Some(Seq("logic", "eager_fork_register_block"))
        ),
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "SIZE",
                RTLConfigParameterTypes.Unsigned,
                lb = Some(1)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                lb = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/fork" + externalPrimitivesFExt
          ),
          moduleName = Some(
            if (externalPrimitivesType == "vhdl") "handshake_fork"
            else "fork_type"
          ),
          dependencies = Some(Seq("fork_dataless") ++ {
            if (externalPrimitivesType == "vhdl") Seq("types") else Seq()
          })
        )
      )),
      "handshake.lazy_fork" -> (Seq(
        (
          ForkStaticInfo,
          RTLConfigModuleSpec(
            "verilog",
            parameters = Right(archs.ForkParams.dynamaticConfigurableParams),
            ioKind = Some("flat"),
            ioMap = Some(
              Map(
                "clk" -> "clock",
                "rst" -> "reset",
                "ins" -> "dIn",
                "outs_*" -> "dOut_*"
              )
            ),
            ioSignals = Some(Map("data" -> "_bits")),
            alreadyDefinedParams = Map("V" -> "L")
          )
        )
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "SIZE",
                RTLConfigParameterTypes.Unsigned,
                lb = Some(1)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                eq = Some(Left(0)),
                generic = Some(false)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/lazy_fork" + externalPrimitivesFExt
          ),
          moduleName = Some("lazy_fork_dataless"),
          dependencies = Some(Seq("logic"))
        ),
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "SIZE",
                RTLConfigParameterTypes.Unsigned,
                lb = Some(1)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                lb = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/lazy_fork" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq("lazy_fork_dataless") ++ {
            if (externalPrimitivesType == "vhdl") Seq("types") else Seq()
          })
        )
      )),
      "handshake.merge" -> (Seq(
        (
          MergeStaticInfo,
          RTLConfigModuleSpec(
            "verilog",
            parameters = Right(archs.MergeParams.dynamaticConfigurableParams),
            ioKind = Some("flat"),
            ioMap = Some(Map("clk" -> "clock", "rst" -> "reset", "ins_*" -> "dIn_*", "outs" -> "dOut")),
            ioSignals = Some(Map("data" -> "_bits"))
          )
        )
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "SIZE",
                RTLConfigParameterTypes.Unsigned,
                lb = Some(1)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                eq = Some(Left(0)),
                generic = Some(false)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/merge" + externalPrimitivesFExt
          ),
          moduleName = Some("merge_dataless"),
          dependencies = Some(Seq("tehb_dataless"))
        ),
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "SIZE",
                RTLConfigParameterTypes.Unsigned,
                lb = Some(1)
              ),
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                lb = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/merge" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq("tehb") ++ {
            if (externalPrimitivesType == "vhdl") Seq("types") else Seq()
          })
        )
      )),
      "handshake.select" -> (Seq(
        (
          SelectStaticInfo,
          RTLConfigModuleSpec(
            "verilog",
            parameters = Right(archs.SelectParams.dynamaticConfigurableParams),
            ioKind = Some("flat"),
            ioMap = Some(
              Map(
                "clk" -> "clock",
                "rst" -> "reset",
                "condition" -> "condIn",
                "falseValue" -> "dIn_0",
                "trueValue" -> "dIn_1",
                "result" -> "dOut"
              )
            ),
            ioSignals = Some(Map("data" -> "_bits"))
          )
        )
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/arith/select" + externalPrimitivesFExt
          ),
          moduleName = Some("selector")
        )
      )),
      "handshake.addi" -> (Seq(
        operatorSpec
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter("DATA_TYPE", RTLConfigParameterTypes.Dataflow)
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/arith/addi" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq(join_dep))
        )
      )),
      "handshake.andi" -> (Seq(
        operatorSpec
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter("DATA_TYPE", RTLConfigParameterTypes.Dataflow)
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/arith/andi" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq(join_dep))
        )
      )),
      "handshake.ori" -> (Seq(
        operatorSpec
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter("DATA_TYPE", RTLConfigParameterTypes.Dataflow)
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/arith/ori" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq(join_dep))
        )
      )),
      "handshake.shli" -> (Seq(
        operatorSpec
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter("DATA_TYPE", RTLConfigParameterTypes.Dataflow)
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/arith/shli" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq(join_dep))
        )
      )),
      "handshake.shrsi" -> (Seq(
        operatorSpec
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter("DATA_TYPE", RTLConfigParameterTypes.Dataflow)
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/arith/shrsi" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq(join_dep))
        )
      )),
      "handshake.shrui" -> (Seq(
        operatorSpec
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter("DATA_TYPE", RTLConfigParameterTypes.Dataflow)
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/arith/shrui" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq(join_dep))
        )
      )),
      "handshake.subi" -> (Seq(
        operatorSpec
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter("DATA_TYPE", RTLConfigParameterTypes.Dataflow)
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/arith/subi" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq(join_dep))
        )
      )),
      "handshake.xori" -> (Seq(
        operatorSpec
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter("DATA_TYPE", RTLConfigParameterTypes.Dataflow)
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/arith/xori" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq(join_dep))
        )
      )),
      "handshake.muli" -> (Seq(
        (
          MultStaticInfo,
          RTLConfigModuleSpec(
            "verilog",
            parameters = Right(archs.MultParams.dynamaticConfigurableParams),
            ioKind = Some("flat"),
            ioMap = Some(
              Map(
                "clk" -> "clock",
                "rst" -> "reset",
                "lhs" -> "a",
                "rhs" -> "b",
                "result" -> "res"
              )
            ),
            ioSignals = Some(Map("data" -> "_bits"))
          )
        )
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter("DATA_TYPE", RTLConfigParameterTypes.Dataflow)
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/arith/muli" + externalPrimitivesFExt
          ),
          dependencies = Some(Seq(join_dep, "delay_buffer", "oehb"))
        )
      )),
      "handshake.source" -> (Seq(
        (
          SourceStaticInfo,
          RTLConfigModuleSpec(
            "verilog",
            parameters = Right(archs.SourceParams.dynamaticConfigurableParams),
            ioKind = Some("flat"),
            ioMap =
              Some(Map("clk" -> "clock", "rst" -> "reset", "outs" -> "dOut"))
          )
        )
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(Seq()),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/source" + externalPrimitivesFExt
          )
        )
      )),
      "handshake.sink" -> (Seq(
        (
          SinkStaticInfo,
          RTLConfigModuleSpec(
            "verilog",
            parameters = Right(archs.SinkParams.dynamaticConfigurableParams),
            ioKind = Some("flat"),
            ioMap =
              Some(Map("clk" -> "clock", "rst" -> "reset", "ins" -> "dIn")),
            ioSignals = Some(Map("data" -> "_bits"))
          )
        )
      ), Seq(
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                eq = Some(Left(0)),
                generic = Some(false)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/sink" + externalPrimitivesFExt
          ),
          moduleName = Some("sink_dataless")
        ),
        RTLConfigModuleSpec(
          externalPrimitivesType,
          parameters = Left(
            Seq(
              RTLConfigParameter(
                "DATA_TYPE",
                RTLConfigParameterTypes.Dataflow,
                lb = Some(1)
              )
            )
          ),
          generic = Some(
            s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/sink" + externalPrimitivesFExt
          )
        )
      )),
      "handshake.join" ->
        (Seq(
          (
            JoinStaticInfo,
            RTLConfigModuleSpec(
              "verilog",
              parameters = Right(archs.JoinParams.dynamaticConfigurableParams),
              ioKind = Some("flat"),
              ioMap = Some(Map("clk" -> "clock", "rst" -> "reset"))
            )
          )
        ), Seq(
          RTLConfigModuleSpec(
            externalPrimitivesType,
            parameters = Left(
              Seq(
                RTLConfigParameter(
                  "SIZE",
                  RTLConfigParameterTypes.Unsigned,
                  lb = Some(1)
                )
              )
            ),
            generic = Some(
              s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/join" + externalPrimitivesFExt
            ),
            moduleName = Some(
              if (externalPrimitivesType == "vhdl") "join_handshake"
              else "join_type"
            ),
            dependencies = Some(
              Seq(if (externalPrimitivesType == "vhdl") "join" else "logic")
            )
          )
        ))
    )

  // Right(impls._2) = native vhdl/verilog that ships with Dynamatic
  // Left(impls._1) = Chisel component
  def implSelection[A, B](
      functionality: String,
      implementations: (A, B)
  ): Either[A, B] = {
    (functionality, implementations) match {
      case ("handshake.cond_br", impls)       => Left(impls._1)
      case ("handshake.control_merge", impls) => Left(impls._1)
      case ("handshake.cmpi", impls)          => Left(impls._1)
      case ("handshake.buffer", impls)        => Left(impls._1)
      case ("handshake.mux", impls)           => Left(impls._1)
      case ("handshake.fork", impls)          => Left(impls._1)
      case ("handshake.lazy_fork", impls)     => Left(impls._1)
      case ("handshake.merge", impls)         => Left(impls._1)
      case ("handshake.select", impls)        => Left(impls._1)
      case ("handshake.addi", impls)          => Left(impls._1)
      case ("handshake.andi", impls)          => Left(impls._1)
      case ("handshake.ori", impls)           => Left(impls._1)
      case ("handshake.shli", impls)          => Left(impls._1)
      case ("handshake.shrsi", impls)         => Left(impls._1)
      case ("handshake.shrui", impls)         => Left(impls._1)
      case ("handshake.subi", impls)          => Left(impls._1)
      case ("handshake.xori", impls)          => Left(impls._1)
      case ("handshake.muli", impls)          => Left(impls._1)
      case ("handshake.sink", impls)          => Left(impls._1)
      case ("handshake.source", impls)        => Left(impls._1)
      case ("handshake.join", impls)          => Left(impls._1)
    }
  }

  def externalPrimitivesFExt(implicit externalPrimitivesType: String) = {
    if (externalPrimitivesType == "vhdl") ".vhd" else ".v"
  }

  def mappedImpl(implicit externalPrimitivesType: String) = mlirOpToImpls.map {
    case (functionality, implementations) =>
      (functionality, implSelection(functionality, implementations))
  }

  def implementedInChisel (
    implicit externalPrimitivesType: String
  ): Seq[(String, (PrimitiveStaticInfo[Params], RTLConfigModuleSpec))] = {
    mappedImpl.collect { case (mlirOp, Left(impl)) =>
      (mlirOp, impl)
    }.toSeq.flatMap { case (mlirOp, impls) =>
      impls.map((mlirOp, _))
    }
  }

  def implementedInVHDL(implicit externalPrimitivesType: String) = mappedImpl
    .collect { case (mlirOp, Right(impls)) =>
      (mlirOp, impls)
    }
    .toSeq
    .flatMap { case (mlirOp, impls) =>
      impls.map((mlirOp, _))
    }

  def mlirOpToRTLConfigModuleSpec(implicit externalPrimitivesType: String) = implementedInChisel

  def externalRTLs(implicit externalPrimitivesType: String) = Seq(
    (
      "handshake.extsi",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "INPUT_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "OUTPUT_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/arith/extsi" + externalPrimitivesFExt
        )
      )
    ),
    (
      "handshake.extui",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "INPUT_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "OUTPUT_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/arith/extui" + externalPrimitivesFExt
        )
      )
    ),
    (
      "handshake.trunci",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "INPUT_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "OUTPUT_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/arith/trunci" + externalPrimitivesFExt
        )
      )
    ),
    (
      "handshake.br",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              eq = Some(Left(0)),
              generic = Some(false)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/br" + externalPrimitivesFExt
        ),
        moduleName = Some("br_dataless")
      )
    ),
    (
      "handshake.br",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1),
              generic = Some(false)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/br" + externalPrimitivesFExt
        ),
        dependencies = Some(Seq(join_dep))
      )
    ),
    (
      "handshake.mc_load",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "ADDR_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/mc_load" + externalPrimitivesFExt
        ),
        dependencies = Some(Seq("tehb"))
      )
    ),
    (
      "handshake.lsq_load",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "ADDR_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/lsq_load" + externalPrimitivesFExt
        )
      )
    ),
    (
      "handshake.mc_store",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "ADDR_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/mc_store" + externalPrimitivesFExt
        ),
        dependencies = Some(Seq(join_dep, "logic"))
      )
    ),
    (
      "handshake.lsq_store",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "ADDR_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/lsq_store" + externalPrimitivesFExt
        )
      )
    ),
    (
      "handshake.mem_controller",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "NUM_CONTROLS",
              RTLConfigParameterTypes.Unsigned,
              eq = Some(Left(0)),
              generic = Some(false)
            ),
            RTLConfigParameter(
              "NUM_LOADS",
              RTLConfigParameterTypes.Unsigned,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "NUM_STORES",
              RTLConfigParameterTypes.Unsigned,
              eq = Some(Left(0)),
              generic = Some(false)
            ),
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "ADDR_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/mem_controller_storeless" + externalPrimitivesFExt
        ),
        dependencies = Some(Seq("mc_support"))
      )
    ),
    (
      "handshake.mem_controller",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "NUM_CONTROLS",
              RTLConfigParameterTypes.Unsigned,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "NUM_LOADS",
              RTLConfigParameterTypes.Unsigned,
              eq = Some(Left(0)),
              generic = Some(false)
            ),
            RTLConfigParameter(
              "NUM_STORES",
              RTLConfigParameterTypes.Unsigned,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "ADDR_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/mem_controller_loadless" + externalPrimitivesFExt
        ),
        dependencies = Some(Seq("mc_support"))
      )
    ),
    (
      "handshake.mem_controller",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "NUM_CONTROLS",
              RTLConfigParameterTypes.Unsigned,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "NUM_LOADS",
              RTLConfigParameterTypes.Unsigned,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "NUM_STORES",
              RTLConfigParameterTypes.Unsigned,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "ADDR_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/mem_controller" + externalPrimitivesFExt
        ),
        dependencies = Some(Seq("mem_controller_loadless"))
      )
    ),
    (
      "handshake.lsq",
      RTLConfigModuleSpec(
        "verilog",
        parameters = Left(Seq()),
        useJSONConfig = Some("$OUTPUT_DIR/$MODULE_NAME.json"),
        generator = Some(
          s"java -jar -Xmx7G \\\"$$DYNAMATIC/bin/generators/lsq-generator.jar\\\" --target-dir \\\"$$OUTPUT_DIR\\\" --spec-file \\\"$$OUTPUT_DIR/$$MODULE_NAME.json\\\" > /dev/null"
        ),
        ioKind = Some("flat"),
        ioMap = Some(Map("clk" -> "clock", "rst" -> "reset", "*" -> "io_*")),
        ioSignals = Some(Map("data" -> "_bits"))
      )
    ),
    (
      "handshake.constant", // TODO we also have a constant, why do we need it here?
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter("VALUE", RTLConfigParameterTypes.String),
            RTLConfigParameter(
              "DATA_WIDTH",
              RTLConfigParameterTypes.Unsigned,
              generic = Some(true)
            )
          )
        ),
        generator = Some(
          if (externalPrimitivesType == "vhdl")
            s"\\\"$$DYNAMATIC/bin/generators/rtl-text-generator\\\" \\\"$$DYNAMATIC/data/vhdl/handshake/constant.vhd\\\" \\\"$$OUTPUT_DIR/$$MODULE_NAME.vhd\\\" ENTITY_NAME $$MODULE_NAME VALUE $$VALUE"
          else
            s"\\\"$$DYNAMATIC/bin/generators/rtl-constant-generator-verilog\\\" \\\"$$DYNAMATIC/data/verilog/handshake/constant.v\\\" \\\"$$OUTPUT_DIR/$$MODULE_NAME.v\\\" $$MODULE_NAME $$VALUE"
        )
      )
    ),
    (
      "handshake.end",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "NUM_MEMORIES",
              RTLConfigParameterTypes.Unsigned,
              eq = Some(Left(0)),
              generic = Some(false)
            ),
            RTLConfigParameter(
              "DATAL_TYPE",
              RTLConfigParameterTypes.Dataflow,
              eq = Some(Left(0)),
              generic = Some(false)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/end_memless" + externalPrimitivesFExt
        ),
        moduleName = Some("end_sync_memless_dataless"),
        dependencies =
          Some(if (externalPrimitivesType == "vhdl") Seq("types") else Seq())
      )
    ),
    (
      "handshake.end",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "NUM_MEMORIES",
              RTLConfigParameterTypes.Unsigned,
              eq = Some(Left(0)),
              generic = Some(false)
            ),
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/end_memless" + externalPrimitivesFExt
        ),
        moduleName = Some("end_sync_memless"),
        dependencies = Some(Seq("end_sync_memless_dataless"))
      )
    ),
    (
      "handshake.end",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "NUM_MEMORIES",
              RTLConfigParameterTypes.Unsigned,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              eq = Some(Left(0)),
              generic = Some(false)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/end" + externalPrimitivesFExt
        ),
        moduleName = Some("end_sync_dataless"),
        dependencies = Some(Seq("logic", join_dep))
      )
    ),
    (
      "handshake.end",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "NUM_MEMORIES",
              RTLConfigParameterTypes.Unsigned,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/end" + externalPrimitivesFExt
        ),
        moduleName = Some("end_sync"),
        dependencies = Some(Seq("end_sync_dataless"))
      )
    ),
    (
      "handshake.not",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "DATA_TYPE",
              RTLConfigParameterTypes.Dataflow,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/not" + externalPrimitivesFExt
        ),
        moduleName = Some("logic_not")
      )
    ),
    (
      "mem_to_bram",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter(
              "DATA_WIDTH",
              RTLConfigParameterTypes.Unsigned,
              lb = Some(1)
            ),
            RTLConfigParameter(
              "ADDR_WIDTH",
              RTLConfigParameterTypes.Unsigned,
              lb = Some(1)
            )
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/support/mem_to_bram" + externalPrimitivesFExt
        )
      )
    ),
    (
      "handshake.divsi",
      RTLConfigModuleSpec(
        externalPrimitivesType,
        parameters = Left(
          Seq(
            RTLConfigParameter("DATA_TYPE", RTLConfigParameterTypes.Dataflow)
          )
        ),
        generic = Some(
          s"$$DYNAMATIC/data/$externalPrimitivesType/arith/divsi" + externalPrimitivesFExt
        ),
        dependencies = Some(Seq(join_dep, "delay_buffer"))
      )
    )
  ) ++ implementedInVHDL

  // Used if any of the unimplemented components need depdendencies
  // Some of these are never in the dependencies of any component...
  def extraRTLConfigObjects(
    implicit externalPrimitivesType: String
  ): Seq[JSONValue] = Seq(
    JSONObject.fromStringJSONMap(
      Map(
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/support/join" + externalPrimitivesFExt
        ),
        "hdl" -> JSONString(externalPrimitivesType)
      )
    ),
    JSONObject.fromStringJSONMap(
      Map(
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/support/delay_buffer" + externalPrimitivesFExt
        ),
        "hdl" -> JSONString(externalPrimitivesType)
      )
    ),
    JSONObject.fromStringJSONMap(
      Map(
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/support/eager_fork_register_block" + externalPrimitivesFExt
        ),
        "hdl" -> JSONString(externalPrimitivesType)
      )
    ),
    JSONObject.fromStringJSONMap(
      Map(
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/support/elastic_fifo_inner" + externalPrimitivesFExt
        ),
        "hdl" -> JSONString(externalPrimitivesType)
      )
    ),
    JSONObject.fromStringJSONMap(
      Map(
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/support/dataless/elastic_fifo_inner" + externalPrimitivesFExt
        ),
        "module-name" -> JSONString("elastic_fifo_inner_dataless"),
        "hdl" -> JSONString(externalPrimitivesType)
      )
    ),
    JSONObject.fromStringJSONMap(
      Map(
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/support/logic" + externalPrimitivesFExt
        ),
        "hdl" -> JSONString(externalPrimitivesType)
      )
    ),
    JSONObject.fromStringJSONMap(
      Map(
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/support/flopoco_ip_cores" + externalPrimitivesFExt
        ),
        "hdl" -> JSONString(externalPrimitivesType)
      )
    ),
    JSONObject.fromStringJSONMap({
      val v = Map(
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/support/mc_support" + externalPrimitivesFExt
        ),
        "hdl" -> JSONString(externalPrimitivesType)
      )
      if (externalPrimitivesType == "vhdl")
        v.updated("dependencies", JSONList.fromStrings(Seq("types")))
      else v
    }),
    JSONObject.fromStringJSONMap({
      val v = Map(
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/support/merge_notehb" + externalPrimitivesFExt
        ),
        "hdl" -> JSONString(externalPrimitivesType)
      )
      if (externalPrimitivesType == "vhdl")
        v.updated("dependencies", JSONList.fromStrings(Seq("types")))
      else v
    }),
    JSONObject.fromStringJSONMap(
      Map(
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/support/dataless/merge_notehb" + externalPrimitivesFExt
        ),
        "hdl" -> JSONString(externalPrimitivesType),
        "module-name" -> JSONString("merge_notehb_dataless")
      )
    ),
    JSONObject.fromStringJSONMap(
      Map(
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/dataless/tehb" + externalPrimitivesFExt
        ),
        "hdl" -> JSONString(externalPrimitivesType),
        "module-name" -> JSONString("tehb_dataless")
      )
    ),
    JSONObject.fromStringJSONMap(
      Map(
        "parameters" -> JSONList(
          Seq(
            JSONObject(
              Map(
                JSONString("name") -> JSONString("NUM_SLOTS"),
                JSONString("type") -> JSONString("unsigned"),
                JSONString("eq") -> JSONInteger(1),
                JSONString("generic") -> JSONBoolean(false)
              )
            ),
            JSONObject(
              Map(
                JSONString("name") -> JSONString("DATA_TYPE"),
                JSONString("type") -> JSONString(RTLConfigParameterTypes.Dataflow),
                JSONString("data-lb") -> JSONInteger(1)
              )
            )
          )
        ),
        "generic" -> JSONString(
          s"$$DYNAMATIC/data/$externalPrimitivesType/handshake/tehb" + externalPrimitivesFExt
        ),
        "dependencies" -> JSONList.fromStrings(Seq("tehb_dataless")),
        "hdl" -> JSONString(externalPrimitivesType)
      )
    )
  ) ++ {
    if (externalPrimitivesType == "vhdl")
      Seq(
        JSONObject.fromStringJSONMap(
          Map(
            "generic" -> JSONString(
              s"$$DYNAMATIC/data/$externalPrimitivesType/support/types" + externalPrimitivesFExt
            ),
            "hdl" -> JSONString(externalPrimitivesType)
          )
        )
      )
    else Seq()
  }

  val mlirOpToExtraParams = Map[String, Map[String, String]] (
    // "handshake.cmpi" -> Map("PRED"->"${PREDICATE}"),
    "handshake.addi" -> Map("OP" -> "add"),
    "handshake.subi" -> Map("OP" -> "sub"),
    "handshake.andi" -> Map("OP" -> "and"),
    "handshake.ori" -> Map("OP" -> "or"),
    "handshake.xori" -> Map("OP" -> "xor"),
    "handshake.shli" -> Map("OP" -> "shl"),
    "handshake.shrsi" -> Map("OP" -> "shrsi"),
    "handshake.shrui" -> Map("OP" -> "shrui")
  )

  def generateRTLConfig(implicit externalPrimitivesType: String): RTLConfig = {
    // compNameToDynCompName goes from "arith.cmpi"->"Comparator"
    val mlirOpToPrimName = compNameToDynCompName.map {
      case (mlirOpName, "") => (mlirOpName, None)
      case (mlirOpName, compName) => (mlirOpName, Some(compName))
    }

    val generators = mlirOpToRTLConfigModuleSpec.map { case (mlirOp, (typeObj, spec)) =>
      genModuleVariants (
        mlirOp,
        spec.copy(alreadyDefinedParams =
          mlirOpToExtraParams.getOrElse(
            mlirOp,
            Map()
          ) ++ spec.alreadyDefinedParams
        ),
        Some(typeObj)
      )
    }

    val unimplemented = externalRTLs.map { case (mlirOp, spec) =>
      genModuleVariants(mlirOp, spec)
    }

    RTLConfig (
      (generators ++ unimplemented).toSeq.flatten,
      externalPrimitivesType
    )
  }

  def main(args: Array[String]): Unit = {
    generateRTLConfig("vhdl")
    generateRTLConfig("verilog")
  }
}
