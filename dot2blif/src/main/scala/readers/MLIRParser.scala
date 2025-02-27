package readers

import mlir._

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.io.Source

// Largely taken from the grammar presented here: https://mlir.llvm.org/docs/LangRef/

// TODO put the datastructures into an mlir package

object MLIRParser extends JavaTokenParsers {
  def letter: Parser[String] = "[a-zA-Z]".r
  def digit: Parser[Int] = "[0-9]".r ^^ { _.toInt }
  def idPunct: Parser[String] = "[$._-]".r
  def number: Parser[Long] = """\d+""".r ^^ { _.toLong }
  def bigNumber: Parser[BigInt] = """\d+""".r ^^ { BigInt(_) }
  def float: Parser[Double] = "[-+]?[0-9]+[.][0-9]*([eE][-+]?[0-9]+)?".r ^^ { _.toDouble }

  def bareId: Parser[String] = (letter | "[_]".r) ~ rep(letter | digit | "[_$.]".r) ^^ {
    case prefix ~ suffix => prefix + suffix.mkString("")
  }

  def suffixId: Parser[String] = (
    (rep1(digit)) ^^ { _.mkString("") }
      | ((letter | idPunct) ~ rep(letter | idPunct | digit)) ^^ {
        case prefix ~ suffix => prefix + suffix.mkString("")
      }
  )

  def valueId: Parser[String] = "%" ~ suffixId ^^ {
    case prefix ~ suffix => prefix + suffix
  }

  def baseIdList: Parser[Seq[String]] = rep1sep(bareId, ",")
  def valueIdList: Parser[Seq[String]] = rep1sep(valueId, ", ")

  def functionTypeRes: Parser[Seq[MLIRType]] = (
    mlirTypeList
      | mlirType ^^ { _ :: Nil }
  )

  def functionType: Parser[MLIRFunc] = mlirTypeList ~ "->" ~ functionTypeRes ^^ {
    case ins ~ "->" ~ outs => MLIRFunc(ins.toList, outs.toList)
  }

  def integerType: Parser[MLIRInteger] = opt("u") ~ "i" ~ number ^^ {
    case Some("u") ~ "i" ~ width => MLIRUInt(width.toInt)
    case None ~ "i" ~ width      => MLIRInt(width.toInt)
    case other                   => scala.sys.error("Unexpected mlir type.")
  }

  // TODO we do not support memref with *
  def axis: Parser[Int] = number <~ "x" ^^ { _.toInt }
  def dimension: Parser[List[Int]] = rep1(axis)

  // TODO only support array of integers so far
  def memrefType: Parser[MLIRMemref] = "memref" ~ "<" ~ dimension ~ integerType ~ ">" ^^ {
    case "memref" ~ "<" ~ dim ~ t ~ ">" => MLIRMemref(dim, t)
    case other                          => scala.sys.error("Unexpected match.")
  }

  def control: Parser[MLIRControl.type] = "!handshake.control<>" ^^^ { MLIRControl }
  def channel: Parser[MLIRChannel] = "!handshake.channel<" ~> opt("u") ~ "i" ~ number <~ ">" ^^ {
    case Some("u") ~ "i" ~ width => MLIRChannel(MLIRUInt(width.toInt))
    case None ~ "i" ~ width      => MLIRChannel(MLIRInt(width.toInt))
    case orther                  => scala.sys.error("Unexpected match.")
  }

  def mlirType: Parser[MLIRType] = (
    "none" ^^ {
      _ => MLIRHs
    }
      | integerType
      | functionType
      | memrefType
      | control
      | channel
  )

  def mlirTypeList: Parser[Seq[MLIRType]] = "(" ~> repsep(mlirType, ",") <~ ")"
  def arrayAttr: Parser[ArrayAttr] = "[" ~> rep1sep(attrValue, ",") <~ "]" ^^ { ArrayAttr(_) }

  def bound: Parser[Int] = (
    (number ^^ { _.toInt })
      | ("inf" ^^^ Integer.MAX_VALUE)
  )

  // TODO double check meaning of fields here
  def bufConstraint: Parser[BufConstraint] = {
    stringLiteral ~ ":" ~ "[" ~ number ~ "," ~ bound ~ "]" ~ "," ~ "[" ~ number ~ "," ~ bound ~ "]" ~ "," ~ float ~ "," ~ float ~ "," ~ float ^^ {
      case loc ~ ":" ~ "[" ~ minT ~ "," ~ maxT ~ "]" ~ "," ~ "[" ~ minO ~ "," ~ maxO ~ "]" ~ "," ~ inDelay ~ "," ~ outDelay ~ "," ~ delay => {
        val slotConstr = BufSlotConstraint(minT.toInt, maxT, minO.toInt, maxO)
        val delayConstr = BufDelayConstraint(inDelay, outDelay, delay)

        BufConstraint(loc.replace("\"", "").toInt, slotConstr, delayConstr)
      }
    }
  }

  def bufProps: Parser[BufConstrAttr] =
    "#" ~ "handshake" ~ "<" ~ "bufProps" ~ "{" ~> rep1sep(bufConstraint, ",") <~ "}" ~ ">" ^^ {
      BufConstrAttr(_)
    }

  def dependencyBound: Parser[BigInt] = opt("-") ~ bigNumber ^^ {
    case Some(_) ~ i => -i
    case None ~ i    => i
  }

  def dependencyComponent: Parser[(BigInt, BigInt)] = "[" ~> dependencyBound ~ "," ~ dependencyBound <~ "]" ^^ {
    case lb ~ "," ~ ub => (lb, ub)
  }

  def dependencyArray: Parser[List[(BigInt, BigInt)]] = "[" ~> repsep(dependencyComponent, ",") <~ "]"

  def dependency: Parser[Dependency] = {
    "<" ~> stringLiteral ~ "(" ~ number ~ ")" ~ opt(dependencyArray) <~ ">" ^^ {
      case opName ~ "(" ~ loopDepth ~ ")" ~ Some(dependenceComponent) => {
        Dependency(opName.replace("\"", ""), loopDepth.toInt, dependenceComponent)
      }

      case opName ~ "(" ~ loopDepth ~ ")" ~ None => {
        Dependency(opName.replace("\"", ""), loopDepth.toInt, Nil)
      }

      case other => scala.sys.error("Unexpected match.")
    }
  }

  def deps: Parser[DepsAttr] = "#" ~ "handshake" ~ "<" ~ "deps" ~ "[" ~> rep1sep(dependency, ",") <~ "]" ~ ">" ^^ {
    DepsAttr(_)
  }

  def memInterface: Parser[MemInterfaceAttr] = {
    "#handshake.mem_interface<" ~> (("MC" ^^^ MC) | ("LSQ" ^^^ LSQ)) <~ ">" ^^ {
      case MC  => MemInterfaceAttr(MC)
      case LSQ => MemInterfaceAttr(LSQ)
    }
  }

  def intAttr: Parser[AttrValue] = opt("-") ~ number ~ ":" ~ integerType ^^ {
    case Some(_) ~ i ~ ":" ~ t => {
      AttrInteger(-i.toInt, t)
    }

    case None ~ i ~ ":" ~ t => {
      AttrInteger(i.toInt, t)
    }

    case other => scala.sys.error("Unexpected int attr.")
  }

  def boolAttr: Parser[AttrValue] = (
    "false" ^^^ AttrBoolean(false)
      | "true" ^^^ AttrBoolean(false)
  )

  def pathCut(str: String): Parser[Boolean] = {
    str ~ ":" ~> number ^^ {
      case i => if (i == 1) true else false
    }
  }

  def bufConfAttr: Parser[BufConfigAttr] = {
    "{" ~ "NUM_SLOTS" ~ "=" ~> number ~ ":" ~ "ui32" ~ "," ~ "TIMING" ~ "=" ~ "#handshake<timing" ~ "{" ~ opt(
      pathCut("D") ~ ","
    ) ~ opt(pathCut("V") ~ ",") ~ opt(pathCut("R")) <~ "}" ~ ">" ~ "}" ^^ {
      case slots ~ ":" ~ "ui32" ~ "," ~ "TIMING" ~ "=" ~ "#handshake<timing" ~ "{" ~ dCut ~ vCut ~ rCut => {

        def getCutValue(opt: Option[Boolean ~ String]): Option[Boolean] = {
          opt.map {
            case b ~ "," => b
            case other   => scala.sys.error("Unexpected match")
          }
        }

        BufConfigAttr(
          slots.toInt,
          getCutValue(dCut).getOrElse(false),
          getCutValue(vCut).getOrElse(false),
          rCut.getOrElse(false)
        )
      }

      case other => scala.sys.error("Unexpected match")
    }
  }

  def attrValue: Parser[AttrValue] = (
    arrayAttr
      | functionType
      | stringLiteral ^^ {
        s => AttrString(s.replace("\"", ""))
      }
      | intAttr
      | bufProps
      | deps
      | memInterface
      | boolAttr
      | bufConfAttr
  )

  def attrEntry: Parser[Attr] = (bareId | stringLiteral) ~ "=" ~ attrValue ^^ {
    case name ~ "=" ~ value => Attr(name, value)
    case other              => scala.sys.error("Unexpected attr entry.")
  }

  def dictAttr: Parser[AttrMap] = "{" ~> repsep(attrEntry, ",") <~ "}" ^^ {
    _.map {
      case Attr(n, v) => (n, v)
    }.toMap
  }
  def dictProperties: Parser[AttrMap] = "<" ~> dictAttr <~ ">"

  // TODO Return the empty string since we assume this is never used...
  def successor: Parser[String] = blockId ~ opt(":" ~ blockArgList) ^^ {
    case _ => ""
  }
  def successorList: Parser[String] = "[" ~ rep1sep(successor, ",") ~ "]" ^^ {
    case _ => ""
  }

  // TODO double check this, valueID seems to be a bit more permissive
  def getId(s: String): Int = s.replace("%", "").toInt

  def resultLoc: Parser[Option[Int]] = opt(":" ~> number) ^^ { _.map(_.toInt) }

  def opResult: Parser[Value] = valueId ~ resultLoc ^^ {
    case id ~ loc => Value(id, loc, false)
  }

  def opResultList: Parser[List[Value]] = repsep(opResult, ",") <~ "="

  def argLoc: Parser[Option[Int]] = opt("#" ~> number) ^^ { _.map(_.toInt) }

  def valueUse: Parser[Value] = valueId ~ argLoc ^^ {
    case id ~ loc => Value(id, loc, true)
  }

  def valueUseList: Parser[List[Value]] = rep1sep(valueUse, ",")

  def regionList: Parser[List[Region]] = "(" ~> rep1sep(region, ",") <~ ")"

  def validIdAndType: Parser[Argument] = valueId ~ ":" ~ mlirType ^^ {
    case id ~ ":" ~ t => Argument(Value(id, None, true), t)
    case other        => scala.sys.error("Unexpected value and type.")
  }

  def valueIdAndTypeList: Parser[List[Argument]] = repsep(validIdAndType, ",")
  def blockArgList: Parser[List[Argument]] = "(" ~> valueIdAndTypeList <~ ")"

  def blockId: Parser[String] = "^" ~> suffixId

  def blockLabel: Parser[(String, List[Argument])] = blockId ~ opt(blockArgList) ~ ":" ^^ {
    case bName ~ Some(args) ~ ":" => (bName, args)
    case bName ~ None ~ ":"       => (bName, Nil)
    case other                    => scala.sys.error("Unexpected block label.")
  }

  def operation: Parser[Operation] = opt(opResultList) ~ genericOp ^^ {
    case Some(values) ~ genOp => Operation(values, genOp)
    case None ~ genOp         => Operation(Nil, genOp)
  }

  def block: Parser[Block] = blockLabel ~ rep(operation) ^^ {
    case (bName, args) ~ ops => {
      Block(bName, args, ops)
    }
  }

  def region: Parser[Region] = "{" ~> opt(rep1(operation)) ~ rep(block) <~ "}" ^^ {
    case ops ~ blocks => {
      Region(ops.getOrElse(Nil), blocks)
    }
  }

  def getName(s: String): PrefixedName = {
    val op = s.split('.')

    PrefixedName(op(0).replace("\"", ""), op(1).replace("\"", ""))
  }

  // TODO we ignore dict properties here???
  def genericOp: Parser[GenericOp] = {
    stringLiteral ~ "(" ~ opt(valueUseList) ~ ")" ~ opt(successorList) ~ opt(dictProperties) ~ opt(regionList) ~ opt(
      dictAttr
    ) ~ ":" ~ functionType ^^ {
      case name ~ "(" ~ args ~ ")" ~ None ~ dictProps ~ regions ~ attrs ~ ":" ~ ftype => {
        val configMap = if (dictProps.isEmpty) {
          Map()
        } else {
          dictProps.get.map {
            (k, v) =>
              {
                assert(k == "predicate")
                assert(v.isInstanceOf[AttrInteger])
                (k, ConfigPredicate.integerMap(v.asInstanceOf[AttrInteger].value))
              }
          }
        }

        GenericOp(getName(name), args.getOrElse(Nil), attrs.getOrElse(Map()), regions.getOrElse(Nil), configMap, ftype)
      }

      case other => scala.sys.error("Unexpected generic operation.")
    }
  }

  def module: Parser[GenericOp] = genericOp

  def apply(fName: String): GenericOp = {
    parseAll(module, Source.fromFile(fName).getLines().mkString("\n")) match {
      case Success(result, _) => result
      case failure: NoSuccess => {
        println("Parsing failed at: " + failure.next.pos)
        scala.sys.error(failure.msg)
      }
    }
  }
}
