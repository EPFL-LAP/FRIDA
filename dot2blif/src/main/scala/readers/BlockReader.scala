package readers

import arch._
import core.Annotation
import core.ADataOnly
import core.AWirePort

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import java.io.Reader

import arch.PhysicalInfo
import scala.compiletime.ops.double
import core._
import archs._

// TODO this is deprecated, remove
class BlockConfigParser extends JavaTokenParsers {
  // val annosStr = Set(AImpl.str, AIo.str, ACanIdentity.str)
  // val namedAttrsStr = Set(NABlifModel.str)

  // def config(libPath: String): Parser[Map[String, TBlock]] = {
  //   rep(config_id~":"~"["~"ins"~":"~ports(true)~";"~"outs"~":"~ports(false)~"]"~"{"~opt(attrs)~"}") ^^ {
  //     case l => l.map {
  //       case blockName~":"~"["~"ins"~":"~inPorts~";"~"outs"~":"~outPorts~"]"~"{"~attrsMap~"}" => {
  //         val attrs = attrsMap.getOrElse(Map[String, String]())

  //         val annos = attrs.filter(annosStr contains _._1).map((k,v) => Annotation.fromStr(k)).toSet
  //         val namedAttrs = attrs.filter(namedAttrsStr contains _._1).map((k,v) => (NamedAttribute.fromStr(k), v))

  //         val prim = typeNameToType(blockName.replaceAll("""\d+""", ""))
  //         val params = prim.getParams(attrs)

  //         if(!attrs.contains("chisel")) {
  //           val blockInterface = BlockInterface(inPorts ++ outPorts, blockName, false)
  //           (blockName,
  //            Block(prim, params, blockInterface, PhysicalInfo.empty(), annos, namedAttrs)
  //           )
  //         } else {
  //           val chiselName = attrs("chisel")
  //           val physicalInfo = PhysicalInfo.empty()

  //           val clk = true //Timing.clocked(physicalInfo.timings)
  //           val blockInterface = BlockInterface(inPorts ++ outPorts, blockName, clk)

  //           (blockName, Block(prim, params, blockInterface, physicalInfo, annos, namedAttrs))
  //         }
  //       }

  //       case _ => throw new Exception("Unexpected String")
  //     }.toMap
  //   }
  // }

  // def ports(isInput: Boolean): Parser[Map[BlockPortID, BlockPort]] = repsep(port_pair(isInput), ",") ^^ {
  //   case l => l.flatten.groupBy(_.id).map( (k,v) => { assert(v.size == 1); (k, v.head) } )
  // }

  // val annosStrings = (
  //   ADataOnly.str
  //     | AEquivalent.str
  //     | AAllowPartialUsage.str
  //     | AWirePort.str
  //     | AOptionalPort.str
  // )

  // def annotation: Parser[Annotation] = annosStrings ^^ { Annotation.fromStr(_) }

  // def annotations: Parser[Set[Annotation]] = repsep(annotation, ",") ^^ { _.toSet }

  // val pmStrings = (
  //   (PMData(None).str ^^^ PMData(None))
  //     | (PMCond(None).str ^^^ PMCond(None))
  //     | (PMAddr(None).str ^^^ PMAddr(None)))

  // def port_pair(isInput: Boolean): Parser[List[BlockPort]] = {
  //   config_id~"("~pmStrings~opt(","~annotations)~")"~":"~dec~"="~dec ^^ {
  //     case portName~"("~pm~annos~")"~":"~portSize~"="~words => {
  //       val pt = if(isInput) PTInput else PTOutput
  //       val id  = BlockPortID(portSize.toInt, pt, PortMeaningWrapper(pm, Impl), Regular)
  //       val annosL = annos match {
  //         case Some(","~annotations) => annotations
  //         case None => Set()
  //         case other => scala.sys.error("match error")
  //       }

  //       val locMapping = (0 until words.toInt).map(i => (i, i)).toMap

  //       BlockPort(id, words.toInt, annosL, locMapping) :: Nil
  //     }

  //     case m => scala.sys.error("UnexpectedMatch " + m)
  //   }
  // }

  // def attrs: Parser[Map[String, String]] = repsep(attr_pair, ",") ^^ (Map() ++ _)

  // def attr_pair: Parser[(String, String)] = config_id~"="~(bool | stringLit | dec | config_id) ^^ {
  //   case attrName~"="~attrValue => (attrName, attrValue)
  // }

  // def config_id: Parser[String] = ident

  // def stringLit: Parser[String] = stringLiteral ^^ {
  //   case s => s.drop(1).dropRight(1)
  // }

  // def dec: Parser[String] = decimalNumber

  // def bool: Parser[String] = "True" | "False"

  def apply(reader: Reader, libPath: String): Map[String, TBlock] = {
    ???
    // parseAll(this.config(libPath), reader) match {
    //   case Success(result, _) => result
    //   case failure: NoSuccess => {
    //     println("Parsing failed at: " + failure.next.pos)
    //     scala.sys.error(failure.msg)
    //   }
    // }
  }
}
