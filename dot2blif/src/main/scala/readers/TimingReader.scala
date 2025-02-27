package readers

import arch._

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import java.io.Reader
import java.io.FileReader
import java.io.File
import io.AnsiColor._

import arch.PhysicalInfo
import frontend.GlobalParamsInst
import readers.BlockPortIDParser.hsType

object TimingReader extends JavaTokenParsers {
  def timingStmts: Parser[PhysicalInfo] =
    bName ~ "(" ~ dec ~ ")" ~ "{" ~ attrs ~ "}" ~ ":" ~ "[" ~ repsep(timingStmt, ",") ~ "]" ^^ {
      case id ~ "(" ~ area ~ ")" ~ "{" ~ attrs ~ "}" ~ ":" ~ "[" ~ timingList ~ "]" =>
        PhysicalInfo(area.toDouble, timingList, attrs)
      case _ => throw new Exception("Unexpected String")
    }

  def bName: Parser[String] = ident ~ opt("." ~ dec ~ "ns") ^^ {
    case id ~ None                   => id
    case id ~ Some("." ~ clk ~ "ns") => id + "." + clk + "ns"
    case _                           => scala.sys.error("Unexpected string")
  }

  def timingStmt: Parser[Timing] = lhs ~ ":" ~ "{" ~ attrs ~ "}" ^^ {
    case (src: PortInstance, dst: PortInstance) ~ ":" ~ "{" ~ attrs ~ "}" => {
      val maxDelay = attrs.getOrElse("maxDelay", "")
      val minDelay = attrs.getOrElse("minDelay", "")

      CombTiming(src, dst, minDelay, maxDelay)
    }

    case (p1: PortInstance) ~ ":" ~ "{" ~ attrs ~ "}" => {
      val t_clock_to_q_min = attrs.getOrElse("T_clock_to_Q_min", "")
      val t_clock_to_q_max = attrs.getOrElse("T_clock_to_Q_max", "")
      val t_hold = attrs.getOrElse("T_hold", "")
      val t_setup = attrs.getOrElse("T_setup", "")

      RegTiming(p1, t_setup, t_hold, t_clock_to_q_min, t_clock_to_q_max)
    }

    case _ => scala.sys.error("Unexpected String")
  }

  def attrs: Parser[Map[String, String]] = repsep(attr_pair, ",") ^^ (Map() ++ _)

  def attr_pair: Parser[(String, String)] = ident ~ "=" ~ (stringLit | dec) ^^ {
    case attrName ~ "=" ~ attrValue => (attrName, attrValue)
  }

  def lhs: Parser[PortInstance | (PortInstance, PortInstance)] = (port ~ "->" ~ port | port) ^^ {
    case (p1: PortInstance) ~ "->" ~ (p2: PortInstance) => (p1, p2)
    case (p1: PortInstance)                             => p1
    case other                                          => scala.sys.error("Cannot pase port instance")
  }

  val pmStrings = ((PMData(None).str ^^^ PMData(None))
    | (PMCond(None).str ^^^ PMCond(None))
    | (PMAddr(None).str ^^^ PMAddr(None)))

  // Dummy ports handled at the very latest since it is only an artificial constraint from VPR
  def port: Parser[PortInstance] = ("(" ~ dec ~
    "," ~ (("In" ^^^ PTInput) | ("Out" ^^^ PTOutput)) ~
    "," ~ pmStrings ~
    "," ~ ((Dummy.str ^^^ Dummy) | (Regular.str ^^^ Regular)) ~ ")" ~
    "[" ~ dec ~ "]"
    ~ opt("." ~ (("v" ^^^ HSValid) | ("r" ^^^ HSReady)))) ^^ {
    case "(" ~ width ~ "," ~ pt ~ "," ~ pm ~ "," ~ dum ~ ")" ~ "[" ~ word ~ "]" ~ Some("." ~ hsT) => {
      val id = BlockPortID(width.toInt, pt, PortMeaningWrapper(pm, Hs), dum)

      hsT match {
        case HSReady => {
          // We will want to clean this up and be consistend in the library too...
          PortInstance(id.flipped(), word.toInt, Some(hsT))
        }

        case HSValid => {
          PortInstance(id, word.toInt, Some(hsT))
        }
      }
    }

    case "(" ~ width ~ "," ~ pt ~ "," ~ pm ~ "," ~ dum ~ ")" ~ "[" ~ word ~ "]" ~ None => {
      val id = BlockPortID(width.toInt, pt, PortMeaningWrapper(pm, D), dum)
      PortInstance(id, word.toInt, None)
    }

    case _ => scala.sys.error("Cannot part port")
  }

  def dec: Parser[String] = decimalNumber

  def stringLit: Parser[String] = stringLiteral ^^ {
    case s => s.drop(1).dropRight(1)
  }

  def apply(libFile: String): PhysicalInfo = {
    val fixup = if (libFile contains "Fork") {
      libFile.replace("W32", "W0").replace("W1", "W0")
    } else {
      libFile
    }

    if (File(fixup).exists()) {
      val reader = new FileReader(fixup)

      parseAll(this.timingStmts, reader) match {
        case Success(result, _) => result
        case failure: NoSuccess => {
          println(fixup)
          println("Parsing failed at: " + failure.next.pos)
          scala.sys.error(failure.msg)
        }
      }
    } else {
      // println(RED + "Dit not find " + libFile + " in library" + RESET)

      // TODO sanity assertions

      PhysicalInfo.empty()
    }
  }
}
