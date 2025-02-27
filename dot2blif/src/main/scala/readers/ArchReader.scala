package readers

import arch._
import archs.VPRFC
import archs.Frac
import archs.Abs

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import java.io.FileReader

// TODO this is deprecated, remove
object ArchParser extends JavaTokenParsers {
  // def archs: Parser[List[Arch]] = rep(arch)

  // def arch: Parser[Arch] = (all | inst)

  // def all: Parser[Arch] = "place" ^^ { case _ => Arch.place(100) }

  // def inst: Parser[Arch] = name~":"~dec~"("~opts~")" ^^ {
  //   case id~":"~size~"("~opts~")" => {
  //     val isRdy = opts.nonEmpty && (opts.contains("RdyVld") || opts.contains("Rdy"))
  //     val isVld = opts.nonEmpty && (opts.contains("RdyVld") || opts.contains("Vld"))

  //     Arch(id, size, isRdy, isVld, false, -1, VPRFC(Frac, 0.5), VPRFC(Abs, 2))
  //   }

  //   case _ => scala.sys.error("Cannot read archs.cfg file")
  // }

  // // def attrs: Parser[Map[String, String]] = repsep((sattr_pair|lattr_pair), ",") ^^ (Map() ++ _)

  // // def sattr_pair: Parser[(String, String)] = name~"="~(bool | stringLit | dec | name) ^^ {
  // //   case attrName~"="~attrValue => (attrName, attrValue.toString())
  // // }

  // // // TODO for now List[Int], check later how to generalize to any type properly
  // // def lattr_pair: Parser[(String, String)] = name~"="~"("~repsep(number, ",")~")" ^^ {
  // //   case attrName~"="~"("~attrs~")" => {
  // //     (attrName, attrs)
  // //   }

  // //   case _ => scala.sys.error("Cannot read archs.cfg file")
  // // }

  // def opts: Parser[List[String]] = repsep(ident, ",")
  // def number: Parser[Int] = opt("-")~dec ^^ {
  //   case Some("-")~d => -d
  //   case None~d => d
  //   case _ => scala.sys.error("Cannot read archs.cfg file")
  // }

  // def dec: Parser[Int] = decimalNumber ^^ {_.toInt}
  // def bool: Parser[String] = "True" | "False"
  // def stringLit: Parser[String] = stringLiteral ^^ {
  //   case s => s.drop(1).dropRight(1)
  // }

  // def name:  Parser[String] = ident

  def apply(fName: String): List[Arch] = {
    ???
    // parseAll(this.archs, new FileReader(fName)) match {
    // case Success(result, _) => result
    // case failure: NoSuccess => {
    //   println("Parsing failed at: " + failure.next.pos)
    //   scala.sys.error(failure.msg)
    // }
  }
}
