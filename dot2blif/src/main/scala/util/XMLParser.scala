package util

import java.io.FileWriter
import scala.xml.XML
import scala.xml.PrettyPrinter
import java.io.PrintWriter
import frontend.GlobalParamsInst

object XMLParser {
  def parseFile(params: GlobalParamsInst, fileName: String): scala.xml.Elem = {
    XML.loadFile(params.buildDir + "/vpr/" + fileName)
  }

  def writeFile(params: GlobalParamsInst, fileName: String, rrg: scala.xml.Elem) = {
    val path: String = params.buildDir

    val prettyPrinter = new scala.xml.PrettyPrinter(180, 2)
    val prettyXml = prettyPrinter.format(rrg)
    new PrintWriter(path + "/vpr/" + fileName) { write(prettyXml); close }
  }
}
