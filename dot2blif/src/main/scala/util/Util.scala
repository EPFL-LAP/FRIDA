package util

import arch._
import frontend._
import crkt._
import handshake._
import core._
import printers.DotPrinter
import printers.TimingFilter

import java.io.FileWriter
import java.io.FileReader
import java.io.File
import scala.io.Source

import io.AnsiColor._

import math.ceil
import math.log10

import collection.mutable.{Map => MMap}

case class GID(var gid: Int)

object Util {
  val keywords = Set("bypass", "bypassOut", "bypassIn")
  val warningColor = MAGENTA
  val timings = MMap[String, Long]()

  def writeOpen(fname: String): FileWriter = {
    val file = new File(fname)
    file.getParentFile().mkdirs()
    val writer = new FileWriter(file)

    writer
  }

  def timeOf[T](name: String, log: Boolean)(f: () => T): T = {
    val before = System.nanoTime

    val res = f()

    val total = System.nanoTime - before
    val totalTimeMillis = total / 1000000

    if (!log) {
      println(name + " took " + totalTimeMillis + "ms")
    } else {
      timings(name) = (timings.getOrElse(name, 0L) + totalTimeMillis)
    }

    res
  }

  def printTimingLog(): Unit = {
    println
    timings.toSeq.sortBy(_._2).foreach {
      (fName, time) =>
        {
          val name = f"$fName%30s"
          val t = f"$time%8d"

          println(name + " took " + YELLOW + t + RESET + "ms in total.")
        }
    }
    println
  }

  def isAttrTrue(attrs: Map[String, String], attrName: String): Boolean = {
    attrs.contains(attrName) && (attrs(attrName) == "true")
  }

  def log2ceil(a: Double) = ceil(log10(a) / log10(2.0))

  def validVprName(s: String) = s.replace(":", "").replace("/", "")
}
