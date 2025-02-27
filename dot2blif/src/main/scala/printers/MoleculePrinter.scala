package printers

import arch._
import crkt._
import packerv2._
import frontend.GlobalParamsInst
import archs.Entry
import archs.EntryParams
import archs.Exit
import archs.ExitParams
import archs.Fork

import io.AnsiColor._
import scala.collection.mutable.{Set => MSet}

case class ExtUniqueID(var id: Int) {
  def incr(): Unit = id = id + 1
}

object MoleculePrinter {
  def apply[M <: AbsMolecule](params: GlobalParamsInst, m: MappedMolecule[M], silent: Boolean): Unit = {
    val rrMask = m.pat.rrs -- m.assignments
      .collect {
        case rra: RRAssignment => rra
      }
      .map(_.rr)
    val primMask = m.pat.prims -- m.assignments
      .collect {
        case a: Assignment => a
      }
      .map(_.pn)

    RRGPrinter(params, m.pat, Some(m.name), rrMask, primMask, silent)
  }

  def apply(params: GlobalParamsInst, m: Molecule, silent: Boolean): Unit = {
    val rrMask = Set()
    val primMask = Set()

    RRGPrinter(params, m.pat, Some(m.name), Set(), Set(), silent)
  }

  def printInternalCrkt(params: GlobalParamsInst, m: Molecule): Unit = {
    val fName = params.buildDir + "/molecules/crkt/" + RRGPrinter.validName(m.name) + ".dot"
    println(YELLOW + fName + RESET)

    val crkt = ElasticGraph(m.mapping.nodeNames)

    DotPrinter(fName)(crkt, Set())
  }
}
