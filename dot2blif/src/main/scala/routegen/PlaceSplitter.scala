package routegen

import arch._
import util._
import crkt._
import packerv2.ImplementedMolecule
import frontend.GlobalParamsInst

import scala.io.Source
import java.io.FileWriter

object PlaceSplitter {
  val keepIfIn = Set("Netlist_File", "Array size")

  // keep line if molecule is empty under arch (forall in mol : not in arch)
  def keepLine(
      line: String,
      arch: Arch,
      molToName: Map[String, ImplementedMolecule]
  ): Boolean = {
    if (line.size == 0) {
      true
    } else if (
      keepIfIn
        .map(
          (keyword: String) => line.contains(keyword)
        )
        .reduce(_ || _)
    ) {
      true
    } else if (line.charAt(0) == '#') {
      true
    } else {
      val locName = line.split("""\s+""")(0)

      if (locName == "clk") {
        true
      } else {
        molToName(locName).locMap.exists {
          (loc, prod) =>
            {
              arch.contains(loc.pin.id)
            }
        }
      }
    }
  }

  def toIncrementalIds(lines: List[String]): List[String] = {
    val blocks = lines.filter {
      (l: String) =>
        {
          val s = l.split("\\s+"); s(s.size - 1).contains("#")
        }
    }

    val oLines = lines.filterNot {
      (l: String) =>
        {
          val s = l.split("\\s+"); s(s.size - 1).contains("#")
        }
    }

    val nBlocks = blocks.zip(0 until blocks.size).map {
      (l: String, id: Int) =>
        {
          val s: Array[String] = l.split("\\s+")
          s(s.size - 1) = "#" + id
          s.mkString("\t")
        }
    }

    oLines ++ nBlocks
  }

  def molToName(mols: List[ImplementedMolecule]): Map[String, ImplementedMolecule] = {
    mols.map {
      mol =>
        {
          val name = mol.primMap.map(_._2).head // TODO is this repeatable???

          (name, mol)
        }
    }.toMap
  }

  def apply(
      params: GlobalParamsInst,
      archs: List[Arch],
      mols: List[ImplementedMolecule]
  ): Unit = {
    val placedFName = params.buildDir + "/vpr/" + params.circuitPref + ".place"
    val placed = Source.fromFile(placedFName).getLines.toList

    val nameToTile = molToName(mols)

    archs.filter(!_.place).foreach {
      (arch: Arch) =>
        {
          val placedFile = placedFName.replace(".place", "_" + arch.name + ".place")
          println("printing filtered placed to : " + placedFile)

          // val placedCorrectNames = placed.map((line: String) => renameLine(line, nameMapping, arch))
          val placedArchTmp = placed.filter(
            (line: String) => keepLine(line, arch, nameToTile)
          )
          val placedArch = toIncrementalIds(placedArchTmp)

          val writer: FileWriter = Util.writeOpen(placedFile)
          writer.write(placedArch.mkString("\n"))
          writer.close()
        }
    }
  }
}
