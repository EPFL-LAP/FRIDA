package printers

import arch._
import crkt._
import util._

import core.Namer
import frontend.GlobalParamsInst

import io.AnsiColor._
import java.io.FileWriter
import scala.collection.mutable
import archs.Entry
import archs.Exit
import packerv2.ImplementedMolecule
import archs.EntryParams
import core.AImpl

// A net is names according to the name of corresponding output of node
// Assumes all nodes from the graph already fit under the ckt constraints

object BlifPrinter {
  val clkIoName = "ioClk"

  def printBlockPort(bp: BlockPort): String = {
    (0 until bp.words)
      .map {
        w =>
          {
            Namer(Pin(bp.id, w))
          }
      }
      .mkString(" ")
  }

  def block2blifModel(b: TBlock, arch: Arch): String = {
    val builder = new StringBuilder()

    val inPorts = b.blockInterface.ports
      .filter(
        (id, bp) => arch.contains(id)
      )
      .filter(_._1.pt == PTInput)
      .map {
        (id, bp) =>
          {
            printBlockPort(bp)
          }
      }
      .mkString(" ")

    val clk = if (b.physicalInfo.clocked) " clk " else ""

    val outPorts = b.blockInterface.ports
      .filter(
        (id, bp) => arch.contains(id)
      )
      .filter(_._1.pt == PTOutput)
      .map {
        (id, bp) =>
          {
            printBlockPort(bp)
          }
      }
      .mkString(" ")

    builder.append(".model " + b.vprName + "\n")
    builder.append(".inputs " + inPorts + clk + "\n")
    builder.append(".outputs " + outPorts + "\n")
    builder.append(".blackbox\n")
    builder.append(".end\n\n")

    builder.toString()
  }

  def getProducingPort(p: Port): Port = {
    p.id.pt match {
      case PTInput  => assert(p.distPorts.size == 1); p.distPorts.head._2
      case PTOutput => p
      case other    => scala.sys.error("Expected port direction")
    }
  }

  def printBlockPort(
      mol: ImplementedMolecule,
      n: TNode,
      bp: BlockPort,
      ports: List[Port],
      arch: Arch
  ): String = {
    val archPrimName = mol.primMap.filter(_._2 == n.name).head._1

    ports
      .map {
        port =>
          {
            assert(arch.contains(port.id))

            val prodPort = getProducingPort(port)
            val archPinCandidates = mol.locToValue
              .filter(_._2 == port.nodeID())
              .filter(_._1.pbName != mol.tile.name) // Not interested in the re-entry on feedback paths

            if (archPinCandidates.size != 1) {
              println("" + prodPort + " -> " + port)
              println(archPinCandidates.mkString("\n"))
            }

            assert(archPinCandidates.size == 1, "for " + port + " candidates: " + archPinCandidates)

            val pin = archPinCandidates.head._1.pin

            Namer(pin) + "=" + Namer(prodPort)
          }
      }
      .mkString(" ")
  }

  def printPorts(node: TNode, arch: Arch, mol: ImplementedMolecule): String = {
    val prim = mol.getPrim(node)

    node.ports
      .filter(
        (id, ps) => arch.contains(id)
      )
      .map {
        (id, ports) =>
          {
            val bp = prim.blockInterface.ports(id)

            printBlockPort(mol, node, bp, ports, arch)
          }
      }
      .mkString(" ")
  }

  def node2blif(mol: ImplementedMolecule, node: TNode, arch: Arch): String = {
    val builder = new StringBuilder()

    val prim = mol.getPrim(node)

    val primInst = ".subckt " + prim.vprName
    val ports = printPorts(node, arch, mol)
    val clkPort = if (prim.physicalInfo.clocked) "clk=clk" else ""
    val name = ".cname " + node.name

    primInst + " " + ports + " " + clkPort + "\n" + name
  }

  def getBlackboxes(
      g: ElasticGraph,
      node2mol: Map[String, ImplementedMolecule],
      arch: Arch
  ): String = {
    val usedPrimitives = g.nodes
      .map(_._2)
      .map {
        n =>
          {
            val blockCandidates = node2mol(n.name).blockMap.filter(_._2 == n.name)
            assert(blockCandidates.size == 1)

            blockCandidates.head._1
          }
      }
      .toSet

    usedPrimitives
      .filter(_.nonEmptyUnder(arch))
      .filter(!_.annotations.contains(AImpl))
      .map {
        b =>
          {
            block2blifModel(b, arch)
          }
      }
      .mkString("\n\n")
  }

  def printIO(p: Port, arch: Arch): String = {
    if (p.thisNode == clkIoName) {
      "clk"
    } else {
      val prodP = getProducingPort(p)
      Namer(prodP)
    }
  }

  def insNames(g: ElasticGraph, arch: Arch): String = {
    g.ios()
      .map {
        n =>
          {
            n.ports.map(_._2).flatten.filter(_.id.pt == PTOutput)
          }
      }
      .flatten
      .filter(
        p => arch.contains(p.id)
      )
      .map(
        p => printIO(p, arch)
      )
      .mkString(" ")
  }

  def outsNames(g: ElasticGraph, arch: Arch): String = {
    g.ios()
      .map {
        n =>
          {
            n.ports.map(_._2).flatten.filter(_.id.pt == PTInput)
          }
      }
      .flatten
      .filter(
        p => arch.contains(p.id)
      )
      .map(
        p => printIO(p, arch)
      )
      .mkString(" ")
  }

  def getCLKNode(width: Int): TNode = {
    val portId = BlockPortID(width, PTOutput, PortMeaningWrapper(PMData(None), D), Regular)
    val port = Port(portId, "", Map(), clkIoName, Map(), 0)
    val ports = Map((portId -> (port :: Nil)))

    Node(clkIoName, Entry(EntryParams(width, Set(D))), Map(), Map(), Set(), ports)
  }

  def getNodeToMol(mols: List[ImplementedMolecule]): Map[String, ImplementedMolecule] = {
    mols
      .map {
        mol =>
          {
            mol.blockMap
              .map(_._2)
              .map(
                n => (n -> mol)
              )
          }
      }
      .flatten
      .toMap
  }

  def print(
      fname: String,
      g: ElasticGraph,
      arch: Arch,
      mols: List[ImplementedMolecule]
  ): Unit = {
    val node2mol = getNodeToMol(mols)

    val writer = Util.writeOpen(fname)
    writer.write(".model top\n")

    val clkWidth = if (arch.place) 1 else arch.width
    val clkNode = getCLKNode(clkWidth)
    val gWithClk = ElasticGraph(g.nodes ++ Map((clkNode.name -> clkNode)))

    val ins = insNames(gWithClk, arch)
    val outs = outsNames(gWithClk, arch)

    writer.write(".inputs " + ins + "\n")
    writer.write(".outputs " + outs + "\n\n")

    gWithClk.nodes.map(_._2).filter(!_.nType.isIo).filter(_.nonEmptyUnder(arch)).foreach {
      n =>
        {
          val mol = node2mol(n.name)
          val sinkRegTile = ArchPrinter.addSinkRegOnDanglingPorts(mol.tile, arch)
          val nBlockMap = mol.blockMap.map {
            (b, n) =>
              {
                (ArchPrinter.addSinkRegOnDanglingPort(b, arch, false), n)
              }
          }

          val molWithSinkReg = ImplementedMolecule(
            sinkRegTile,
            mol.locMap,
            mol.mm,
            nBlockMap,
            mol.primMap,
            mol.locToValue
          )

          writer.write(node2blif(molWithSinkReg, n, arch))
          writer.write("\n\n")
        }
    }

    writer.write(getBlackboxes(g, node2mol, arch))
    writer.close()
  }

  def apply(
      params: GlobalParamsInst,
      g: ElasticGraph,
      archs: List[Arch],
      mols: List[ImplementedMolecule],
      tiles: List[RootPb]
  ): Unit = {
    archs.foreach {
      (arch: Arch) =>
        {
          val blif = params.buildDir + "/vpr/" + params.circuitPref + "_" + arch.name + ".eblif"

          println(CYAN + "Printing : " + blif + RESET)
          print(blif, g, arch, mols)
        }
    }
  }
}
