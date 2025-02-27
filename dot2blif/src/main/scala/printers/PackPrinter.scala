package printers

import arch._
import packerv2._
import frontend.GlobalParamsInst
import core.Namer
import util.Util
import crkt.ElasticGraph
import crkt.PortNodeID
import archs.Entry
import archs.Exit
import dummy.InsertDummiesImplementedMolecule

import io.AnsiColor._

object PackPrinter {
  def blockXML(
      bName: String,
      instance: String,
      ins: List[xml.Elem],
      outs: List[xml.Elem],
      clks: List[xml.Elem],
      subBlocks: List[xml.Elem]
  ): xml.Elem = {
    <block name={bName} instance={instance} mode="default">
      <attributes/>
      <parameters/>
      <inputs>
      {ins}
      </inputs>
      <outputs>
      {outs}
      </outputs>
      <clocks>
      {clks}
      </clocks>
      {subBlocks}
    </block>
  }

  def portXml(loc: PbLoc, prod: Option[Producer]): xml.Elem = {
    val pName = Namer(loc.pin)

    val prodStr = prod match {
      case None                                => "open"
      case Some(SourceProducer(srcLoc, value)) => Namer(value)
      case Some(LinkProducer(srcLoc, link)) => {
        link match {
          case d: Direct => {
            val (dName, _, _) = Namer(d)
            val srcLocStr = Namer(srcLoc)

            srcLocStr + "->" + dName
          }

          case m: Mux => {
            val (mName, _, _) = Namer(m)
            val srcLocStr = Namer(srcLoc)

            srcLocStr + "->" + mName
          }

          case c: CLK => {
            scala.sys.error("CLK should be handled by the getCLK function.")
          }
        }
      }
    }

    <port name={pName}>{prodStr}</port>
  }

  // Never put a clk on IOTile
  // Always put a clk on other tiles
  def getCLK(
      mol: ImplementedMolecule,
      arch: Arch,
      pb: PbType,
      parent: Option[PbType],
      isIo: Boolean // was forcing open on IO clocks... why?
  ): List[xml.Elem] = {
    val pbClocked = if (pb.isInstanceOf[PrimPb]) {
      mol.usesClockedPrimitive(pb, arch)
    } else {
      pb.subBlocks.filter(mol.usesClockedPrimitive(_, arch)).nonEmpty
    }

    if (pbClocked) {
      if (parent.isEmpty) {
        <port name="clk">clk</port> :: Nil
      } else {
        val p = parent.get
        val clkCandidates = p.links
          .collect {
            case c: CLK => c
          }
          .filter(_.dstPb == pb.name)
        assert(clkCandidates.size == 1, p.name + " -> " + pb.name)

        val clk = clkCandidates.head
        val (inC, outC, cName) = Namer(clk)

        val clkStr = inC + "->" + cName

        <port name="clk">{clkStr}</port> :: Nil
      }
    } else {
      Nil
    }
  }

  def getInterfaceMap(mol: ImplementedMolecule, pb: PbType, arch: Arch): (List[xml.Elem], List[xml.Elem]) = {
    val (inLocs, outLocs) = pb.bi.ports
      .map(_._2)
      .map(_.toPins())
      .flatten
      .filter(
        pin => arch.contains(pin.id)
      )
      .map(
        pin => PbLoc(pb.name, pin)
      )
      .partition(_.pin.id.pt == PTInput)

    val inPortsXml = inLocs
      .map(
        loc => portXml(loc, mol.locMap(loc))
      )
      .toList
    val outPortsXml = outLocs
      .map(
        loc => portXml(loc, mol.locMap(loc))
      )
      .toList

    (inPortsXml, outPortsXml)
  }

  def isOpen(mol: ImplementedMolecule, pb: PbType, arch: Arch): Boolean = {
    !mol.pbAssigned(pb, arch)
  }

  def getBlockName(mol: ImplementedMolecule, pb: PbType, arch: Arch): String = {
    if (isOpen(mol, pb, arch)) {
      "open"
    } else {
      pb match {
        case prim: PrimPb => {
          val candidates = mol.primMap.filter(_._1 == prim.name)
          assert(candidates.size == 1, candidates)

          candidates.head._2
        }

        case other => {
          mol.primMap.map(_._2).head
        }
      }
    }
  }

  def printPb(
      mol: ImplementedMolecule,
      pb: PbType,
      recPbs: List[xml.Elem],
      arch: Arch,
      pbToParent: Map[String, PbType],
      index: Int
  ): xml.Elem = {
    val blockName = getBlockName(mol, pb, arch)
    val instance = pb.name + "[" + index + "]"

    if (blockName == "open") {
      <block name="open" instance={instance}/>
    } else {
      val (inPorts, outPorts) = getInterfaceMap(mol, pb, arch)

      val parent = pbToParent.get(pb.name)
      val isIo = mol.tile.name == "IoTile"

      val clk = getCLK(mol, arch, pb, parent, isIo)

      blockXML(blockName, instance, inPorts, outPorts, clk, recPbs)
    }
  }

  def getIns(g: ElasticGraph): List[String] = {
    "clk" :: g
      .ios()
      .map {
        n =>
          {
            n.ports.map(_._2).flatten.filter(_.id.pt == PTOutput).map(Namer(_))
          }
      }
      .flatten
      .toList
  }

  def getOuts(g: ElasticGraph): Seq[String] = {
    g.ios()
      .map {
        n =>
          {
            n.ports.map(_._2).flatten.filter(_.id.pt == PTInput).map {
              p =>
                {
                  assert(p.distPorts.size == 1)
                  "out:" + Namer(p.distPorts.head._2)
                }
            }
          }
      }
      .flatten
      .toList
  }

  def getXML(mol: ImplementedMolecule, pb: PbType, arch: Arch, num: Int, pbToParent: Map[String, PbType]): xml.Elem = {
    assert(pb.nonEmptyUnder(arch))

    val recPbs = if (isOpen(mol, pb, arch)) {
      Nil
    } else {
      pb.subBlocks
        .filter(_.nonEmptyUnder(arch))
        .map {
          recPb =>
            {
              getXML(mol, recPb, arch, num, pbToParent)
            }
        }
        .toList
    }

    val index = if (pb.isInstanceOf[RootPb]) num else 0

    printPb(mol, pb, recPbs, arch, pbToParent, index)
  }

  def getClkTargetLoc(tile: RootPb, ioWidth: Int): PrimLoc = {
    def rec(explored: List[PbType]): PrimLoc = {
      val pb = explored.head

      pb match {
        case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
          prim.prim match {
            case Entry(p) => {
              if ((p.width == ioWidth) && (p.pbs.exists(_.contains(D)))) {
                val locId = BlockPortID(ioWidth, PTOutput, PortMeaningWrapper(PMData(None), D), Regular)
                PrimLoc(pb, Pin(locId, 0))
              } else {
                rec(explored.tail)
              }
            }

            case other => rec(explored.tail)
          }
        }

        case other => {
          rec(explored.tail ++ pb.subBlocks)
        }
      }
    }

    rec(tile :: Nil)
  }

  // TODO a bit ugly... Taken from ImplementMolecule/findPath
  def getClkMappings(
      aLoc: ArchLoc,
      targetLoc: ArchLoc,
      clkVal: PortNodeID,
      acc: List[PinMapping],
      pbParents: Map[String, PbType]
  ): List[PinMapping] = {
    val reachedTarget = targetLoc match {
      case pl @ PrimLoc(pb, pin) => {
        aLoc match {
          case PrimLoc(rpb, pin) => false
          case RouteLoc(rpb, pbLink) => {
            pbLink match {
              case Direct(srcLoc, dstLoc, delay) => dstLoc == PbLoc(pb.name, pin)
              case Mux(sources, dstLoc, delay)   => dstLoc == PbLoc(pb.name, pin)
              case CLK(srcPb, dstPb)             => scala.sys.error("Unexpected link.")
            }
          }
        }
      }

      case rl: RouteLoc => {
        aLoc match {
          case pl: PrimLoc   => false
          case arl: RouteLoc => rl == arl
        }
      }
    }

    val producer = acc.headOption.map(_._2).getOrElse(None)
    val pinMapping = ImplementMolecule.getPinMap(aLoc, producer, clkVal)

    if (reachedTarget) {
      pinMapping :: acc
    } else {
      val recLocs = RRG.getNextRLocs(aLoc, pbParents, false)

      val recMapping = recLocs.map(getClkMappings(_, targetLoc, clkVal, pinMapping :: acc, pbParents))
      recMapping.filter(_.nonEmpty).headOption.getOrElse(Nil)
    }
  }

  def getClkXml(arch: Arch, tiles: List[RootPb]): xml.Elem = {
    val ioWidth = if (arch.place) 1 else arch.width
    val tile = tiles.filter(_.name.contains("ioTile")).head
    val pbToParent = RRG.pbToParent(tile)

    val topLocs = tile.bi.ports
      .map(_._2)
      .filter(
        bp => (bp.id.width == ioWidth) && (bp.id.pmw.pb == D) && (bp.id.pt == PTOutput)
      )

    assert(topLocs.size == 1, topLocs)

    val startLoc = getClkTargetLoc(tile, ioWidth)
    val targetLoc = PrimLoc(tile, Pin(topLocs.head.id, 0))

    val clkId = targetLoc.pin.id
    val clkVal = PortNodeID("clk", clkId, 0)

    val locMap = getClkMappings(startLoc, targetLoc, clkVal, Nil, pbToParent)
      .map(
        pm => (pm.loc, pm.prod)
      )
      .toMap

    val blockMap = Set((startLoc.pb.asInstanceOf[PrimPb].prim -> "clk"))
    val primMap = Map((startLoc.pb.name -> "clk"))
    val locToValue = Map((PbLoc(startLoc.pb.name, startLoc.pin) -> clkVal))

    val clkMol = InsertDummiesImplementedMolecule.addMissingOpenLocs(
      ImplementedMolecule(tile, locMap, None, blockMap, primMap, locToValue)
    )

    getXML(clkMol, tile, arch, 0, pbToParent)
  }

  def getXML(
      fName: String,
      mols: List[ImplementedMolecule],
      arch: Arch,
      g: ElasticGraph,
      tiles: List[RootPb]
  ): xml.Elem = {
    val clkXml = getClkXml(arch, tiles)

    val molsXml = mols
      .filter(
        mol => !isOpen(mol, mol.tile, arch)
      )
      .zipWithIndex
      .map {
        (mol, i) =>
          {
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

            val pbToParent = RRG.pbToParent(sinkRegTile)

            getXML(molWithSinkReg, sinkRegTile, arch, i + 1, pbToParent) // index 0 is the clk tile
          }
      }

    val allMols = clkXml :: molsXml

    val ins = getIns(g)
    val outs = getOuts(g)

    <block name="circuit.net" instance="FPGA_packed_netlist[0]">
    <inputs>
      {ins}
    </inputs>
    <outputs>
      {outs}
    </outputs>
    <clocks>clk</clocks>
      {allMols}
    </block>
  }

  def apply(
      params: GlobalParamsInst,
      mols: List[ImplementedMolecule],
      archs: List[Arch],
      g: ElasticGraph,
      tiles: List[RootPb]
  ): Unit = {
    archs.map {
      arch =>
        {
          val fName = params.buildDir + "/vpr/" + params.packedPref + "_" + arch.name + ".net"
          println(CYAN + "Printing : " + fName + RESET)

          val packedNetlist = getXML(fName, mols, arch, g, tiles)
          val blocks = ArchPrinter.collectBlocks(params, tiles)
          val legalized = LegalizeIO(packedNetlist, false, true, blocks, Some(g))

          val f = Util.writeOpen(fName)
          f.write(xml.PrettyPrinter(160, 4).format(legalized))
          f.close()
        }
    }
  }
}
