package arch

import scala.collection.mutable.{Map => MMap}
import math.max
import io.AnsiColor._
import core.NAPrimID
import core.AWireizeFirst
import core.AWireizeLast
import util.GID
import archs.VPRConfig
import core.AWirePort
import arch.InterfaceCombiner.canonBlockPort
import core.CompilerState
import frontend.GlobalParamsInst
import printers.ArchPrinter

object PbTypeBuilder {
  def findModeInterface(
      m: PbElem,
      childrenPbs: List[PbType],
      tileClocked: Boolean,
      isBypassed: Set[Bypass],
      literal: Boolean
  ): (BlockInterface, List[PbType]) = {
    val tc = m.modes.head.combinator

    val (modeInterface, nChildren) = if (isBypassed.nonEmpty) {
      ByPassHandler.findModeInterface(m, childrenPbs, isBypassed, literal)
    } else {
      InterfaceCombiner.findTopInterface(m, childrenPbs, tc, literal)
    }

    if (tileClocked) {
      (BlockInterface(modeInterface.ports, modeInterface.pbName, modeInterface.clocked || tileClocked), nChildren)
    } else {
      (modeInterface, nChildren)
    }
  }

  def findModeInterconnect(
      mode: PbElem,
      topLevel: BlockInterface,
      interfaces: List[BlockInterface],
      isBypassed: Set[Bypass],
      literal: Boolean
  ): List[Link] = {
    if (isBypassed.nonEmpty) {
      ByPassHandler.findModeInterconnect(mode, topLevel, interfaces, literal)
    } else {
      InterfaceLinker.findModeInterconnect(mode, topLevel, interfaces, literal)
    }
  }

  def removePortsAnnotations(ports: Map[BlockPortID, BlockPort]): Map[BlockPortID, BlockPort] = {
    ports.map {
      (id, bp) =>
        {
          (id, bp.withoutAnnotations)
        }
    }
  }

  def castPorts(
      ports: Map[BlockPortID, BlockPort],
      m: PrimMode,
      physBlock: TBlock
  ): Map[BlockPortID, (BlockPort, BlockPort)] = {
    ports.map {
      (id, bp) =>
        {
          if (m.castedPorts contains id) {
            val toId = m.castedPorts(id)
            val fromBp = physBlock.blockInterface.ports(id)
            val toBp = BlockPort(toId, fromBp.words, Set())

            (toId, (toBp, bp))
          } else {
            (id, (bp, bp))
          }
        }
    }
  }

  def instantiateMatchers(
      ports: Map[BlockPortID, (BlockPort, BlockPort)],
      pinMap: MMap[Pin, Pin],
      m: PrimMode
  ): Map[BlockPortID, BlockPort] = {
    ports
      .map {
        case (bpId, (bp, srcBp)) => {
          assert(bp.words == srcBp.words)

          (0 until bp.words).map {
            i =>
              {
                val lID = LocatedBlockPortID(bpId, i)

                val nId = m.locConfig.get(lID).fold(bpId) {
                  case Local(m) => {
                    bpId.withMatcher("loc_" + m)
                  }

                  case Global => {
                    ??? // TODO deprecated
                  }
                }

                val primPin = Pin(srcBp.id, i)

                (BlockPort(nId, 1, bp.annotations), primPin)
              }
          }
        }
      }
      .flatten
      .groupBy(_._1.id)
      .map {
        (id, bpsPrimPins) =>
          {
            val (bps, primPins) = bpsPrimPins.unzip

            primPins.toSeq.sortBy(_.loc).zipWithIndex.map {
              (primPin, loc) =>
                {
                  val pbPin = Pin(id, loc)
                  pinMap(pbPin) = primPin
                }
            }

            (id, BlockPort(id, bps.size, bps.head.annotations))
          }
      }
  }

  // Set Hs matcher ports to 0 width.
  // Update the pin map accordingly
  def canonPinMap(pinMap: Map[Pin, Pin]): Map[Pin, Pin] = {
    pinMap.map {
      (pbPin, primPin) =>
        {
          pbPin.id.pmw.pb match {
            case Hs => {
              if (pbPin.id.pmw.pm.matcher.nonEmpty) {
                (Pin(BlockPortID(0, pbPin.id.pt, pbPin.id.pmw, pbPin.id.dummy), pbPin.loc), primPin)
              } else {
                (pbPin, primPin)
              }
            }
            case other => (pbPin, primPin)
          }
        }
    }
  }

  def canonBlockInterface(bi: BlockInterface): BlockInterface = {
    val nPorts = bi.ports
      .map(_._2)
      .map {
        bp =>
          {
            bp.id.pmw.pb match {
              case Hs => {
                if (bp.id.pmw.pm.matcher.nonEmpty) {
                  (BlockPort(BlockPortID(0, bp.id.pt, bp.id.pmw, bp.id.dummy), bp.words, bp.annotations))
                } else {
                  bp
                }
              }

              case other => bp
            }
          }
      }
      .map(
        bp => (bp.id, bp)
      )
      .toMap

    BlockInterface(nPorts, bi.pbName, bi.clocked)
  }

  def instantiateBlock(
      params: GlobalParamsInst,
      primM: PrimMode,
      preGen: Boolean
  ): (BlockInterface, TBlock, Map[Pin, Pin]) = {
    val primInst = primM.prim.instantiate(params)
    val hsPrim = LowerToExplicithandshakeBlock(primInst)

    val physBlock = if (primM.annotations.contains(AWireizeFirst)) {
      RewriteBlockWithoutZeroData(WireizeBlockPass(params, hsPrim))
    } else if (primM.annotations.contains(AWireizeLast)) {
      hsPrim
    } else {
      RewriteBlockWithoutZeroData(hsPrim)
    }

    val portsWithoutAnnos = removePortsAnnotations(physBlock.blockInterface.ports)
    val nPortsCasted = castPorts(portsWithoutAnnos, primM, physBlock)

    // TODO could map this one too....
    val pinMap = MMap[Pin, Pin]()

    val matcherPorts = instantiateMatchers(nPortsCasted, pinMap, primM)

    val nBi = BlockInterface(matcherPorts, primM.name, physBlock.blockInterface.clocked)
    val cBi = canonBlockInterface(nBi)
    val cPinMap = canonPinMap(pinMap.toMap)

    (cBi, physBlock, cPinMap)
  }

  // TODO remove tileClocked
  // TODO remove literal
  def modePb(
      params: GlobalParamsInst,
      m: AbsMode,
      vprConfig: Option[VPRConfig],
      tileClocked: Boolean,
      literal: Boolean
  ): PbType = {
    val nPb = m match {
      case primM @ PrimMode(name, combinator, locConfig, annotations, castedPorts, dagId, prim) => {
        val (bi, instPrim, pinMap) = if (literal) {
          val primInst = prim.instantiate(prim.p)

          val pinMap = primInst.blockInterface.ports
            .map {
              (id, bp) =>
                {
                  (0 until bp.words).map {
                    w =>
                      {
                        (Pin(id, w) -> Pin(id, w))
                      }
                  }
                }
            }
            .flatten
            .toMap

          (prim, pinMap)

          ???
        } else {
          instantiateBlock(params, primM, true)
        }

        PrimPb(bi, Nil, Nil, primM.combinator, primM.annotations, m.name, instPrim, pinMap, primM.dagId)
      }

      case m: Mode => {
        val pbTypes = m.modes.filter(!Bypass.isModeBypass(_)).map(modePb(params, _, None, false, literal))
        val bypassInfo = Bypass.getByPass(m.modes.map(_.name))

        val (modeInterface, nPbTypes) = findModeInterface(m, pbTypes, tileClocked, bypassInfo, literal)

        if (m.modes.head.combinator == TileOr) { // interconnect inside <mode> tag)
          val pbTypesUpdated = nPbTypes.map {
            (pb: PbType) =>
              {
                val modeLinks = findModeInterconnect(m, modeInterface, List(pb.bi), bypassInfo, literal)
                val links = pb.links

                val nModePb = pb match {
                  case prim: PrimPb => {
                    PrimPb(pb.bi, links, modeLinks, pb.c, pb.annotations, pb.name, prim.prim, prim.pinMap, prim.dagIds)
                  }

                  case inter: InterPb => {
                    assert(pb.subBlocks.nonEmpty)
                    InterPb(pb.bi, links, modeLinks, pb.c, pb.annotations, pb.name, pb.subBlocks)
                  }

                  case root: RootPb => {
                    assert(pb.subBlocks.nonEmpty)
                    RootPb(pb.bi, links, modeLinks, pb.c, pb.annotations, pb.name, pb.subBlocks, root.vprConfig)
                  }
                }

                nModePb
              }
          }

          if (vprConfig.nonEmpty) {
            RootPb(modeInterface, Nil, Nil, m.combinator, m.annotations, m.name, pbTypesUpdated, vprConfig.get)
          } else {
            assert(pbTypesUpdated.nonEmpty)
            InterPb(modeInterface, Nil, Nil, m.combinator, m.annotations, m.name, pbTypesUpdated)
          }
        } else { // interconnect after the pb_types declarations
          val links = findModeInterconnect(m, modeInterface, nPbTypes.map(_.bi), bypassInfo, literal)

          if (vprConfig.nonEmpty) {
            RootPb(
              modeInterface,
              links,
              Nil,
              m.combinator,
              m.annotations,
              m.name,
              nPbTypes,
              vprConfig.get
            )
          } else {
            assert(nPbTypes.nonEmpty)
            InterPb(modeInterface, links, Nil, m.combinator, m.annotations, m.name, nPbTypes)
          }
        }
      }
    }

    // if(nPb.name == "muxMergesforkSrcCst__0") {
    //   println(nPb)
    //   println(nPb.bi)
    //   println("--")
    //   println(nPb.links.mkString("\n"))
    //   println(nPb.modeLinks.mkString("\n"))
    //   println("---")

    //   // ???
    // }

    nPb
  }

  def isPbClocked(pb: PbType): Boolean = {
    val recClocked = pb.subBlocks.exists(isPbClocked(_))

    pb match {
      case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
        prim.physicalInfo.clocked
      }

      case other => recClocked
    }
  }

  def addClockNets(root: RootPb): RootPb = {
    def rec(pb: PbType): PbType = {
      val clkLinks = pb.subBlocks.filter(isPbClocked(_)).map {
        recPb =>
          {
            CLK(pb.name, recPb.name)
          }
      }

      val recBlocks = pb.subBlocks.map(rec(_))

      pb match {
        case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
          PrimPb(bi, links ++ clkLinks, modeLinks, c, annotations, name, prim, pinMap, dagIds)
        }

        case InterPb(bi, links, modeLinks, c, annotations, name, subBlocks) => {
          InterPb(bi, links ++ clkLinks, modeLinks, c, annotations, name, recBlocks)
        }

        case RootPb(bi, links, modeLinks, c, annotations, name, subBlocks, vprConfig) => {
          RootPb(bi, links ++ clkLinks, modeLinks, c, annotations, name, recBlocks, vprConfig)
        }
      }
    }

    rec(root).asInstanceOf[RootPb]
  }

  // TODO remove literal
  def apply(params: GlobalParamsInst, tile: Tile, literal: Boolean): RootPb = {
    val tileClocked = false // tile.attrs.contains("clocked") && (tile.attrs("clocked") == "true")

    addClockNets(modePb(params, tile.asMode, Some(tile.vprConfig), tileClocked, literal).asInstanceOf[RootPb])
  }
}
