package arch

import core.AWireizeLast
import core.AWireizeFirst
import arch.Direct

type CastKey = (Int, PortMeaning, PortType) | (PortMeaning, PortType)

// TODO need to refactor this pass

object CastPb {
  // def getKey(bp: BlockPortID, withWidth: Boolean): CastKey = {
  //   if(withWidth) {
  //     (bp.width, bp.pmw.pm.canonicalize(), bp.pt)
  //   } else {
  //     (bp.pmw.pm.canonicalize(), bp.pt)
  //   }
  // }

  // def getWithWidth(m: Mode): Boolean = m.annotations contains AWireizeFirst

  // def getLegalisationPort(m: Mode, withWidth: Boolean): Map[CastKey, CastKey] = {
  //   m.castedPorts.map {
  //     (fromId, toId) => {
  //       (toId, fromId)
  //     }
  //   }.map {
  //     (from, to) => {
  //       val fromKey = getKey(from, withWidth)
  //       val toKey = getKey(to, withWidth)

  //       (fromKey, toKey)
  //     }
  //   }
  // }

  // def mapInterfacePorts(ports: Map[BlockPortID, BlockPort], m: Mode,
  //                       legalMeanings: Map[CastKey, CastKey]): Map[BlockPortID, BlockPort] = {
  //   ports.map {
  //     (id, bp) => {
  //       val (nWidth, nPm) = legalMeanings.get(getKey(id, getWithWidth(m))) match {
  //         case Some((pm: PortMeaning, pt: PortType)) => (id.width, id.pmw.pm.matcher.fold(pm)(m => pm.withMatcher(m)))
  //         case Some((width: Int, pm: PortMeaning, pt: PortType)) => (width, id.pmw.pm.matcher.fold(pm)(m => pm.withMatcher(m)))
  //         case None => (id.width, id.pmw.pm)
  //       }

  //       val nPmw = PortMeaningWrapper(nPm, bp.id.pmw.pb)
  //       val nId = BlockPortID(nWidth, bp.id.pt, nPmw, bp.id.dummy)
  //       val nBp = BlockPort(nId, bp.words, bp.annotations, bp.locMapping)

  //       (nId, nBp)
  //     }
  //   }
  // }

  // def getCanonWidthPm(bp: BlockPort, m: Mode, legalMeanings: Map[CastKey, CastKey]): (Int, PortMeaning) = {
  //   val (nWidth, nPm) = legalMeanings.get(getKey(bp.id, getWithWidth(m))) match {
  //     case Some((pm: PortMeaning, pt: PortType)) => (bp.id.width, pm)
  //     case Some((width: Int, pm: PortMeaning, pt: PortType)) => (width, pm)
  //     case None => (bp.id.width, bp.id.pmw.pm)
  //   }

  //   val nPmM = bp.id.pmw.pm.matcher.fold(nPm) {
  //     m => {
  //       nPm.withMatcher(m)
  //     }
  //   }

  //   (nWidth, nPmM)
  // }

  // def getCanonInterface(bi: BlockInterface, bp: BlockPort, m: Mode,
  //                       legalMeanings: Map[CastKey, CastKey]): (BlockInterface, BlockPort) = {
  //   val (nWidth, nPm) = getCanonWidthPm(bp, m, legalMeanings)

  //   val nSinkPmw = PortMeaningWrapper(nPm, bp.id.pmw.pb)
  //   val nSinkId = BlockPortID(nWidth, bp.id.pt, nSinkPmw, bp.id.dummy)
  //   val nSinkPort = BlockPort(nSinkId, bp.words, bp.annotations, bp.locMapping)

  //   val nSinkPorts = mapInterfacePorts(bi.ports, m, legalMeanings)
  //   val nSink = BlockInterface(nSinkPorts, bi.blockName, bi.clocked)

  //   (nSink, nSinkPort)
  // }

  // def updateInterconnect(links: List[Link], m: Mode): List[Link] = {
  //   val legalMeanings = getLegalisationPort(m, getWithWidth(m))

  //   links.map {
  //     case d @ Direct(srcLoc, dstLoc, delay) => {
  //       if(sink.blockName == m.name) {
  //         val (nSink, nSinkPort) = getCanonInterface(sink, sinkPort, m, legalMeanings)

  //         Direct(source, sourcePort, at0, nSink, nSinkPort, at1, delay)
  //       } else if(source.blockName == m.name) {
  //         val (nSource, nSourcePort) = getCanonInterface(source, sourcePort, m, legalMeanings)

  //         Direct(nSource, nSourcePort, at0, sink, sinkPort, at1, delay)
  //       } else {
  //         d
  //       }
  //     }

  //     case mux @ Mux(sources, sink, sinkPort, sinkAt, delay) => {
  //       if(sink.blockName == m.name) {
  //         val (nSink, nSinkPort) = getCanonInterface(sink, sinkPort, m, legalMeanings)

  //         Mux(sources, nSink, nSinkPort, sinkAt, delay)
  //       } else {
  //         val nSources = sources.map {
  //           (bi, bp, at) => {
  //             if(bi.blockName == m.name) {
  //               val (nSource, nSourcePort) = getCanonInterface(bi, bp, m, legalMeanings)

  //               (nSource, nSourcePort, at)
  //             } else {
  //               (bi, bp, at)
  //             }
  //           }
  //         }

  //         Mux(nSources, sink, sinkPort, sinkAt, delay)
  //       }
  //     }

  //     case other => other
  //   }
  // }

  def apply(tPb: PbType, t: Tile, hsBlocks: Map[String, TBlock]): PbType = {
    ???
    // def rec(pb: PbType, m: Mode): (PbType, Option[Mode]) = {
    //   if(pb.subBlocks.isEmpty) {
    //     if(m.castedPorts.nonEmpty) {
    //       val legalMeanings = getLegalisationPort(m, getWithWidth(m))
    //       val nPorts = mapInterfacePorts(pb.interface.ports, m, legalMeanings)

    //       assert(pb.interconnect.isEmpty)

    //       val nBi = BlockInterface(nPorts, pb.name, pb.interface.clocked)
    //       val nInterconnect = updateInterconnect(pb.modeInterconnect, m)

    //       val nPb = PbType(pb.name, nBi, Nil, nInterconnect, Nil, pb.c, pb.vprConfig,
    //                        pb.annotations, pb.namedAttributes)

    //       (nPb, Some(m))
    //     } else {
    //       (pb, None)
    //     }
    //   } else {
    //     val subMapped = pb.subBlocks.map {
    //       recPb => {
    //         val recM = m.modes.filter(_.name == recPb.name).head
    //         rec(recPb, recM)
    //       }
    //     }

    //     val recBlocks = subMapped.map(_._2).flatten

    //     val nInterconnect = recBlocks.foldLeft(pb.interconnect) {
    //       (prevLinks, mode) => {
    //         updateInterconnect(prevLinks, mode)
    //       }
    //     }

    //     val nPb = PbType(pb.name, pb.interface, nInterconnect, pb.modeInterconnect, subMapped.map(_._1), pb.c,
    //                      pb.vprConfig, pb.annotations, pb.namedAttributes)

    //     (nPb, None)
    //   }
    // }

    // val m = Mode(t.name, t.modes, TileAnd, Map(), Set(), Map(), Map(), None)
    // rec(tPb, m)._1
  }
}
