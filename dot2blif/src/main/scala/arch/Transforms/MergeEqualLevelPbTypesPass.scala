package arch

import scala.collection.mutable

// TODO deprecate this pass

object MergeEqualLevelPbTypesPass {
  // def countPbTypes(pb: PbType): Map[String, Int] = {
  //   val count = mutable.HashMap[String, Int]()

  //   pb.subBlocks.foreach {
  //     (subPb: PbType) => {
  //       val nCount = count.getOrElse(subPb.name, 0)
  //       count(subPb.name) = nCount + 1
  //     }
  //   }

  //   count.toMap
  // }

  // def mapInterface(bi: BlockInterface, seen: mutable.HashMap[String, Boolean], count: Map[String, Int],
  //                  d: Direct, source: Boolean): Option[Direct] = {
  //   val sufPort = if(source) d.sourcePort else d.sinkPort
  //   val hs = if(!sufPort.pmw.pb.isImpl() && sufPort.pmw.pb.isHandshake()) "_hs" else ""
  //   val seenKey = bi.blockName + ":" + sufPort.toString() + hs
  //   val beenSeen = seen.getOrElse(seenKey, false)
  //   if(beenSeen) {
  //     //println("dropping: " + seenKey)
  //     None
  //   } else {
  //     val nBi = bi.withNumPb(count(bi.blockName))
  //     val nD = if(source) {
  //       Direct(nBi, d.sourcePort, d.at0, d.sink, d.sinkPort, d.at1, d.isDirectionValid)
  //     } else {
  //       Direct(d.source, d.sourcePort, d.at0, nBi, d.sinkPort, d.at1, d.isDirectionValid)
  //     }

  //     seen(seenKey) = true

  //     Some(nD)
  //   }
  // }

  // def factor(pb: PbType): PbType = {
  //   if(pb.subBlocks.isEmpty) { // nothing to factor
  //     pb
  //   } else {
  //     if((pb.subBlocks.head.c == TileAnd) && (pb.subBlocks.size > 1)) {
  //       val nSubs = pb.subBlocks.map(factor(_)).groupBy(_.name).map {
  //         (pbName: String, pbs: List[PbType]) => {
  //           val nPbs = pbs.size
  //           val h = pbs.head
  //           val nBi = h.interface.withNumPb(nPbs)

  //           (h.name, PbType(h.name, nBi, h.interconnect, h.modeInterconnect, h.subBlocks, h.c, h.attrs, h.annotations, h.namedAttributes))
  //         }
  //       }.map(_._2).toList

  //       // TODO mapping modes is probably unecessary..
  //       val seen = mutable.HashMap[String, Boolean]() // TODO make me a Set
  //       val count = countPbTypes(pb)

  //       val gotClk = mutable.Set[String]()
  //       val nInterconnect = pb.interconnect.map {
  //         case d @ Direct(source, sourcePort, at0, sink, sinkPort, at1, _) => {
  //           if((count contains source.blockName) && (count(source.blockName) > 1)) {
  //             mapInterface(source, seen, count, d, true)
  //           } else if((count contains sink.blockName) && (count(sink.blockName) > 1)) {
  //             mapInterface(sink, seen, count, d, false)
  //           } else {
  //             Some(d)
  //           }
  //         }

  //         case mux: Mux => Some(mux)
  //         case clk @ CLK(source, sink) => {
  //           if(gotClk(sink.blockName)) {
  //             None
  //           } else {
  //             gotClk.addOne(sink.blockName)
  //             Some(CLK(source, sink.withNumPb(count(sink.blockName))))
  //           }
  //         }
  //         case c: Complete => ???
  //       }.filter(_.isDefined).map(_.get)

  //       val nPb = PbType(pb.name, pb.interface, nInterconnect, pb.modeInterconnect, nSubs, pb.c, pb.attrs,
  //                        pb.annotations, pb.namedAttributes)
  //       //println(nPb)
  //       nPb
  //     } else {
  //       val nSubs = pb.subBlocks.map(factor(_))
  //       PbType(pb.name, pb.interface, pb.interconnect, pb.modeInterconnect, nSubs, pb.c, pb.attrs,
  //              pb.annotations, pb.namedAttributes)
  //     }
  //   }
  // }

  def apply(physTiles: Map[String, PbType]): Map[String, PbType] = {
    // physTiles.map {
    //   (tileName: String, tile: PbType) => {
    //     val nTile = factor(tile)
    //     (tileName, nTile)
    //   }
    // }
    ???
  }
}
