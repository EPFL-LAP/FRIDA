package arch

// import math.min
// import scala.collection.mutable.{Map => MMap}

// This pass checks there are each word of each port is used by exaclty one link at a time

object ArchSanityChecks {
  // case class GlobalPortId(bi: BlockInterface, bp: BlockPort, at: Int) {
  //   override def toString(): String = bi.blockName + "." + bp + "[" + at + "]"
  // }

  // object GlobalPortCount {
  //   def empty = GlobalPortCount(0, 0, 0, 0, 0)
  // }

  // case class GlobalPortCount(d: Int, dVld: Int, dRdy: Int, mVld: Int, mRdy: Int) {
  //   def incD = GlobalPortCount(d+1, dVld, dRdy, mVld, mRdy)
  //   def incDvld = GlobalPortCount(d, dVld+1, dRdy, mVld, mRdy)
  //   def incDrdy = GlobalPortCount(d, dVld, dRdy+1, mVld, mRdy)
  //   def incMvld = GlobalPortCount(d, dVld, dRdy, mVld+1, mRdy)
  //   def incMrdy = GlobalPortCount(d, dVld, dRdy, mVld, mRdy+1)

  //   def count(isTopLevel: Boolean): Int = {
  //     if((dVld > 0) || (dRdy > 0) || (mVld > 0) || (mRdy > 0)) {
  //       if(isTopLevel) {
  //         assert(d==0)

  //         val inPort = ((dVld == 1) && (mVld == 1) && (mRdy == 1)) || (mVld == 1) || (mRdy == 1)
  //         val outPort = ((dRdy == 1) && (mVld == 1) && (mRdy == 1)) || (mVld == 1) || (mRdy == 1)

  //         assert(inPort || outPort)

  //         1
  //       } else {
  //         assert(d == 0)

  //         assert(((dVld == 1) && (mRdy == 1)) || ((dRdy == 1) && (mVld == 1)) || (mVld == 1) || (mRdy == 1))

  //         1
  //       }
  //     } else {
  //       d
  //     }
  //   }
  // }

  // def expandPbPorts(pb: PbType): MMap[GlobalPortId, GlobalPortCount] = {
  //   val portPairs = pb.interface.ports.map {
  //     (id, bp) => {
  //       (0 until bp.words).map {
  //         i => {
  //           (GlobalPortId(pb.interface, bp, i), GlobalPortCount.empty)
  //         }
  //       }
  //     }
  //   }.flatten.toSeq

  //   MMap().addAll(portPairs)
  // }

  // def getPortReccords(pb: PbType): MMap[GlobalPortId, GlobalPortCount] = {
  //   pb.subBlocks.map(expandPbPorts(_)).reduce(_ ++ _) ++ expandPbPorts(pb)
  // }

  // // TODO this probably became useless....
  // def reccordPortUsage(links: List[Link], pb: PbType,
  //                      portReccords: MMap[GlobalPortId, GlobalPortCount]): Unit = {
  //   links.foreach {
  //     case d @ Direct(source, sourcePort, at0, sink, sinkPort, at1, isDirectionValid, delay) => {
  //       val bound = min(sourcePort.words, sinkPort.words)

  //       (0 until bound).foreach {
  //         i => {
  //           val srcReccord = GlobalPortId(source, sourcePort, at0 + i)
  //           val dstReccord = GlobalPortId(sink, sinkPort, at1 + i)

  //           isDirectionValid match {
  //             case None => {
  //               portReccords(srcReccord) = portReccords(srcReccord).incD

  //               // println
  //               // println(portReccords.filter(_._1.bi.blockName == sink.blockName).mkString("\n"))
  //               // println(d)
  //               // println(dstReccord)

  //               portReccords(dstReccord) = portReccords(dstReccord).incD
  //             }

  //             case Some(true) => {
  //               portReccords(srcReccord) = portReccords(srcReccord).incDvld
  //               portReccords(dstReccord) = portReccords(dstReccord).incDvld
  //             }

  //             case Some(false) => {
  //               portReccords(srcReccord) = portReccords(srcReccord).incDrdy
  //               portReccords(dstReccord) = portReccords(dstReccord).incDrdy
  //             }
  //           }
  //         }
  //       }
  //     }

  //     case Mux(sources, sink, sinkPort, sinkAt, isDirectionValid, delay) => {
  //       val bound = min(sources.head._2.words, sinkPort.words)

  //       val sourcesReccords = sources.map {
  //         (bi, bp, at) => {
  //           (0 until bound).map {
  //             i => {
  //               GlobalPortId(bi, bp, at + i)
  //             }
  //           }
  //         }
  //       }.flatten

  //       val sinkReccord = (0 until bound).map(i => GlobalPortId(sink, sinkPort, sinkAt + i))

  //       if(isDirectionValid) {
  //         (sinkReccord ++ sourcesReccords).foreach {
  //           globBpId => {
  //             portReccords(globBpId) = portReccords(globBpId).incMvld
  //           }
  //         }
  //       } else {
  //         (sinkReccord ++ sourcesReccords).foreach {
  //           globBpId => {
  //             portReccords(globBpId) = portReccords(globBpId).incMrdy
  //           }
  //         }
  //       }
  //     }

  //     case CLK(source, sink) =>
  //     case Complete(sources, sinks) => ???
  //   }
  // }

  // // Check that all ports of the pb are the source or target of exaclty one interconnect link
  // // Check the same on the ports of the parents used by the Pb
  // def checkModeInterconnect(pb: PbType): Unit = {
  //   val modeInterconnects = pb.subBlocks.map(_.modeInterconnect).flatten

  //   val portReccords = getPortReccords(pb)
  //   reccordPortUsage(modeInterconnects, pb, portReccords)

  //   portReccords.foreach {
  //     case (GlobalPortId(bi, bp, at), count) => {
  //       if(bi.blockName == pb.name) {
  //         val expectedCount = pb.subBlocks.toSeq.map(_.interface.ports.toSeq)
  //           .reduce(_ ++ _)
  //           .filter((id, locBp) => (id == bp.id) && (at < locBp.words))
  //           .size

  //         def errStr = "" + GlobalPortId(bi, bp, at) + ": "
  //           + count.count(bi.blockName == pb.name) + " != " + expectedCount
  //         assert(count.count(bi.blockName == pb.name) == expectedCount, errStr)
  //       } else {
  //         assert(count.count(bi.blockName == pb.name) == 1)
  //       }
  //     }
  //   }
  // }

  // def checkAndInterconnect(pb: PbType): Unit = {
  //   val portReccords = getPortReccords(pb)
  //   reccordPortUsage(pb.interconnect, pb, portReccords)

  //   portReccords.foreach {
  //     case (GlobalPortId(bi, bp, at), count) => {
  //       println(bi.blockName + "." + bp)
  //       assert(count.count(bi.blockName == pb.name) == 1)
  //     }
  //   }
  // }

  // def checkChainInterconnect(pb: PbType): Unit = {
  //   val portReccords = getPortReccords(pb)
  //   reccordPortUsage(pb.interconnect, pb, portReccords)

  //   // println(pb.name)
  //   // println(pb)

  //   portReccords.foreach {
  //     case (GlobalPortId(bi, bp, at), count) => {
  //       // TODO add a way to properly check for multicast within the tiles
  //       assert(count.count(bi.blockName == pb.name) >= 1, "" + GlobalPortId(bi, bp, at) + " -> " + count)
  //     }
  //   }
  // }

  // // TileAnd: check that all ports of the pb are the target of one word
  // // TileAndThen: check that start of the chain exactly one
  // def checkInterconnect(pb: PbType): Unit = {
  //   pb.subBlocks.headOption.map {
  //     _.c match {
  //       case TileAnd => checkAndInterconnect(pb)
  //       case TileOr => checkModeInterconnect(pb)
  //       case TileAndThen => checkChainInterconnect(pb)
  //     }
  //   }
  // }

  def apply(pb: PbType): Unit = {
    // pb.subBlocks.map(ArchSanityChecks(_))
    // checkInterconnect(pb)

    ???
  }
}
