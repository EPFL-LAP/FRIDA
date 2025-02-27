package readers

import frontend.GlobalParamsInst
import scala.io.Source
import crkt._
import arch._
import packerv2.Molecule
import collection.mutable.{Map => MMap}

case class GlobalInterconnectDelayInfo(srcPort: PortNodeID, dstPort: PortNodeID, delay: Double) {
  override def toString(): String = {
    "" + srcPort + " -> " + dstPort + ": " + delay
  }
}

// What is this doing ??

object ParseFinalRoutingDelays {
  // def isPortPrimitive(line: String): Boolean = {
  //   (line contains "[0]") && !(line.contains("Startpoint"))
  //   && !(line.contains("Endpoint")) & !(line.contains("clk"))
  // }

  // def isIO(line: String): Boolean = line.contains(".input") || line.contains(".output")

  // def dummyNameToNodeName(nodeName: String): String = {
  //   if (nodeName contains "d_return0") {
  //     "d_return0"
  //   } else if (nodeName contains "control_merge") {
  //     nodeName.split("_CntrlMerge")(0)
  //   } else {
  //     val dummySuffix = nodeName.indexOf('_')
  //     nodeName.replace(nodeName.slice(dummySuffix, nodeName.size), "")
  //   }
  // }

  // def idStrToId(idStr: String): BlockPortID = {
  //   val onlyIdStr = idStr.split("_")(0)

  //   val dum = if (idStr contains "Reg") Regular else Dummy

  //   val pm = if (idStr contains "PMData") {
  //     PMData(None)
  //   } else if (idStr contains "PMCond") {
  //     PMCond(None)
  //   } else if (idStr contains "PMAddr") {
  //     PMAddr(None)
  //   } else {
  //     ???
  //   }

  //   val pt = if (idStr contains "In") {
  //     PTInput
  //   } else {
  //     PTOutput
  //   }

  //   val width = if (onlyIdStr contains "32") {
  //     32
  //   } else if (onlyIdStr contains "1") {
  //     1
  //   } else {
  //     0
  //   }

  //   val pb = if (idStr contains HSReady.str) {
  //     Hs
  //   } else if (idStr contains HSValid.str) {
  //     Hs
  //   } else {
  //     D
  //   }

  //   BlockPortID(width, pt, PortMeaningWrapper(pm, pb), dum)
  // }

  // def getLoc(nodeName: String, ppId: BlockPortID, pLoc: Int, packedg: List[Molecule]): Int = {
  //   var loc: Option[Int] = None

  //   packedg.foreach {
  //     crktTile =>
  //       {
  //         crktTile.mols.foreach {
  //           mol =>
  //             {
  //               if (mol.s.nToPn.contains(nodeName)) {
  //                 val pn = mol.s.nToPn(nodeName)
  //                 val pp = pn.edges.ports.map(_._1).filter(_.id == ppId).filter(_.loc == pLoc).head

  //                 if (!mol.s.portMappings.contains(pp)) {
  //                   println(mol.s.portMappings.filter(_._1.pNodeName == pn.name).mkString("\n"))
  //                   println(nodeName + "." + ppId + "[" + pLoc + "]")
  //                 }

  //                 val p = mol.s.portMappings(pp)

  //                 assert(loc.isEmpty)

  //                 loc = Some(p.loc)
  //               }
  //             }
  //         }
  //       }
  //   }

  //   loc.get
  // }

  // def parsePort(line: String, packedg: List[CrktTile]): PortNodeID = {
  //   if (isIO(line)) {
  //     val nName = line.split("__")(0)
  //     val port = 0
  //     val portID = idStrToId(line.split("__")(1).split('.')(0))

  //     val dummyPrim = nName contains "/"

  //     val nodeName = if (dummyPrim) {
  //       dummyNameToNodeName(nName)
  //     } else {
  //       nName
  //     }

  //     PortNodeID(nodeName.replace(".", ""), portID, port)
  //   } else {
  //     val nName = line.split('.')(0)
  //     val port = line.split('.')(1).split(" ")(0).replace("[0]", "")
  //     val prim = line.split('.')(1).split(" ")(1).replace("(", "")

  //     val dummyPrim = prim contains "Dummy"

  //     val ppLoc = port.split("_")(1).replace("w", "").toInt
  //     val portID = idStrToId(port)

  //     val nodeName = if (dummyPrim) {
  //       dummyNameToNodeName(nName)
  //     } else {
  //       nName
  //     }

  //     val portLoc = if (dummyPrim) {
  //       if (prim contains "Dirl") {
  //         val dummyId = BlockPortID(portID.width, PTInput, portID.pmw, Regular)
  //         getLoc(nodeName, dummyId, ppLoc, packedg)
  //       } else {
  //         val dummyId = BlockPortID(portID.width, PTOutput, portID.pmw, Regular)
  //         getLoc(nodeName, dummyId, ppLoc, packedg)
  //       }
  //     } else {
  //       getLoc(nodeName, portID, ppLoc, packedg)
  //     }

  //     PortNodeID(nodeName.replace(".", ""), portID, portLoc)
  //   }
  // }

  // def dummyToReg(id: PortNodeID): PortNodeID = {
  //   PortNodeID(id.nodeName, BlockPortID(id.pId.width, id.pId.pt, id.pId.pmw, Regular), id.loc)
  // }

  // // TODO refactor for new packer
  // def apply(
  //     params: GlobalParamsInst,
  //     crkt: ElasticGraph,
  //     packedg: List[Molecule]
  // ): List[GlobalInterconnectDelayInfo] = {
  //   val fName = params.buildDir + "/report_timing.setup.rpt"

  //   var startPort: Option[PortNodeID] = None
  //   var globalInterconnectDelay: Double = 0.0
  //   var seenChan = false

  //   val globalInterconnectInfo = MMap[(PortNodeID, PortNodeID), GlobalInterconnectDelayInfo]()

  //   Source.fromFile(fName).getLines().foreach {
  //     line =>
  //       {
  //         // println(line)
  //         if (isPortPrimitive(line)) {
  //           if (startPort.isEmpty || (startPort.nonEmpty && (globalInterconnectDelay == 0.0))) {
  //             startPort = Some(parsePort(line, packedg))
  //             globalInterconnectDelay = 0
  //             seenChan = false
  //           } else if (startPort.nonEmpty && (globalInterconnectDelay != 0.0)) {
  //             if (!seenChan) {
  //               globalInterconnectDelay = 0
  //               seenChan = false
  //               startPort = Some(parsePort(line, packedg))
  //             } else {
  //               val endPort = parsePort(line, packedg)

  //               val srcPort = dummyToReg(startPort.get)
  //               val sinkPort = dummyToReg(endPort)

  //               val delay = globalInterconnectInfo.get((srcPort, sinkPort)).fold(0)(_.delay)

  //               val delayInfo = if (srcPort.pId.pt == PTOutput) {
  //                 GlobalInterconnectDelayInfo(srcPort, sinkPort, globalInterconnectDelay)
  //               } else {
  //                 GlobalInterconnectDelayInfo(sinkPort, srcPort, globalInterconnectDelay)
  //               }

  //               globalInterconnectInfo((delayInfo.srcPort, delayInfo.dstPort)) = delayInfo

  //               startPort = None
  //               seenChan = false
  //               globalInterconnectDelay = 0
  //             }
  //           }
  //         } else if (line.replaceAll("\\s+", "").nonEmpty && line.replaceAll("\\s+", "").charAt(0) == '|') {
  //           if (line.contains("CHANX") || line.contains("CHANY")) {
  //             seenChan = true

  //             val sLine = line.split("\\s+")
  //             val delay = sLine(sLine.size - 2).toDouble

  //             globalInterconnectDelay += delay
  //           } else if (line.contains("intra")) {
  //             val sLine = line.split("\\s+")
  //             val delay = sLine(sLine.size - 2).toDouble

  //             globalInterconnectDelay += delay
  //           }
  //         }
  //       }
  //   }

  //   val delays = globalInterconnectInfo.toMap.map(_._2)

  //   val finalDelays = delays
  //     .filter {
  //       globDelay =>
  //         {
  //           !((globDelay.srcPort.nodeName contains "end") && (globDelay.dstPort.nodeName contains "end"))
  //         }
  //     }
  //     .map {
  //       globDelay =>
  //         {
  //           if (globDelay.dstPort.nodeName contains "out:") {
  //             val srcNode = crkt(globDelay.srcPort.nodeName)
  //             val srcPort = srcNode.ports(globDelay.srcPort.pId).filter(_.loc == globDelay.srcPort.loc).head

  //             val dstPortTmp = srcPort.distPorts.head._2

  //             val dstPort = if (dstPortTmp.thisNode contains srcNode.name) {
  //               val dummyNode = crkt(dstPortTmp.thisNode)
  //               val dummyPort = dummyNode.ports(globDelay.srcPort.pId).filter(_.loc == globDelay.srcPort.loc).head

  //               dummyPort.distPorts.head._2
  //             } else {
  //               dstPortTmp
  //             }

  //             val nDelay = GlobalInterconnectDelayInfo(globDelay.srcPort, dstPort.nodeID(), globDelay.delay)
  //             nDelay
  //           } else if (globDelay.srcPort.nodeName contains "out:") {
  //             val dstNode = crkt(globDelay.dstPort.nodeName)
  //             val dstPort = dstNode.ports(globDelay.dstPort.pId).filter(_.loc == globDelay.dstPort.loc).head

  //             val srcPortTmp = dstPort.distPorts.head._2

  //             val srcPort = if (srcPortTmp.thisNode contains dstNode.name) {
  //               val dummyNode = crkt(srcPortTmp.thisNode)
  //               val dummyPort = dummyNode.ports(globDelay.srcPort.pId).filter(_.loc == globDelay.srcPort.loc).head

  //               dummyPort.distPorts.head._2
  //             } else {
  //               srcPortTmp
  //             }

  //             val nDelay = GlobalInterconnectDelayInfo(srcPort.nodeID(), globDelay.dstPort, globDelay.delay)
  //             nDelay
  //           } else {
  //             globDelay
  //           }
  //         }
  //     }

  //   finalDelays.toList
  // }

  def apply() = ???
}
