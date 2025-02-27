package crkt

object RewriteTileOperators {
  def apply() = ???
}

// import arch._
// import core.PatternRewriter

// case class OperatorTile(inCst0: Option[Node], inCst1: Option[Node], op: Option[Node], cst: Option[Node],
//                         fork: Node, outBufs: List[Node]) {

//   val N_OUTS = 8
//   val N_F_OUTS = 8

//   def createPortToTypeMapping(): Map[PortNodeID, (PortMeaning, Int)] = {
//     val cst0Map: List[(PortNodeID, (PortMeaning, Int))] = inCst0.map {
//       (n: Node) => {
//         val id = PortNodeID(n.name, BlockPortID(0, PTInput, PortMeaningWrapper(PMData, false, true), false), 0)
//         (id, (PMBPConst, 0))
//       }
//     }.fold(Nil)(_ :: Nil)

//     // println("cst0: " + cst0Map.mkString("\n"))

//     val cst1Map: List[(PortNodeID, (PortMeaning, Int))] = inCst0.map {
//       (n: Node) => {
//         val id = PortNodeID(n.name, BlockPortID(0, PTInput, PortMeaningWrapper(PMData, false, true), false), 0)
//         (id, (PMBPConst, 0))
//       }
//     }.fold(Nil)(_ :: Nil)

//     // println("cst1: " + cst1Map.mkString("\n"))

//     val opMap = if(op.isDefined) {
//       val id0 = PortNodeID(op.get.name, BlockPortID(32, PTInput, PortMeaningWrapper(PMData, false, true), false), 0)
//       val id1 = PortNodeID(op.get.name, BlockPortID(32, PTInput, PortMeaningWrapper(PMData, false, true), false), 1)
//       val id2 = PortNodeID(op.get.name, BlockPortID(32, PTInput, PortMeaningWrapper(PMData, false, false), false), 0)
//       val id3 = PortNodeID(op.get.name, BlockPortID(32, PTInput, PortMeaningWrapper(PMData, false, false), false), 1)

//       val id4 = PortNodeID(op.get.name, BlockPortID(32, PTOutput, PortMeaningWrapper(PMData, false, false), false), 0)

//       (id0, (PMMult, 0)) :: (id1, (PMMult, 1)) :: (id2, (PMMult, 0)) :: (id3, (PMMult, 1)) :: (id4, (PMMult, 0)) :: Nil
//     } else {
//       val id0 = PortNodeID(op.get.name, BlockPortID(0, PTInput, PortMeaningWrapper(PMData, false, true), false), 0)
//       val id1 = PortNodeID(op.get.name, BlockPortID(32, PTOutput, PortMeaningWrapper(PMData, false, false), false), 0)

//       (id0, (PMMult, 0)) :: (id1, (PMMult, 1)) :: Nil
//     }

//     val forksMap = PartialCrossbar.allocateForks(List(fork), N_OUTS, N_F_OUTS)

//     val outData = outBufs match {
//       case Nil => Nil
//       case buf0 :: Nil =>
//         (PortNodeID(buf0.name, BlockPortID(32, PTOutput, PortMeaningWrapper(PMData, false, false), false), 0), (PMMult, 0)) :: Nil
//       case buf0 :: buf1 :: Nil =>
//         (PortNodeID(buf1.name, BlockPortID(32, PTOutput, PortMeaningWrapper(PMData, false, false), false), 0), (PMMult, 0)) :: Nil
//       case _ => scala.sys.error("Unexpected match")
//     }

//     (cst0Map ++ cst1Map ++ opMap ++ forksMap ++ outData).toMap
//   }

//   def ports(g: ElasticGraph): List[Port] = {
//     if(op.isDefined) {
//       val inDataL = if(inCst0.isDefined) {
//         inCst0.get.inPorts().values.flatten
//       } else {
//         op.get.inPorts().values.flatten.filter(_.loc == 0)
//       }

//       val inDataR = if(inCst1.isDefined) {
//         inCst1.get.inPorts().values.flatten
//       } else {
//         op.get.inPorts().values.flatten.filter(_.loc == 1)
//       }

//       val outData = outBufs match {
//         case Nil => op.get.outPorts().values.flatten.filter(p => p.pmw.implicitHandshake || !p.pmw.isHandshake)
//         case buf0 :: Nil => buf0.outPorts().values.flatten.filter(p => p.pmw.implicitHandshake || !p.pmw.isHandshake)
//         case buf0 :: buf1 :: Nil => buf1.outPorts().values.flatten.filter(p => p.pmw.implicitHandshake || !p.pmw.isHandshake)
//         case _ => scala.sys.error("Unexpected match")
//       }

//       val outHandshakes = fork.outPorts().values.flatten

//       (inDataL ++ inDataR ++ outData ++ outHandshakes).toList
//     } else {
//       assert(cst.isDefined)
//       val inData = cst.get.inPorts().values.flatten
//       val outData = cst.get.outPorts().values.flatten.filter(p => p.pmw.implicitHandshake || !p.pmw.isHandshake)
//       val outHandshakes = fork.outPorts().values.flatten

//       (inData ++ outData ++ outHandshakes).toList
//     }
//   }

//   def nodeToDangling(n: Node, nNode: Node): Unit = {
//     val inCstName0 = inCst0.fold(Set())(n => Set(n.name))
//     val inCstName1 = inCst1.fold(Set())(n => Set(n.name))
//     val forkName = Set(fork.name)
//     val cstName = cst.fold(Set())(n => Set(n.name))
//     val opName = op.fold(Set())(n => Set(n.name))
//     val outBufsName = outBufs.map(_.name).toSet

//     val allNames = inCstName0 ++ inCstName1 ++ forkName ++ cstName ++ opName ++ outBufsName

//     n.ports.values.flatten.foreach {
//       (p: Port) => {
//         p.distPorts.foreach {
//           (dpID, dp) => {
//             if(!allNames.contains(dp.thisNode) && (dp.thisNode != nNode.name)) {
//               dp.distPorts.map {
//                 (dpdpID, dpdp) => {
//                   if(dpdpID == p.nodeID()) {
//                     val nP = p.withNodeName(nNode.name)
//                     (nP.nodeID(), nP)
//                   } else {
//                     (dpdpID, dpdp)
//                   }
//                 }
//               }
//             } else {
//               dp.distPorts = Map()
//             }
//           }
//         }

//         p.distPorts = Map()
//       }
//     }
//   }

//   def tileToDangling(nNode: Node): Unit = {
//     inCst0.foreach(nodeToDangling(_, nNode))
//     inCst1.foreach(nodeToDangling(_, nNode))
//     nodeToDangling(fork, nNode)
//     cst.foreach(nodeToDangling(_, nNode))
//     op.foreach(nodeToDangling(_, nNode))
//     outBufs.foreach(nodeToDangling(_, nNode))
//   }

//   def portsToProperType(ports: List[Port]): List[Port] = {
//     val portsMapping = createPortToTypeMapping()
//     // println("--")
//     // println(portsMapping.mkString("\n"))

//     val nPorts = ports.map {
//       (p: Port) => {
//         // println(p)
//         val nPmw = PortMeaningWrapper(portsMapping(p.nodeID())._1, p.pmw.implicitHandshake, p.pmw.isHandshake)
//         val nId = BlockPortID(p.width, p.pt, nPmw, p.dummy)
//         val nP = Port(nId, p.name, p.attr, p.thisNode, p.distPorts, portsMapping(p.nodeID())._2, p.lsq_arg)
//         Port.updateDistPorts(p, nP)
//         nP
//       }
//     }

//     // println(nPorts.mkString("\n"))

//     nPorts
//   }

//   def makeTile(g: ElasticGraph): Node = {
//     val attrs = Map[String, String] (
//       "type" -> "OperatorTile",
//       "in" -> "",
//       "out" -> "",
//       "shape" -> "oval",
//       "color" -> "white"
//     )

//     val nName = op.fold(cst.get.name)(_.name) + "_opTile"
//     val nPortsTemp = ports(g)

//     val properPorts = portsToProperType(nPortsTemp)

//     properPorts.foreach {
//       (p: Port) => {
//         p.distPorts.foreach {
//           (_, dp) => {
//             if(p.thisNode == dp.thisNode) {
//               println("" + p + " -> " + dp)
//             }
//           }
//         }
//       }
//     }

//     val nPorts = properPorts.map(_.withNodeName(nName)).map(p => p.withName(p.id.toString())).groupBy(_.id)
//     val nNode = Node(nName, attrs, nPorts)

//     tileToDangling(nNode)

//     nNode
//   }
// }

// object RewriteTileOperators extends PatternRewriter {
//   import RewriteTileComparators._

//   def pMatch(n: Node, blocks: Map[String, Block], g: ElasticGraph): Boolean = {
//     val isOp = (n.nodeType() == "Operator")
//     val isMult = (n.nodeType() == "Mult")

//     lazy val linkedFork = getARemoteOutPort(n).fold(false)(p => getNode(p, g).fold(false)(_.nodeType() == "CFork"))
//     val isConstantToFork = (n.nodeType() == "Constant") && linkedFork

//     isOp || isMult || isConstantToFork
//   }

//   def rewrite(root: Node, g: ElasticGraph, blocks: Map[String, Block]): List[Node] = {
//     assert(pMatch(root, blocks, g))

//     root.nodeType() match {
//       case "Constant" => {
//         val outBuf0 = linkedBuffers(root, g).headOption
//         val outBuf1 = outBuf0.fold(None)(linkedBuffers(_, g).headOption)
//         val outBufs = (outBuf0 :: outBuf1 :: Nil).filter(_.isDefined).map(_.get)

//         val outFork = outBufs match {
//           case Nil => getOutputFork(root, g).get
//           case buf0 :: Nil => getOutputFork(buf0, g).get
//           case buf0 :: buf1 :: Nil => getOutputFork(outBuf1.get, g).get
//           case _ => scala.sys.error("Unexpected match")
//         }

//         OperatorTile(None, None, None, Some(root), outFork, outBufs).makeTile(g) :: Nil
//       }

//       case "Mult" => {
//         val (op0Const, op1Const) = getConstantProducers(root, g)

//         val outBuf0 = linkedBuffers(root, g).headOption
//         val outBuf1 = outBuf0.fold(None)(linkedBuffers(_, g).headOption)
//         val outBufs = (outBuf0 :: outBuf1 :: Nil).filter(_.isDefined).map(_.get)

//         val outFork = outBufs match {
//           case Nil => getOutputFork(root, g).get
//           case buf0 :: Nil => getOutputFork(buf0, g).get
//           case buf0 :: buf1 :: Nil => getOutputFork(outBuf1.get, g).get
//           case _ => scala.sys.error("Unexpected match")
//         }

//         OperatorTile(op0Const, op1Const, Some(root), None, outFork, outBufs).makeTile(g) :: Nil
//       }

//       case "Operator" => {
//         // println(root.name)

//         val (op0Const, op1Const) = getConstantProducers(root, g)

//         val outBuf0 = linkedBuffers(root, g).headOption
//         val outBuf1 = outBuf0.fold(None)(linkedBuffers(_, g).headOption)
//         val outBufs = (outBuf0 :: outBuf1 :: Nil).filter(_.isDefined).map(_.get)

//         val outFork = outBufs match {
//           case Nil => getOutputFork(root, g).get
//           case buf0 :: Nil => getOutputFork(buf0, g).get
//           case buf0 :: buf1 :: Nil => getOutputFork(outBuf1.get, g).get
//           case _ => scala.sys.error("Unexpected match")
//         }

//         OperatorTile(op0Const, op1Const, Some(root), None, outFork, outBufs).makeTile(g) :: Nil
//       }
//     }
//   }
// }
