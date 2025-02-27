package generators

import crkt._
import archs._
import arch.BlockPortID
import arch.PTInput
import arch.PTOutput
import arch.PortMeaningWrapper
import arch.PMData
import arch.Impl
import arch.Regular
import printers.DotPrinter
import printers.DotPrinterCGRAME
import frontend.GlobalParamsInst
import frontend.GlobalParams

object SimpleChoice {
  val id32In = BlockPortID(32, PTInput, PortMeaningWrapper(PMData(None), Impl), Regular)
  val id32Out = BlockPortID(32, PTOutput, PortMeaningWrapper(PMData(None), Impl), Regular)

  def smallTile(suf: String): (List[Node], List[Port],  List[Port]) = {
    val multIn0 = Port(id32In, "", Map(), "Mult" + suf, Map(), 0)
    val multIn1 = Port(id32In, "", Map(), "Mult" + suf, Map(), 1)
    val multOut = Port(id32Out, "", Map(), "Mult" + suf, Map(), 0)

    val add0In0 = Port(id32In, "", Map(), "Add0" + suf, Map(), 0)
    val add0In1 = Port(id32In, "", Map(), "Add0" + suf, Map(), 1)
    val add0Out = Port(id32Out, "", Map(), "Add0" + suf, Map(), 0)

    val add1In0 = Port(id32In, "", Map(), "Add1" + suf, Map(), 0)
    val add1In1 = Port(id32In, "", Map(), "Add1" + suf, Map(), 1)
    val add1Out = Port(id32Out, "", Map(), "Add1" + suf, Map(), 0)

    val divIn0 = Port(id32In, "", Map(), "Div" + suf, Map(), 0)
    val divIn1 = Port(id32In, "", Map(), "Div" + suf, Map(), 1)
    val divOut = Port(id32Out, "", Map(), "Div" + suf, Map(), 0)

    multOut.distPorts = Map((add0In0.nodeID() -> add0In0))
    add0In0.distPorts = Map((multOut.nodeID() -> multOut))

    add1Out.distPorts = Map((add0In1.nodeID() -> add0In1))
    add0In1.distPorts = Map((add1Out.nodeID() -> add1Out))

    divOut.distPorts = Map((add1In0.nodeID() -> add1In0))
    add1In0.distPorts = Map((divOut.nodeID() -> divOut))

    val multPrim = Mult(MultParams(32))
    val mult = Node("Mult" + suf, multPrim, Map(), Map(), Set(), (multIn0 :: multIn1 :: multOut :: Nil).groupBy(_.id))

    val addPrim = Operator(OperatorParams(32, ALUOperation.add))
    val add0 = Node("Add0" + suf, addPrim, Map(), Map(), Set(), (add0In0 :: add0In1 :: add0Out :: Nil).groupBy(_.id))
    val add1 = Node("Add1" + suf, addPrim, Map(), Map(), Set(), (add1In0 :: add1In1 :: add1Out :: Nil).groupBy(_.id))

    val divPrim = Div(DivParams(32))
    val div0 = Node("Div" + suf, divPrim, Map(), Map(), Set(), (divIn0 :: divIn1 :: divOut :: Nil).groupBy(_.id))

    val nodes = mult :: add0 :: add1 :: div0 :: Nil
    val inPorts = multIn0 :: multIn1 :: divIn0 :: divIn1 :: add1In1 :: Nil
    val outPorts = add0Out :: Nil

    (nodes, inPorts, outPorts)
  }

  def bigTile(suf: String): (List[Node], List[Port],  List[Port]) = {
    val multIn0 = Port(id32In, "", Map(), "Mult" + suf, Map(), 0)
    val multIn1 = Port(id32In, "", Map(), "Mult" + suf, Map(), 1)
    val multOut = Port(id32Out, "", Map(), "Mult" + suf, Map(), 0)

    val add0In0 = Port(id32In, "", Map(), "Add0" + suf, Map(), 0)
    val add0In1 = Port(id32In, "", Map(), "Add0" + suf, Map(), 1)
    val add0Out = Port(id32Out, "", Map(), "Add0" + suf, Map(), 0)

    val add1In0 = Port(id32In, "", Map(), "Add1" + suf, Map(), 0)
    val add1In1 = Port(id32In, "", Map(), "Add1" + suf, Map(), 1)
    val add1Out = Port(id32Out, "", Map(), "Add1" + suf, Map(), 0)

    val add2In0 = Port(id32In, "", Map(), "Add2" + suf, Map(), 0)
    val add2In1 = Port(id32In, "", Map(), "Add2" + suf, Map(), 1)
    val add2Out = Port(id32Out, "", Map(), "Add2" + suf, Map(), 0)

    val divIn0 = Port(id32In, "", Map(), "Div" + suf, Map(), 0)
    val divIn1 = Port(id32In, "", Map(), "Div" + suf, Map(), 1)
    val divOut = Port(id32Out, "", Map(), "Div" + suf, Map(), 0)

    multOut.distPorts = Map((add1In0.nodeID() -> add1In0))
    add1In0.distPorts = Map((multOut.nodeID() -> multOut))

    divOut.distPorts = Map((add0In0.nodeID() -> add0In0))
    add0In0.distPorts = Map((divOut.nodeID() -> divOut))

    add0Out.distPorts = Map((add2In0.nodeID() -> add2In0))
    add2In0.distPorts = Map((add0Out.nodeID() -> add0Out))

    add1Out.distPorts = Map((add2In1.nodeID() -> add2In1))
    add2In1.distPorts = Map((add1Out.nodeID() -> add1Out))

    val multPrim = Mult(MultParams(32))
    val mult = Node("Mult" + suf, multPrim, Map(), Map(), Set(), (multIn0 :: multIn1 :: multOut :: Nil).groupBy(_.id))

    val addPrim = Operator(OperatorParams(32, ALUOperation.add))
    val add0 = Node("Add0" + suf, addPrim, Map(), Map(), Set(), (add0In0 :: add0In1 :: add0Out :: Nil).groupBy(_.id))
    val add1 = Node("Add1" + suf, addPrim, Map(), Map(), Set(), (add1In0 :: add1In1 :: add1Out :: Nil).groupBy(_.id))
    val add2 = Node("Add2" + suf, addPrim, Map(), Map(), Set(), (add2In0 :: add2In1 :: add2Out :: Nil).groupBy(_.id))

    val divPrim = Div(DivParams(32))
    val div0 = Node("Div" + suf, divPrim, Map(), Map(), Set(), (divIn0 :: divIn1 :: divOut :: Nil).groupBy(_.id))

    val nodes = mult :: add0 :: add1 :: add2 :: div0 :: Nil
    val inPorts = add1In1 :: multIn0 :: multIn1 :: divIn0 :: divIn1 :: add0In1 :: Nil
    val outPorts = add2Out :: Nil

    (nodes, inPorts, outPorts)
  }

  def toIo(ports: List[Port], suf: String): List[Node] = {
    ports.zipWithIndex.map {
      (p, i) => {
        p.id.pt match {
          case PTInput => {
            val name = "In" + i + suf

            val inOut = Port(id32Out, "", Map(), name, Map(), 0)

            inOut.distPorts = Map((p.nodeID() -> p))
            p.distPorts = Map((inOut.nodeID() -> inOut))

            val inPrim = Entry(EntryParams(32, Set(Impl)))
            Node(name, inPrim, Map(), Map(), Set(), (inOut :: Nil).groupBy(_.id))
          }

          case PTOutput => {
            val name = "Out" + i + suf

            val inOut = Port(id32In, "", Map(), name, Map(), 0)

            inOut.distPorts = Map((p.nodeID() -> p))
            p.distPorts = Map((inOut.nodeID() -> inOut))

            val inPrim = Exit(ExitParams(32, Set(Impl)))
            Node(name, inPrim, Map(), Map(), Set(), (inOut :: Nil).groupBy(_.id))
          }

          case other => scala.sys.error("Expected defined port direction.")
        }
      }
    }
  }

  def apply(params: GlobalParamsInst): ElasticGraph = {
    val (sNodes, sInPorts, sOutPorts) = smallTile("_s")
    val (bNodes, bInPorts, bOutPorts) = bigTile("_b")

    val sOut = sOutPorts.head
    val bIn = bInPorts.head

    sOut.distPorts = Map((bIn.nodeID() -> bIn))
    bIn.distPorts = Map((sOut.nodeID() -> sOut))

    val ios = toIo(sInPorts ++ sOutPorts.tail ++ bInPorts.tail ++ bOutPorts, "")

    val g = ElasticGraph(sNodes ++ bNodes ++ ios)

    DotPrinterCGRAME(GlobalParams.root + "/build/benchmarks/dot/bp.dot", g)

    g
  }
}

