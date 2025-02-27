package dummy

import frontend.GlobalParamsInst
import crkt._
import arch._
import archs.DummyTypeParams
import archs.DummyType
import printers.ArchPrinter
import printers.BlifPrinter
import packerv2.ImplementedMolecule

object InsertDummiesCrkt {
  def needsDummy(params: GlobalParamsInst, n: TNode, mol: ImplementedMolecule): Boolean = {
    mol.getPrim(n).physicalInfo.extended
  }

  def getExpander(params: GlobalParamsInst, n: Node, dir: PortType, mol: ImplementedMolecule, log: Boolean): Node = {
    val nName = dir match {
      case PTInput  => n.name + InsertDummiesArch.lDummyS
      case PTOutput => n.name + InsertDummiesArch.rDummyS
      case other    => scala.sys.error("expected defined direction.")
    }

    val physInfo = mol.getPrim(n).physicalInfo

    val bps = n.ports.map {
      (id, ports) =>
        {
          (id, BlockPort(id, ports.size, Set()))
        }
    }.toMap

    val block = Block(n.nType, BlockInterface(bps, "", false), physInfo, Set(), Map())

    val nPorts = InsertDummiesArch
      .getExpanderPorts(block, dir, log)
      .map {
        (id, bp) =>
          {
            bp.toPins().map {
              pin =>
                {
                  Port(id, "", Map(), nName, Map(), pin.loc)
                }
            }
          }
      }
      .flatten
      .toList
      .groupBy(_.id)

    val dummyParams = DummyTypeParams(nName)
    val dummy = DummyType(dummyParams)

    Node(nName, dummy, Map(), Map(), Set(), nPorts)
  }

  def getExpanders(params: GlobalParamsInst, n: Node, mol: ImplementedMolecule, log: Boolean): (Node, Node) = {
    val lExp = getExpander(params, n, PTInput, mol, log)
    val rExp = getExpander(params, n, PTOutput, mol, log)

    (lExp, rExp)
  }

  def nWithDummyPorts(n: Node, lExp: Node, rExp: Node): Node = {
    val lExpDummyPorts = lExp.ports.toList.filter(_._1.dummy == Dummy)
    val rExpDummyPorts = rExp.ports.toList.filter(_._1.dummy == Dummy)

    val dummyPorts = (lExpDummyPorts ++ rExpDummyPorts).map(_._2).flatten.map {
      p =>
        {
          Port(p.id.flipped(), "", Map(), n.name, Map(), p.loc)
        }
    }

    val nPorts = (n.ports.map(_._2).flatten ++ dummyPorts).toList.groupBy(_.id)

    Node(n.name, n.nType, n.mlirAttr, n.attr, n.annos, nPorts)
  }

  def rewireInternalLinks(p: Port, lExp: Node, rExp: Node): Unit = {
    p.id.pt match {
      case PTInput => {
        val exp = p.id.pmw.pb match {
          case Rdy   => rExp
          case other => lExp
        }

        val srcPort = exp.getPin(Pin(p.id.flipped(), p.loc))

        p.distPorts = Map((srcPort.nodeID() -> srcPort))
        srcPort.distPorts = Map((p.nodeID() -> p))
      }

      case PTOutput => {
        val exp = p.id.pmw.pb match {
          case Rdy   => lExp
          case other => rExp
        }

        val dstPort = exp.getPin(Pin(p.id.flipped(), p.loc))

        p.distPorts = Map((dstPort.nodeID() -> dstPort))
        dstPort.distPorts = Map((p.nodeID() -> p))
      }

      case other => scala.sys.error("Expected defined port direction.")
    }
  }

  def rewireExternalLinks(p: Port, lExp: Node, rExp: Node): Unit = {
    val exp = p.id.pt match {
      case PTInput => {
        p.id.pmw.pb match {
          case Rdy   => rExp
          case other => lExp
        }
      }

      case PTOutput => {
        p.id.pmw.pb match {
          case Rdy   => lExp
          case other => rExp
        }
      }

      case other => scala.sys.error("Expected defined port direction.")
    }

    val dstPort = exp.getPin(Pin(p.id, p.loc))

    dstPort.distPorts = p.distPorts

    p.distPorts.map(_._2).map {
      dp =>
        {
          dp.distPorts = dp.distPorts.map {
            (_, ddp) =>
              {
                if (ddp.nodeID() == p.nodeID()) {
                  (dstPort.nodeID() -> dstPort)
                } else {
                  (ddp.nodeID() -> ddp)
                }
              }
          }
        }
    }
  }

  def rewire(n: Node, lExp: Node, rExp: Node): List[Node] = {
    n.ports.map(_._2).flatten.map {
      p =>
        {
          p.id.dummy match {
            case Regular => {
              rewireExternalLinks(p, lExp, rExp)
              rewireInternalLinks(p, lExp, rExp)
            }

            case Dummy => {
              rewireInternalLinks(p, lExp, rExp)
            }
          }
        }
    }

    n :: lExp :: rExp :: Nil
  }

  def rewriteWithExpanders(params: GlobalParamsInst, n: Node, mol: ImplementedMolecule): List[Node] = {
    val (lExp, rExp) = getExpanders(params, n, mol, false)
    val nNode = nWithDummyPorts(n, lExp, rExp)
    rewire(nNode, lExp, rExp)
  }

  def apply(params: GlobalParamsInst, g: ElasticGraph, mols: List[ImplementedMolecule]): ElasticGraph = {
    val node2mol = BlifPrinter.getNodeToMol(mols)

    val nNodes = g.nodes
      .map(_._2)
      .map {
        n =>
          {
            if (needsDummy(params, n, node2mol(n.name))) {
              rewriteWithExpanders(params, n, node2mol(n.name))
            } else {
              n :: Nil
            }
          }
      }
      .flatten

    ElasticGraph(
      nNodes
        .map(
          n => (n.name, n)
        )
        .toMap,
      g.properties
    )
  }
}
