package packerv2

import frontend.GlobalParamsInst
import crkt.ElasticGraph
import crkt.PortNodeID
import crkt.ElasticEdge
import crkt.BufferConstraint
import mlir.BufDelayConstraint
import mlir.BufSlotConstraint
import arch.PTInput
import archs.Exit
import archs.TEHB
import archs.OEHB
import archs.MuxConfig
import archs.MuxConfigParams
import arch.PTOutput
import readers.MLIRParser.bufConstraint
import mlir.BufConstraint
import arch.BlockPortID
import arch.D
import arch.Hs
import arch.Bypass.bypass

// We dump constraints for valid paths only, not ready
// TODO approximate results here to elastic channels

object FindEdgeConstraints {
  val GI_HOP_COST = 0.1 // in ns

  def getConcreteWidth(id: BlockPortID): Int = {
    id.pmw.pb match {
      case D     => id.width
      case Hs    => 1 // We assume the valid direction
      case other => ???
    }
  }

  // We do not rely on the RRG here and just pick the number of tile inputs
  def getCBCost(params: GlobalParamsInst, mm: MappedMolecule[Molecule], edge: ElasticEdge): Double = {
    val isExt = mm.m.isExt(edge) && !mm.m.mapping.nodeNames.contains(edge.fromPort.thisNode)
    val isFbk = mm.isFeedback(edge)

    assert(isExt || isFbk, edge)

    val width = getConcreteWidth(edge.toPort.id)
    val usedMask = params.scArch.archs.filter(_.width == width).head
    val num = usedMask.fcIn.getFc(usedMask.chanWidth)

    val cmuxP = MuxConfigParams(width, num, 0, 0)
    MuxConfig(cmuxP).instantiate(params).physicalInfo.criticalPath
  }

  def getBaseCost(
      params: GlobalParamsInst,
      mm: MappedMolecule[Molecule],
      edge: ElasticEdge
  ): Double = {
    val reachableRREAs = mm.getRRs(edge)

    reachableRREAs
      .map(
        rrea => (rrea.src :: rrea.dst :: Nil)
      )
      .flatten
      .toSet
      .map {
        case cmux: RRCMux => {
          val width = getConcreteWidth(cmux.rrType)
          val num = mm.m.pat.preds(cmux).size

          val cmuxP = MuxConfigParams(width, num, 0, 0)
          val cp = MuxConfig(cmuxP).instantiate(params).physicalInfo.criticalPath
          assert(cp > 0)

          cp
        }

        case gi: RRGI => {
          val giCost = GI_HOP_COST * 3
          val cbCost: Double = getCBCost(params, mm, edge)

          giCost + cbCost
        }

        case other => 0
      }
      .reduceOption(_ + _)
      .getOrElse(0.0)
  }

  // Find Delays for exiting routes
  // We assume that all exit routes from a port start from the same prefix within the tile
  def outEdgeDelays(
      params: GlobalParamsInst,
      packed: List[MappedMolecule[Molecule]]
  ): Map[PortNodeID, BufDelayConstraint] = {
    packed
      .map {
        mm =>
          {
            mm.mapping.locAssignments.map {
              (pn, n) =>
                {
                  n.ports.map(_._2).flatten.filter(_._1.pt == PTOutput).map {
                    p =>
                      {
                        p.distPorts
                          .map(_._2)
                          .map(
                            dp => ElasticEdge(p, dp)
                          )
                          .filter(mm.m.isExt(_))
                          .map {
                            e =>
                              {
                                val baseCost = getBaseCost(params, mm, e)

                                (p.nodeID() -> BufDelayConstraint(0, 0, baseCost))
                              }
                          }
                      }
                  }
                }
            }
          }
      }
      .flatten
      .flatten
      .flatten
      .toMap
  }

  // Find delays for any edge going to an input of the tile
  def inEdgeDelays(
      params: GlobalParamsInst,
      packed: List[MappedMolecule[Molecule]],
      outDelays: Map[PortNodeID, BufDelayConstraint]
  ): Map[PortNodeID, BufDelayConstraint] = {
    packed
      .map {
        mm =>
          {
            mm.mapping.locAssignments.map {
              (pn, n) =>
                {
                  n.ports.map(_._2).flatten.filter(_._1.pt == PTInput).map {
                    p =>
                      {
                        p.distPorts.map(_._2).map {
                          dp =>
                            {
                              val e = ElasticEdge(dp, p)
                              val baseCost = getBaseCost(params, mm, e)

                              val entryCost = if (mm.m.isExt(e)) {
                                val giCost = GI_HOP_COST * 3 // TODO keep 2 here or something else?
                                val cbCost = getCBCost(params, mm, e)
                                val otherTileCost = outDelays(dp.nodeID()).delay

                                giCost + cbCost + otherTileCost
                              } else {
                                0
                              }

                              val totalCost = baseCost + entryCost
                              (p.nodeID() -> BufDelayConstraint(0, 0, totalCost))
                            }
                        }
                      }
                  }
                }
            }
          }
      }
      .flatten
      .flatten
      .flatten
      .toMap
  }

  // Special logic for entries and exits
  def findEdgeDelays(
      params: GlobalParamsInst,
      packed: List[MappedMolecule[Molecule]]
  ): Map[PortNodeID, BufDelayConstraint] = {
    val outDelays = outEdgeDelays(params, packed)
    inEdgeDelays(params, packed, outDelays)
  }

  def findExternalConstraints(
      params: GlobalParamsInst,
      packed: List[MappedMolecule[Molecule]]
  ): Map[PortNodeID, BufSlotConstraint] = {
    packed
      .map {
        mm =>
          {
            mm.mapping.locAssignments.map {
              (pn, n) =>
                {
                  n.ports.map(_._2).flatten.filter(_._1.pt == PTInput).map {
                    p =>
                      {
                        p.distPorts
                          .map(_._2)
                          .map(
                            dp => ElasticEdge(dp, p)
                          )
                          .filter(mm.m.isExt(_))
                          .map {
                            e =>
                              {
                                (p.nodeID() -> BufSlotConstraint(0, Integer.MAX_VALUE, 0, Integer.MAX_VALUE))
                              }
                          }
                      }
                  }
                }
            }
          }
      }
      .flatten
      .flatten
      .flatten
      .toMap
  }

  def findInternalConstraint(
      params: GlobalParamsInst,
      packed: List[MappedMolecule[Molecule]]
  ): Map[PortNodeID, BufSlotConstraint] = {
    packed
      .map {
        mm =>
          {
            mm.mapping.locAssignments.map {
              (_, n) =>
                {
                  n.ports.map(_._2).flatten.filter(_._1.pt == PTInput).map {
                    p =>
                      {
                        p.distPorts
                          .map(_._2)
                          .map(
                            dp => ElasticEdge(dp, p)
                          )
                          .filter(!mm.m.isExt(_))
                          .map {
                            e =>
                              {
                                val reachableRREAs = mm.getRRs(e)

                                val cmuxs = reachableRREAs.map(_.dst).collect {
                                  case cmux: RRCMux => cmux
                                }
                                val bypassedPrims =
                                  cmuxs.map(mm.m.pat.getBypassing(_)).flatten.filter(_.isBuf).map(_.block.prim)

                                // TODO If there are multiple buffers on the path of the same type, we just pick the biggest for now
                                val tehbs = bypassedPrims.collect {
                                  case tehb: TEHB => tehb
                                }
                                val oehbs = bypassedPrims.collect {
                                  case oehb: OEHB => oehb
                                }

                                if ((tehbs.size > 1) || (oehbs.size > 1)) {
                                  println("Ignoring some buffer slots on the path.")
                                }

                                val maxT = tehbs
                                  .map(_.p.depth)
                                  .reduceOption(math.max)
                                  .fold(0)(
                                    d => d
                                  )
                                val maxO = oehbs
                                  .map(_.p.depth)
                                  .reduceOption(math.max)
                                  .fold(0)(
                                    d => d
                                  )

                                (p.nodeID() -> BufSlotConstraint(0, maxT, 0, maxO))
                              }
                          }
                      }
                  }
                }
            }
          }
      }
      .flatten
      .flatten
      .flatten
      .toMap
  }

  // On each route, check potential bypassed buffers and record their properties
  // Feedback edges or crossing edges have unlimited buffer space
  // Internal edges only have a buffer if there is a bypassed one in the architecture
  // This assumes that on multiple bitwidth routes, buffers have the same property
  // TODO, we could try to make this more generic in the future....
  def findEdgeConstraints(
      params: GlobalParamsInst,
      packed: List[MappedMolecule[Molecule]]
  ): Map[PortNodeID, BufSlotConstraint] = {
    val extConstraints = findExternalConstraints(params, packed)
    val intConstraints = findInternalConstraint(params, packed)

    extConstraints ++ intConstraints
  }

  def apply(params: GlobalParamsInst, g: ElasticGraph, packed: List[MappedMolecule[Molecule]]): ElasticGraph = {
    val delayConstraints = findEdgeDelays(params, packed)
    val slotConstraints = findEdgeConstraints(params, packed)

    val bufConstraints = delayConstraints.map {
      (target, dc) =>
        {
          val sc = slotConstraints(target)

          (target -> BufferConstraint(sc, dc))
        }
    }.toMap

    ElasticGraph(g.nodes, bufConstraints)
  }
}
