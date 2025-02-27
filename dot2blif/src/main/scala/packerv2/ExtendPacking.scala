package packerv2

import crkt._
import arch.PTOutput
import arch.Hs
import arch.PTInput
import archs.TEHB
import archs.OEHB
import arch.Impl
import frontend.GlobalParamsInst

import io.AnsiColor._

object ExtendPacking {
  def getBuffers(bufferedG: ElasticGraph, e: ElasticEdge): List[Node] = {
    def rec(p: Port, acc: List[Node]): List[Node] = {
      if (p == e.toPort) {
        acc.reverse
      } else {
        val dp = p.distPorts.map(_._2)
        assert(p.distPorts.size == 1)

        if (dp.head == e.toPort) {
          acc.reverse
        } else {
          val n = bufferedG(dp.head.thisNode)
          assert(n.nType.isBuf, e)

          val nP = n.ports(dp.head.id.flipped()).filter(_.loc == dp.head.loc)
          assert(nP.size == 1)

          rec(nP.head, n :: acc)
        }
      }
    }

    assert(e.fromPort.id.pt == PTOutput)

    val start = bufferedG(e.fromPort.thisNode).ports.map(_._2).flatten.filter(_ == e.fromPort)
    assert(start.size == 1)

    rec(start.head, Nil)
  }

  def orderBuffers(rrg: RRG, buffers: List[RRGPrim]): List[RRGPrim] = {
    def ordering(pn0: RRGPrim, pn1: RRGPrim): Boolean = {
      rrg.getLocalPrimSuccessors(pn0).contains(pn1)
    }

    buffers.sortWith(ordering)
  }

  // Fix discrepancies between our view and the channel view given to the buffering algorithm
  // Inserting buffer may change the width of edges in the architecture,
  // if the data was not entering the tile, or entering later for example.
  // Since we assume that any buffer that can buffer more than something 0-width has all widths, for now,
  // We know the equivalent buffer with proper width exists.
  def fixBufferWidths(
      bufferedG: ElasticGraph,
      mm: MappedMolecule[Molecule],
      buffers: List[RRGPrim],
      crktBuffers: List[TNode]
  ): List[RRGPrim] = {
    val crktWidth = crktBuffers.head.nType match {
      case TEHB(p) => p.width
      case OEHB(p) => p.width
      case other   => ???
    }

    val buffersOk = buffers.map(_.block.prim).forall {
      case TEHB(p) => p.width == crktWidth
      case OEHB(p) => p.width == crktWidth
      case other   => ???
    }

    if (buffersOk) {
      buffers
    } else {
      buffers.map {
        pn =>
          {
            pn.block.prim match {
              case TEHB(p) => {
                // TODO this should probably be a bit more precise, and only check surrounding buffers
                // TODO or, we can simply change the names more in the architecture to prevent anything to be found

                val nName = pn.name.replace(p.width.toString(), crktWidth.toString())
                // println(pn.name + " -> " + nName)

                val nCandidates = mm.m.pat.prims.filter(_.name == nName)
                if (nCandidates.size > 0) {
                  assert(nCandidates.size == 1, nCandidates.map(_.name).mkString(", ") + " for " + buffers.map(_.name))
                  nCandidates.head
                } else {
                  pn
                }
              }

              case OEHB(p) => {
                val nName = pn.name.replace(p.width.toString(), crktWidth.toString())
                // println(pn.name + " -> " + nName)

                val nCandidates = mm.m.pat.prims.filter(_.name == nName)
                if (nCandidates.size > 0) {
                  assert(nCandidates.size == 1, nCandidates.map(_.name).mkString(", ") + " for " + buffers.map(_.name))
                  nCandidates.head
                } else {
                  pn
                }
              }

              case other => ???
            }
          }
      }
    }
  }

  def withBuffers(bufferedG: ElasticGraph, mm: MappedMolecule[Molecule]): Molecule = {
    // println(mm.name)
    val nAssignments = mm.m.mapping.locAssignments
      .map {
        (pn, n) =>
          {
            n.ports.map(_._2).flatten.map {
              p =>
                {
                  assert(p.id.pmw.pb != Impl)

                  p.distPorts
                    .map(_._2)
                    .map(
                      dp => { assert(dp.id.pmw.pb != Impl); ElasticEdge(p, dp) }
                    )
                    .filter(
                      e => mm.m.isExt(e) || (e.toPort.id.pt == PTInput)
                    )
                    .filter(
                      e => e.fromPort.id.pmw.pb == Hs
                    )
                    .filter(
                      e => !bufferedG.contains(e)
                    )
                    .map {
                      e =>
                        {
                          val crktBuffers = getBuffers(bufferedG, e)
                          assert((crktBuffers.size == 2) || (crktBuffers.size == 1))

                          val reachableRREAs = mm.getRRs(e)
                          val cmuxs = reachableRREAs.map(_.dst).collect {
                            case cmux: RRCMux => cmux
                          }
                          val bypBufs = cmuxs.map(mm.m.pat.getBypassing(_)).flatten.filter(_.isBuf).toList

                          val bypassedBuffers = fixBufferWidths(bufferedG, mm, bypBufs, crktBuffers)
                          val buffersOrdered = orderBuffers(mm.m.pat, bypassedBuffers)
                          val archBuffers = fixBufferWidths(bufferedG, mm, buffersOrdered, crktBuffers)

                          // println
                          // println
                          // println(e)
                          // println(crktBuffers.map(_.name))
                          // println(cmuxs.map(_.cmuxName))
                          // println(cmuxs.map(mm.m.pat.getBypassing(_)).flatten.map(_.name))
                          // println(bypBufs.map(_.name))
                          // println(bypassedBuffers.map(_.name))
                          // println(buffersOrdered.map(_.name))
                          // println(archBuffers.map(_.name))
                          // println("--")

                          // If multiple buffers of the same type on the path, pick one at random

                          var pickedSomethingAt = 0
                          val absorbedOnEdge = crktBuffers.map {
                            n =>
                              {
                                val buf = archBuffers
                                  .drop(pickedSomethingAt)
                                  .filter(
                                    cBuf => cBuf.block.prim.canMap(n, cBuf.block.prim.p)
                                  )

                                // println(n.name + " -> " + buf.map(_.name))
                                // println(n.nType)

                                if (buf.isEmpty) {
                                  None
                                } else {
                                  pickedSomethingAt = archBuffers.indexOf(buf.head)

                                  // println("picked: " + buf.head.name + " at " + archBuffers.indexOf(buf.head))

                                  Some(LocAssignment(buf.head, n))
                                }
                              }
                          }

                          if (absorbedOnEdge.flatten.size != crktBuffers.size) {
                            Nil
                          } else {
                            absorbedOnEdge
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
      .flatten

    val m = mm.m

    Molecule(m.name, m.pat, Mapping(m.mapping.assignments ++ nAssignments), m.dagIds)
  }

  def mapNodes(m: Molecule, bufferdG: ElasticGraph): Molecule = {
    val nAssignments: Set[Assignment] = m.mapping.assignments.map {
      case LocAssignment(pn, n)     => LocAssignment(pn, bufferdG(n.name))
      case ExtAssignment(pn, value) => ??? // Only possible after the legalizer
    }

    val nMapping = Mapping(nAssignments)

    Molecule(m.name, m.pat, nMapping, m.dagIds)
  }

  def apply(
      params: GlobalParamsInst,
      bufferedG: ElasticGraph,
      packedNoBufs: List[MappedMolecule[Molecule]]
  ): List[MappedMolecule[Molecule]] = {
    println(GREEN + "Extend Molecules with Buffers" + RESET)

    packedNoBufs.map(withBuffers(bufferedG, _)).map {
      mB =>
        {
          val nM = mapNodes(mB, bufferedG)

          val callback = (r: Router) => {
            val res = nM.route(r, params, true)
            if (res.isEmpty) {
              None
            } else {
              Some(res)
            }
          }

          val assignments = Router.withFullILP(bufferedG, nM, callback, KeepMixed)
          assert(assignments.nonEmpty, nM.mapping)

          MappedMolecule(nM, assignments.get)
        }
    }
  }
}
