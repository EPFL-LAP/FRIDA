package packerv2

import crkt.PortNodeID
import crkt.LowerToExplicitVldRdy
import arch._
import crkt.ElasticGraph

object ImplementedMoleculeToExplicitVldRdy {
  import dummy.InsertDummiesImplementedMolecule.addMissingOpenLocs

  def mapValue(value: PortNodeID): List[PortNodeID] = {
    value.pId.pmw.pb match {
      case D => value :: Nil
      case Hs => {
        val (vId, rId) = LowerToExplicitVldRdy.convertId(value.pId)

        PortNodeID(value.nodeName, vId, value.loc) :: PortNodeID(value.nodeName, rId, value.loc) :: Nil
      }

      case other => scala.sys.error("Unexpected port bundle.")
    }

  }

  def mapLink(loc: PbLoc, srcLoc: PbLoc, link: Link): List[Link] = {
    val nLinks = link match {
      case Direct(srcLoc, dstLoc, delay) => LowerExplicitValidReady.mapLinks(link :: Nil)
      case Mux(sources, dstLoc, delay) => {
        dstLoc.pin.id.pmw.pb match {
          case D => LowerExplicitValidReady.mapLinks(link :: Nil)
          case Hs => {
            val nLinks = LowerExplicitValidReady.mapLinks(link :: Nil)

            val (vldLinks, rdyLinks) = nLinks.partition {
              case Direct(srcLoc, dstLoc, _) => dstLoc.pin.id.pmw.pb == Vld
              case Mux(sources, dstLoc, _)   => dstLoc.pin.id.pmw.pb == Vld
              case other                     => scala.sys.error("Unexpected link.")
            }

            val srcLocRdy = LowerExplicitValidReady.mapLoc(srcLoc).filter(_._2.id.pmw.pb == Rdy)
            val locRdy = LowerExplicitValidReady.mapLoc(loc).filter(_._2.id.pmw.pb == Rdy)

            assert(srcLocRdy.size == 1)
            assert(locRdy.size == 1)

            val keptRdyLinks = rdyLinks
              .collect {
                case d: Direct => d
              }
              .filter {
                d =>
                  {
                    (((d.srcLoc == srcLocRdy.head) && (d.dstLoc == locRdy.head))
                    || ((d.srcLoc == locRdy.head) && (d.dstLoc == srcLocRdy.head)))
                  }
              }

            assert(keptRdyLinks.size == 1, keptRdyLinks.mkString("\n"))
            assert(vldLinks.size == 1, vldLinks.mkString("\n"))

            vldLinks ++ keptRdyLinks
          }

          case other => scala.sys.error("Unexpected Port Bundle.")
        }
      }

      case other => scala.sys.error("Unexpected link.")
    }

    nLinks
  }

  def mapLoc(pbLoc: PbLoc): List[PbLoc] = {
    LowerExplicitValidReady.mapLoc(pbLoc).toList
  }

  def findRdySource(hsValue: PortNodeID, g: ElasticGraph): PortNodeID = {
    val hsP = g(hsValue.nodeName).getPort(hsValue)
    assert(hsP.distPorts.size == 1)

    val hsDPval = hsP.distPorts.head._2.nodeID()

    val rdyId = BlockPortID(
      hsDPval.pId.width,
      hsDPval.pId.pt.flipped,
      PortMeaningWrapper(hsDPval.pId.pmw.pm, Rdy),
      hsDPval.pId.dummy
    )

    PortNodeID(hsDPval.nodeName, rdyId, hsDPval.loc)
  }

  def mapProducer(hsG: ElasticGraph, loc: PbLoc, prod: Producer): List[Producer] = {
    val nLocs = mapLoc(prod.loc)

    prod match {
      case lp: LinkProducer => {
        mapLink(loc, lp.loc, lp.link).map {
          nLink =>
            {
              val nPb = nLink match {
                case Direct(srcLoc, dstLoc, delay) => srcLoc.pin.id.pmw.pb
                case Mux(sources, dstLoc, delay) => {
                  assert(dstLoc.pin.id.pmw.pb != Rdy)
                  dstLoc.pin.id.pmw.pb
                }
                case clk @ CLK(srcPb, dstPb) => ???
              }

              val nLoc = nLocs.filter(_.pin.id.pmw.pb == nPb)
              assert(nLoc.size == 1)

              LinkProducer(nLoc.head, nLink)
            }
        }
      }

      case sp: SourceProducer => {
        mapValue(sp.value).map {
          nValue =>
            {
              val nPb = nValue.pId.pmw.pb
              val nLoc = nLocs.filter(_.pin.id.pmw.pb == nPb)
              assert(nLoc.size == 1)

              nPb match {
                case Rdy => {
                  val rdyValue = findRdySource(sp.value, hsG)
                  SourceProducer(nLoc.head, rdyValue)
                }

                case other => {
                  SourceProducer(nLoc.head, nValue)
                }
              }
            }
        }
      }
    }
  }

  def mapLocMap(hsG: ElasticGraph, locMap: Map[PbLoc, Option[Producer]]): Map[PbLoc, Option[Producer]] = {
    locMap
      .map {
        (loc, prodOpt) =>
          {
            prodOpt match {
              case None => {
                mapLoc(loc).map(
                  nLoc => (nLoc -> None)
                )
              }

              case Some(prod) => {
                val nProds = mapProducer(hsG, loc, prod)

                mapLoc(loc).map {
                  nLoc =>
                    {
                      val nPb = nLoc.pin.id.pmw.pb
                      val nProd = nProds.filter(_.loc.pin.id.pmw.pb == nPb)

                      assert(nProd.size == 1)

                      (nLoc -> Some(nProd.head))
                    }
                }
              }
            }
          }
      }
      .flatten
      .toMap
  }

  def findTarget(locMap: Map[PbLoc, Option[Producer]], srcProd: SourceProducer): PbLoc = {
    val prodToTarget = locMap
      .filter(_._2.isDefined)
      .map(
        (k, v) => (k, v.get)
      )
      .filter(!_._2.isInstanceOf[SourceProducer])
      .map {
        (loc, prod) =>
          {
            prod match {
              case LinkProducer(srcLoc, link) => (srcLoc, loc)
              case other                      => ???
            }
          }
      }
      .toMap

    def rec(loc: PbLoc): PbLoc = {
      if (!prodToTarget.contains(loc)) {
        loc
      } else {
        rec(prodToTarget(loc))
      }
    }

    rec(srcProd.loc)
  }

  def updateProducer(locMap: Map[PbLoc, Option[Producer]], pinMap: PinMapping): PinMapping = {
    val loc = pinMap.loc

    pinMap.prod match {
      case None => pinMap
      case Some(SourceProducer(srcLoc, value)) => {
        val target = findTarget(locMap, SourceProducer(srcLoc, value))
        PinMapping(target, Some(SourceProducer(target, value))) // Value has already been updated
      }

      case Some(LinkProducer(srcLoc, link)) => {
        PinMapping(srcLoc, Some(LinkProducer(loc, link)))
      }
    }
  }

  def fixupRdyDirection(locMap: Map[PbLoc, Option[Producer]]): Map[PbLoc, Option[Producer]] = {
    val (rdyLocs, unchangedLocs) = locMap.partition(_._1.pin.id.pmw.pb == Rdy)
    val nRdyLocs = rdyLocs
      .map(
        (loc, prod) => updateProducer(rdyLocs, PinMapping(loc, prod))
      )
      .map(
        pinMap => (pinMap.loc, pinMap.prod)
      )

    (nRdyLocs ++ unchangedLocs).toMap
  }

  def rdyLinkMap(rootPb: RootPb): Map[Direct, Mux] = {
    def rec(pb: PbType): List[(Direct, Mux)] = {
      assert(pb.modeLinks.isEmpty)

      val recMap = pb.subBlocks.map(rec(_)).flatten

      val locMap = pb.links
        .collect {
          case mux: Mux => mux
        }
        .filter(_.dstLoc.pin.id.pmw.pb == Rdy)
        .map {
          case mux @ Mux(srcLocs, dstLoc, _) => {
            srcLocs.map {
              srcLoc =>
                {
                  (Direct(srcLoc, dstLoc, None) -> mux)
                }
            }
          }
        }
        .flatten

      (recMap ++ locMap).toList
    }

    rec(rootPb).toMap
  }

  def insertRdyMuxs(
      locMap: Map[PbLoc, Option[Producer]],
      nTile: RootPb
  ): Map[PbLoc, Option[Producer]] = {
    val linkMap = rdyLinkMap(nTile)

    locMap.map {
      (loc, prod) =>
        {
          loc.pin.id.pmw.pb match {
            case Rdy => {
              prod match {
                case Some(LinkProducer(srcLoc, link)) => {
                  link match {
                    case d: Direct => {
                      if (linkMap.contains(d)) {
                        (loc, Some(LinkProducer(srcLoc, linkMap(d))))
                      } else {
                        (loc, prod)
                      }
                    }

                    case other => scala.sys.error("unexpected link: " + other)
                  }
                }

                case other => (loc, prod)
              }
            }

            case other => (loc, prod)
          }
        }
    }
  }

  def lowerBlock(b: TBlock): TBlock = {
    LowerExplicitValidReady(b)
  }

  def lowerLocToValue(locToValue: Map[PbLoc, PortNodeID]): Map[PbLoc, PortNodeID] = {
    locToValue
      .map {
        (loc, value) =>
          {
            val nLocs = mapLoc(loc)
            val nValues = mapValue(value)

            nLocs.map {
              nLoc =>
                {
                  val valCandidates = nValues.filter(_.pId.pmw.pb == nLoc.pin.id.pmw.pb)
                  assert(valCandidates.size == 1)

                  (nLoc, valCandidates.head)
                }
            }
          }
      }
      .flatten
      .toMap
  }

  def apply(m: ImplementedMolecule, hsG: ElasticGraph): ImplementedMolecule = {
    val nTile = LowerExplicitValidReady(m.tile)

    val initLocs = mapLocMap(hsG, m.locMap)
    val rightDir = fixupRdyDirection(initLocs)
    val validLocs = insertRdyMuxs(rightDir, nTile)

    val nMapping = m.blockMap.map(
      (b, nName) => (lowerBlock(b), nName)
    )
    val nLocToValue = lowerLocToValue(m.locToValue)

    val nMol = addMissingOpenLocs(ImplementedMolecule(nTile, validLocs, None, nMapping, m.primMap, nLocToValue))
    assert(ImplementMolecule.allLocsExist(nMol))

    nMol
  }
}
