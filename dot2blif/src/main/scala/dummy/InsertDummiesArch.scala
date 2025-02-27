package dummy

import arch._
import util.GID
import core.Namer
import archs.EmptyParams
import archs.DummyType
import archs.Primitive
import util.Util
import core.NAPrimID
import core.NamedAttribute
import archs.DummyTypeParams
import archs.CntrlMergeParams
import archs.ForkParams
import archs.EmptyVPRConfig
import frontend.GlobalParamsInst

object InsertDummiesArch {
  val zeroDelay = "0.0e-12"
  val lDummyS = "_lDummy"
  val rDummyS = "_rDummy"
  val wrapperS = "_wrapper"

  def updateInterconnectNaming(links: List[Link], nNames: Set[String]): List[Link] = {
    links.map {
      case d @ Direct(srcLoc, dstLoc, delay) => {
        val nSrcLoc = if (!nNames.contains(srcLoc.pbName)) {
          val nName = srcLoc.pbName + wrapperS
          PbLoc(nName, srcLoc.pin)
        } else {
          srcLoc
        }

        val nDstLoc = if (!nNames.contains(dstLoc.pbName)) {
          val nName = dstLoc.pbName + wrapperS
          PbLoc(nName, dstLoc.pin)
        } else {
          dstLoc
        }

        Direct(nSrcLoc, nDstLoc, delay)
      }

      case Mux(sources, dstLoc, delay) => {
        val nDstLoc = if (!nNames.contains(dstLoc.pbName)) {
          val nName = dstLoc.pbName + wrapperS
          PbLoc(nName, dstLoc.pin)
        } else {
          dstLoc
        }

        val nSources = sources.map {
          case srcLoc => {
            if (!nNames.contains(srcLoc.pbName)) {
              val nName = srcLoc.pbName + wrapperS
              PbLoc(nName, srcLoc.pin)
            } else {
              srcLoc
            }
          }
        }

        Mux(nSources, nDstLoc, delay)
      }

      case CLK(srcPb, dstPb) => {
        val nSource = if (!nNames.contains(srcPb)) srcPb + wrapperS else srcPb
        val nSink = if (!nNames.contains(dstPb)) dstPb + wrapperS else dstPb

        CLK(nSource, nSink)
      }
    }
  }

  def getDummyPrimName(prim: PrimPb, dir: PortType): String = {
    dir match {
      case PTInput  => prim.prim.vprName + lDummyS
      case PTOutput => prim.prim.vprName + rDummyS
      case other    => scala.sys.error("Unexpected port direction.")
    }
  }

  def getDummyName(prim: PrimPb, dir: PortType): String = {
    dir match {
      case PTInput  => prim.name + lDummyS
      case PTOutput => prim.name + rDummyS
      case other    => scala.sys.error("Unexpected port direction.")
    }
  }

  def getPinMap(bi: BlockInterface): Map[Pin, Pin] = {
    bi.ports
      .map(_._2)
      .map(_.toPins())
      .flatten
      .map {
        pin =>
          {
            (pin, pin)
          }
      }
      .toMap
  }

  def dummyPorts(physicalInfo: PhysicalInfo, dir: PortType): Set[Pin] = {
    physicalInfo.timings
      .map {
        case c @ CombTiming(source, dest, _, _) => {
          // println(c)
          dir match { // TODO should be a dummy thing
            case PTInput => {
              val src = source.toPin.id.pmw.pb match {
                case Rdy => None
                case others => {
                  if (source.id.dummy == Dummy) {
                    Some(source.toPin)
                  } else {
                    None
                  }
                }
              }

              val dst = dest.toPin.id.pmw.pb match {
                case Rdy => {
                  if (dest.id.dummy == Dummy) {
                    Some(dest.toPin)
                  } else {
                    None
                  }
                }
                case others => None
              }

              src :: dst :: Nil
            }

            case PTOutput => {
              val src = source.toPin.id.pmw.pb match {
                case Rdy => {
                  if (source.id.dummy == Dummy) {
                    Some(source.toPin)
                  } else {
                    None
                  }
                }
                case others => None
              }

              val dst = dest.toPin.id.pmw.pb match {
                case Rdy => None
                case others => {
                  if (dest.id.dummy == Dummy) {
                    Some(dest.toPin)
                  } else {
                    None
                  }
                }
              }

              src :: dst :: Nil
            }

            case other => ???
          }
        }

        case RegTiming(loc, _, _, _, _) => None
      }
      .flatten
      .flatten
      .toSet
  }

  def getExpanderPorts(prim: TBlock, dir: PortType, log: Boolean): Map[BlockPortID, BlockPort] = {
    // println(primPb.prim.physicalInfo.timings.mkString("\n"))

    val dummies = dummyPorts(prim.physicalInfo, dir)

    val expDummies = dummies.toList.map {
      pin =>
        {
          if (!prim.blockInterface.ports.contains(pin.id.toRegular)) {
            println(prim)
            println("--")
            println(pin.id.toRegular)
          }

          val bp = prim.blockInterface.ports(pin.id.toRegular)

          (bp.id.toDummy.flipped(), BlockPort(bp.id.toDummy.flipped(), bp.words, Set()))
        }
    }

    val regPorts = prim.blockInterface.ports
      .filter {
        (id, bp) =>
          {
            id.pmw.pb match {
              case Rdy   => dir.flipped == id.pt
              case other => dir == id.pt
            }
          }
      }
      .map {
        (id, bp) =>
          {
            (id, bp) :: (id.flipped(), bp.flipped) :: Nil
          }
      }
      .flatten

    (regPorts ++ expDummies).toMap
  }

  def facingDummy(id: BlockPortID, dir: PortType): Boolean = {
    id.pmw.pb match {
      case Rdy   => id.pt.flipped == dir
      case Vld   => id.pt == dir
      case D     => id.pt == dir
      case other => scala.sys.error("Unexpected port direction.")
    }
  }

  def getPhysicalInfo(prim: PrimPb, dir: PortType): PhysicalInfo = {
    val zeroDelay = "0.0e-12"

    val timings = prim.bi.ports
      .filter(
        (id, _) => facingDummy(id, dir)
      )
      .map {
        (id, bp) =>
          {
            (0 until bp.words).map {
              i =>
                {
                  val hsType = id.pmw.pb match {
                    case Vld   => Some(HSValid)
                    case Rdy   => Some(HSReady)
                    case D     => None
                    case other => ???
                  }

                  val nId = id.pmw.pb match {
                    case Vld   => BlockPortID(id.width, id.pt, PortMeaningWrapper(id.pmw.pm, Hs), id.dummy)
                    case Rdy   => BlockPortID(id.width, id.pt, PortMeaningWrapper(id.pmw.pm, Hs), id.dummy)
                    case D     => BlockPortID(id.width, id.pt, id.pmw, id.dummy)
                    case other => ???
                  }

                  val pi = PortInstance(nId, i, hsType)
                  val piFlipped = PortInstance(nId.flipped(), i, hsType)

                  val src = if (pi.id.pt == PTInput) pi else piFlipped
                  val dst = if (pi.id.pt == PTInput) piFlipped else pi

                  val forward = CombTiming(src, dst, zeroDelay, zeroDelay)

                  val locTimings = if (prim.prim.physicalInfo.containsDummy(pi.toDummy)) {
                    val sSrc = if (pi.id.pt == PTInput) pi else piFlipped.toDummy
                    val sDst = if (pi.id.pt == PTInput) piFlipped.toDummy else pi

                    val split = CombTiming(sSrc, sDst, zeroDelay, zeroDelay)

                    forward :: split :: Nil
                  } else {
                    forward :: Nil
                  }

                  locTimings
                }
            }.flatten
          }
      }
      .flatten

    assert(timings.nonEmpty)

    PhysicalInfo(0, timings.toList, Map())
  }

  def getExpander(prim: PrimPb, dir: PortType, log: Boolean): PrimPb = {
    val ports = getExpanderPorts(prim.prim, dir, log)

    val dName = getDummyPrimName(prim, dir)

    val dummyParams = DummyTypeParams(dName)
    val dummy = DummyType(dummyParams)

    val expName = getDummyName(prim, dir)

    val expBi = BlockInterface(ports, expName, false)
    val physInfo = getPhysicalInfo(prim, dir)

    val dummyBlock = Block(dummy, expBi, physInfo, Set(), Map())

    PrimPb(expBi, Nil, Nil, TileAndThen, Set(), expName, dummyBlock, getPinMap(expBi), prim.dagIds)
  }

  def getExpanders(prim: PrimPb, log: Boolean): (PrimPb, PrimPb) = {
    val lExp = getExpander(prim, PTInput, log)
    val rExp = getExpander(prim, PTOutput, log)

    (lExp, rExp)
  }

  def primWithDummyPorts(prim: PrimPb, lExp: PrimPb, rExp: PrimPb): PrimPb = {
    val lExpDummyPorts = lExp.bi.ports.toList.filter(_._1.dummy == Dummy)
    val rExpDummyPorts = rExp.bi.ports.toList.filter(_._1.dummy == Dummy)

    val dummyPorts = (lExpDummyPorts ++ rExpDummyPorts).map {
      (id, bp) =>
        {
          (id.flipped(), bp.flipped)
        }
    }

    val nBi = BlockInterface(prim.bi.ports ++ dummyPorts, prim.prim.name, prim.prim.blockInterface.clocked)
    val nBlock = Block(prim.prim.prim, nBi, prim.prim.physicalInfo, prim.prim.annotations, Map())

    PrimPb(nBi, Nil, Nil, TileAndThen, prim.annotations, prim.name, nBlock, getPinMap(nBi), prim.dagIds)
  }

  def genWrapperLinks(lDummy: PrimPb, prim: PrimPb, rDummy: PrimPb): List[Link] = {
    val wrapperName = prim.name + wrapperS

    val lLinks = lDummy.bi.ports
      .map(_._2)
      .filter {
        bp =>
          {
            bp.id.pmw.pb match {
              case Rdy   => bp.pt == PTOutput
              case other => bp.pt == PTInput
            }
          }
      }
      .map(_.toPins())
      .flatten
      .map {
        pin =>
          {
            pin.id.pmw.pb match {
              case Rdy   => Direct(PbLoc(lDummy.name, pin), PbLoc(wrapperName, pin), None)
              case other => Direct(PbLoc(wrapperName, pin), PbLoc(lDummy.name, pin), None)
            }
          }
      }

    val rLinks = rDummy.bi.ports
      .map(_._2)
      .filter {
        bp =>
          {
            bp.id.pmw.pb match {
              case Rdy   => bp.pt == PTInput
              case other => bp.pt == PTOutput
            }
          }
      }
      .map(_.toPins())
      .flatten
      .map {
        pin =>
          {
            pin.id.pmw.pb match {
              case Rdy   => Direct(PbLoc(wrapperName, pin), PbLoc(rDummy.name, pin), None)
              case other => Direct(PbLoc(rDummy.name, pin), PbLoc(wrapperName, pin), None)
            }
          }
      }

    val internalLinks = prim.bi.ports
      .map(_._2)
      .map {
        bp =>
          {
            val (srcBp, dstBp, srcName, dstName) = bp.id.pmw.pb match {
              case Rdy => {
                bp.id.pt match {
                  case PTInput => {
                    (rDummy.bi.ports(bp.id.flipped()), bp, rDummy.name, prim.name)
                  }

                  case PTOutput => {
                    (bp, lDummy.bi.ports(bp.id.flipped()), prim.name, lDummy.name)
                  }

                  case other => scala.sys.error("Unexpected Port Direction")
                }
              }

              case other => {
                bp.id.pt match {
                  case PTInput => {
                    (lDummy.bi.ports(bp.id.flipped()), bp, lDummy.name, prim.name)
                  }

                  case PTOutput => {
                    (bp, rDummy.bi.ports(bp.id.flipped()), prim.name, rDummy.name)
                  }

                  case other => scala.sys.error("Unexpected Port Direction")
                }
              }
            }

            srcBp.toPins().zip(dstBp.toPins()).map {
              (srcPin, dstPin) =>
                {
                  Direct(PbLoc(srcName, srcPin), PbLoc(dstName, dstPin), None)
                }
            }
          }
      }
      .flatten

    if (prim.prim.physicalInfo.clocked) {
      CLK(wrapperName, prim.name) :: (lLinks ++ internalLinks ++ rLinks).toList
    } else {
      (lLinks ++ internalLinks ++ rLinks).toList
    }
  }

  def genWrapper(lDummy: PrimPb, prim: PrimPb, rDummy: PrimPb, initPrim: PrimPb): InterPb = {
    val wrapperName = prim.name + wrapperS

    val inPorts = lDummy.bi.ports.filter {
      (id, bp) =>
        {
          ((id.pt == PTInput) && (id.pmw.pb != Rdy)) || ((id.pt == PTOutput) && (id.pmw.pb == Rdy))
        }
    }
    val outPorts = rDummy.bi.ports.filter {
      (id, bp) =>
        {
          ((id.pt == PTInput) && (id.pmw.pb == Rdy)) || ((id.pt == PTOutput) && (id.pmw.pb != Rdy))
        }
    }

    val nBi = BlockInterface((inPorts ++ outPorts), wrapperName, prim.bi.clocked)
    assert(nBi.ports.map(_._2).forall(_.id.dummy == Regular))

    val nLinks = genWrapperLinks(lDummy, prim, rDummy)

    InterPb(
      nBi,
      nLinks,
      Nil,
      initPrim.c,
      Set(),
      wrapperName,
      lDummy :: prim :: rDummy :: Nil
    )
  }

  def updatePb(params: GlobalParamsInst, pb: PrimPb): PbType = {
    val (lExpander, rExpander) = getExpanders(pb, false)
    val nPrimPb = primWithDummyPorts(pb, lExpander, rExpander)

    genWrapper(lExpander, nPrimPb, rExpander, pb)
  }

  def apply(params: GlobalParamsInst, pb: PbType): PbType = {
    pb match {
      case prim: PrimPb => {
        if (prim.prim.physicalInfo.extended) {
          updatePb(params, prim)
        } else {
          prim
        }
      }

      case nonLeaf: NonLeafPb => {
        val nSubBlocks = nonLeaf.subBlocks.map(InsertDummiesArch(params, _))

        val nNames = nSubBlocks.map(_.name).toSet + nonLeaf.name
        val nInterconnect = updateInterconnectNaming(pb.links, nNames)

        nonLeaf match {
          case InterPb(bi, links, modeLinks, c, annotations, name, _) => {
            InterPb(bi, nInterconnect, modeLinks, c, annotations, name, nSubBlocks)
          }

          case RootPb(bi, links, modeLinks, c, annotations, name, _, vprConfig) => {
            RootPb(bi, nInterconnect, modeLinks, c, annotations, name, nSubBlocks, vprConfig)
          }
        }
      }
    }
  }
}
