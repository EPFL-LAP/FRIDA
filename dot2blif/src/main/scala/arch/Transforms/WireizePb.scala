package arch

import core.AWireizeLast
import core.AAllowPartialUsage
import core.NAPrimID
import util.ArchWalker
import util.GID
import core.AWirePort
import core.CompilerState
import frontend.GlobalParamsInst

import io.AnsiColor._

// TODO Define a trait for this pass and similar ones

case class WireId(pbName: String, width: Int, pmw: PortMeaningWrapper, dummy: DummyType) {
  override def toString(): String = {
    pbName + ".("
      + BLUE + width + RESET + ", "
      + BLUE + pmw.pm + RESET + "(" + BLUE + pmw.pm.matcher + RESET + "), "
      + BLUE + pmw.pb + RESET + ", "
      + BLUE + dummy + RESET + ")"
  }
}

object WireizePb {
  def zeroDataId(id: BlockPortID): Boolean = (id.width == 0) && (id.pmw.pb == D)

  def removeZeroDataBi(bi: BlockInterface): BlockInterface = {
    val nPorts = bi.ports.filter {
      (id, _) =>
        {
          !zeroDataId(id)
        }
    }

    BlockInterface(nPorts, bi.pbName, bi.clocked)
  }

  def removeZeroDataLinks(links: List[Link]): List[Link] = {
    links.filter {
      case Direct(srcLoc, dstLoc, delay) => !(zeroDataId(srcLoc.pin.id) || zeroDataId(dstLoc.pin.id))
      case Mux(sources, dstLoc, delay) => {
        !(sources.exists(
          srcLoc => zeroDataId(srcLoc.pin.id)
        ) || zeroDataId(dstLoc.pin.id))
      }
      case others => true
    }
  }

  def removeZeroData(rootPb: RootPb): RootPb = {
    def rec(pb: PbType): PbType = {
      val nBi = removeZeroDataBi(pb.bi)
      val nLinks = removeZeroDataLinks(pb.links)
      val nModeLinks = removeZeroDataLinks(pb.modeLinks)
      val nSubBlocks = pb.subBlocks.map(rec(_))

      val nPb = pb match {
        case RootPb(bi, links, modeLinks, c, annotations, name, subBlocks, vprConfig) => {
          RootPb(nBi, nLinks, nModeLinks, c, annotations, name, nSubBlocks, vprConfig)
        }

        case InterPb(bi, links, modeLinks, c, annotations, name, subBlocks) => {
          InterPb(nBi, nLinks, nModeLinks, c, annotations, name, nSubBlocks)
        }

        case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
          PrimPb(nBi, nLinks, nModeLinks, c, annotations, name, prim, pinMap, dagIds)
        }
      }

      // if(nPb.name == "CMergeCMF__") {
      //   println(nPb)
      //   println(nPb.bi)
      //   println("--------")
      //   println(nPb.links.mkString("\n"))
      //   println("--")
      //   ???
      // }

      nPb
    }

    rec(rootPb).asInstanceOf[RootPb]
  }

  def getCanonIds(primPb: PrimPb): Map[BlockPortID, BlockPortID] = {
    primPb.bi.ports.map {
      (id, bp) =>
        {
          (id, InterfaceCombiner.canonBlockPort(bp, false).id)
        }
    }
  }

  def getLocMap(pb: PbType): Map[PbLoc, PbLoc] = {
    pb match {
      case primPb: PrimPb => {
        val canonIds = getCanonIds(primPb)

        primPb.bi.ports
          .map {
            (id, bp) =>
              {
                if (canonIds.contains(id)) {
                  (0 until bp.words).map {
                    w =>
                      {
                        (PbLoc(primPb.name, Pin(id, w)) -> PbLoc(primPb.name, Pin(canonIds(id), w)))
                      }
                  }
                } else {
                  Nil
                }
              }
          }
          .flatten
          .toMap
      }

      case other => Map()
    }
  }

  def mapLinks(links: List[Link], locMap: Map[PbLoc, PbLoc]): List[Link] = {
    links.map {
      case Direct(srcLoc, dstLoc, delay) => {
        val nSrcLoc = locMap
          .get(srcLoc)
          .fold(srcLoc)(
            l => l
          )
        val nDstLoc = locMap
          .get(dstLoc)
          .fold(dstLoc)(
            l => l
          )

        Direct(nSrcLoc, nDstLoc, delay)
      }

      case other => other
    }
  }

  def canonPrim(primPb: PrimPb): PrimPb = {
    val canonIds = getCanonIds(primPb)
    val nPorts = primPb.bi.ports.map {
      (id, bp) =>
        {
          if (canonIds.contains(id)) {
            val nId = canonIds(id)
            val nBp = BlockPort(nId, bp.words, bp.annotations)

            (nId, nBp)
          } else {
            (id, bp)
          }
        }
    }

    val nBi = BlockInterface(nPorts, primPb.bi.pbName, primPb.bi.clocked)

    val nPinMap = primPb.pinMap.map {
      (pbPin, primPin) =>
        {
          if (canonIds.contains(pbPin.id)) {
            val nPin = Pin(canonIds(pbPin.id), pbPin.loc)

            (nPin, primPin)
          } else {
            (pbPin, primPin)
          }
        }
    }

    val nModeLinks = mapLinks(primPb.modeLinks, getLocMap(primPb))

    PrimPb(nBi, Nil, nModeLinks, primPb.c, primPb.annotations, primPb.name, primPb.prim, nPinMap, primPb.dagIds)
  }

  // Is only useful to canonicalize the primtives with AWireLast that were not canonicalized before
  def canonPb(rootPb: RootPb): RootPb = {
    def rec(pb: PbType): PbType = {
      val recPbs = pb.subBlocks.map(rec(_))
      val recPbLocMap = pb.subBlocks.map(getLocMap(_)).flatten.toMap

      pb match {
        case primPb: PrimPb => {
          val nPrim = canonPrim(primPb)

          // if(primPb.name == "brcidcstforkFork__00"){
          //   println(primPb.bi)
          //   println("--")
          //   println(nPrim.bi)
          // }

          nPrim
        }

        case interPb: InterPb => {
          val nLinks = mapLinks(interPb.links, recPbLocMap)
          InterPb(interPb.bi, nLinks, interPb.modeLinks, interPb.c, interPb.annotations, interPb.name, recPbs)
        }

        case rPb: RootPb => {
          val nLinks = mapLinks(rPb.links, recPbLocMap)
          RootPb(rPb.bi, nLinks, rPb.modeLinks, rPb.c, rPb.annotations, rPb.name, recPbs, rPb.vprConfig)
        }
      }
    }

    rec(rootPb).asInstanceOf[RootPb]
  }

  def getWireIds(primPb: PrimPb): Set[WireId] = {
    val primWireIds = primPb.prim.blockInterface.ports.filter(_._2.annotations.contains(AWirePort)).map(_._1).toSet
    val revPinMap = primPb.pinMap
      .map(
        (k, v) => (v.id, k.id)
      )
      .toMap

    primWireIds
      .map(
        id => revPinMap(id)
      )
      .map {
        primId =>
          {
            primPb.bi.ports
              .filter(
                (id, _) => id.canonicalize() == primId
              )
              .filter(!_._1.pmw.pb.isHandshake())
              .map {
                (instId, _) =>
                  {
                    WireId(primPb.name, instId.width, instId.pmw.canonicalize(), instId.dummy)
                  }
              }
          }
      }
      .flatten
  }

  def getWireId(pbLoc: PbLoc): WireId = {
    WireId(pbLoc.pbName, pbLoc.pin.id.width, pbLoc.pin.id.pmw.canonicalize(), pbLoc.pin.id.dummy)
  }

  def getWireId(pbName: String, id: BlockPortID): WireId = {
    WireId(pbName, id.width, id.pmw.canonicalize(), id.dummy)
  }

  def getLinks(links: List[Link], wireIds: Set[WireId]): (List[Link], List[Link]) = {
    links.partition {
      case Direct(srcPb, dstPb, _) => {
        val srcWire = getWireId(srcPb)
        val dstWire = getWireId(dstPb)

        wireIds.contains(srcWire) || wireIds.contains(dstWire)
      }

      case others => false
    }
  }

  def mapDataLinks(dWireLinks: List[Direct], dWireIds: Set[WireId], log: Boolean): List[Direct] = {
    dWireLinks
      .filter(
        d => !dWireIds.contains(getWireId(d.srcLoc))
      )
      .map {
        srcD =>
          {
            val targetLinks = dWireLinks
              .collect {
                case d: Direct => d
              }
              .filter(
                d => getWireId(d.srcLoc) == getWireId(srcD.dstLoc)
              )

            // if(log) {
            //   println
            //   println("src: " + srcD)
            //   println("to: " + targetLinks.mkString("\n"))
            // }

            targetLinks.map {
              dstD =>
                {
                  assert(srcD.delay.isEmpty && dstD.delay.isEmpty)
                  Direct(srcD.srcLoc, dstD.dstLoc, None)
                }
            }
          }
      }
      .flatten
  }

  def mapHsLinks(hsWireLinks: List[Direct], hsWireIds: Set[WireId], log: Boolean): List[Direct] = {
    hsWireLinks.map {
      d =>
        {
          val nSrc = if (hsWireIds.contains(getWireId(d.srcLoc))) {
            val nId = BlockPortID(0, d.srcLoc.pin.id.pt, d.srcLoc.pin.id.pmw, d.srcLoc.pin.id.dummy)
            PbLoc(d.srcLoc.pbName, Pin(nId, d.srcLoc.pin.loc))
          } else {
            d.srcLoc
          }

          val nDst = if (hsWireIds.contains(getWireId(d.dstLoc))) {
            val nId = BlockPortID(0, d.dstLoc.pin.id.pt, d.dstLoc.pin.id.pmw, d.dstLoc.pin.id.dummy)
            PbLoc(d.dstLoc.pbName, Pin(nId, d.dstLoc.pin.loc))
          } else {
            d.dstLoc
          }

          assert(d.delay.isEmpty)
          val nD = Direct(nSrc, nDst, None)

          // if(log) {
          //   println
          //   println("src: " + d)
          //   println("to: " + nD)
          // }

          nD
        }
    }
  }

  def mapLinks(links: List[Link], wireIds: Set[WireId], log: Boolean): List[Link] = {
    if (wireIds.isEmpty) {
      links
    } else {
      val dWire = wireIds
      val hsWire = wireIds.map(
        wId => WireId(wId.pbName, wId.width, PortMeaningWrapper(wId.pmw.pm, Hs), wId.dummy)
      )

      val (dWireLinks, otherLinks) = getLinks(links, dWire)
      val nDWireLinks = mapDataLinks(
        dWireLinks.collect {
          case d: Direct => d
        },
        dWire,
        log
      )

      val (hsWireLinks, untouchedLinks) = getLinks(otherLinks, hsWire)
      val nHsWireLinks = mapHsLinks(
        hsWireLinks.collect {
          case d: Direct => d
        },
        hsWire,
        log
      )

      val nLinks = nDWireLinks ++ nHsWireLinks ++ untouchedLinks

      // if(log) {
      //   println(wireIds)
      //   println(links.mkString("\n"))
      //   println("--")
      //   println(nLinks.mkString("\n"))
      // }

      nLinks
    }
  }

  def mapPinMap(primPb: PrimPb, wireIds: Set[WireId]): Map[Pin, Pin] = {
    val dWire = wireIds
    val hsWire = wireIds.map(
      wId => WireId(wId.pbName, wId.width, PortMeaningWrapper(wId.pmw.pm, Hs), wId.dummy)
    )

    val toCanonIds = primPb.bi.ports
      .filter(
        (id, _) => hsWire.contains(getWireId(primPb.name, id))
      )
      .filter(_._1.pmw.pb.isHandshake())
      .map(_._1)
      .toSet

    primPb.pinMap
      .filter(
        (pbPin, _) => !dWire.contains(getWireId(primPb.name, pbPin.id))
      )
      .map {
        (pbPin, primPin) =>
          {
            if (toCanonIds.contains(pbPin.id)) {
              val nPbId = BlockPortID(0, pbPin.id.pt, pbPin.id.pmw, pbPin.id.dummy)
              val nPbPin = Pin(nPbId, pbPin.loc)

              val nPrimId = BlockPortID(0, primPin.id.pt, primPin.id.pmw, primPin.id.dummy)
              val nPrimPin = Pin(nPrimId, primPin.loc)

              (nPbPin, nPrimPin)
            } else {
              (pbPin, primPin)
            }
          }
      }
  }

  def convertPorts(primPb: PrimPb, ports: List[BlockPort]): List[BlockPort] = {
    ports.map {
      bp =>
        {
          if (primPb.prim.blockInterface.ports(bp.id).annotations.contains(AWirePort) && bp.pmw.pb.isHandshake()) {
            val nId = BlockPortID(0, bp.id.pt, bp.id.pmw, bp.id.dummy)
            BlockPort(nId, bp.words, bp.annotations)
          } else {
            bp
          }
        }
    }
  }

  def mapInterface(primPb: PrimPb, wireIds: Set[WireId]): BlockInterface = {
    val nonWirePorts = primPb.bi.ports
      .filter {
        (id, _) => !wireIds.contains(getWireId(primPb.bi.pbName, id))
      }
      .map(_._2)
      .toList

    val nPorts = convertPorts(primPb, nonWirePorts)
      .map(
        bp => (bp.id, bp)
      )
      .toMap

    BlockInterface(nPorts, primPb.bi.pbName, primPb.bi.clocked)
  }

  def lowerPrim(params: GlobalParamsInst, block: TBlock): TBlock = {
    val wB = WireizeBlockPass(params, block)
    val zB = RewriteBlockWithoutZeroData(wB)

    zB // .canonicalize()
  }

  def mapPrimitive(params: GlobalParamsInst, primPb: PrimPb): PrimPb = {
    assert(primPb.links.isEmpty)

    val wireIds = getWireIds(primPb)
    val nPrim = lowerPrim(params, primPb.prim)

    val nBi = mapInterface(primPb, wireIds)
    val nPinMap = mapPinMap(primPb, wireIds)

    val nModeLinks = mapLinks(primPb.modeLinks, wireIds, false)

    // if(primPb.name == "brcidcstforkFork__00"){
    //   println(nBi)
    //   println(nPrim)
    //   println(nPinMap.mkString("\n"))
    // }

    PrimPb(nBi, Nil, nModeLinks, primPb.c, primPb.annotations, primPb.name, nPrim, nPinMap, primPb.dagIds)
  }

  def getWireIds(pbs: Seq[PbType]): Set[WireId] = {
    pbs
      .filter(_.annotations.contains(AWireizeLast))
      .collect {
        case primPb: PrimPb => primPb
      }
      .map(getWireIds(_))
      .flatten
      .toSet
  }

  def apply(params: GlobalParamsInst, r: RootPb): RootPb = {
    def rec(pb: PbType): PbType = {
      val recPbs = pb.subBlocks.map(rec(_))
      val recWireIds = getWireIds(pb.subBlocks)

      pb match {
        case primPb: PrimPb => {
          val nPrim = if (primPb.annotations.contains(AWireizeLast)) {
            mapPrimitive(params, primPb)
          } else {
            primPb
          }

          // if(primPb.name == "brcidcstforkFork__00"){
          //   println(primPb.bi)
          //   println("--")
          //   println(nPrim.bi)
          //   println("--")
          // }
          nPrim
        }

        case interPb: InterPb => {
          val nLinks = mapLinks(interPb.links, recWireIds, interPb.name == "muxMergesforkSrcCst__0")
          val nPb = InterPb(interPb.bi, nLinks, interPb.modeLinks, interPb.c, interPb.annotations, interPb.name, recPbs)

          // if(interPb.name == "muxMergesforkSrcCst__0"){
          //   println("inter-----------------")
          //   println(interPb.bi)
          //   println("--")
          //   println(nPb.bi)
          //   println("--")

          //   println(nPb.links.mkString("\n"))
          // }

          nPb
        }

        case rPb: RootPb => {
          val nLinks = mapLinks(rPb.links, recWireIds, false)
          RootPb(rPb.bi, nLinks, rPb.modeLinks, rPb.c, rPb.annotations, rPb.name, recPbs, rPb.vprConfig)
        }
      }
    }

    val wiredPb = rec(r).asInstanceOf[RootPb]

    removeZeroData(canonPb(wiredPb))
  }
}
