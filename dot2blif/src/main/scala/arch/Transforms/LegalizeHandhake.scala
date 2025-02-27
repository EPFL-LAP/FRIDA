package arch

// Maps any handshake that is non zero width to zero width

object LegalizeHandhake {
  def mapId(id: BlockPortID): BlockPortID = {
    if (id.pmw.pb.isHandshake() && (id.width != 0)) {
      BlockPortID(0, id.pt, id.pmw, id.dummy)
    } else {
      id
    }

  }

  def mapPort(bp: BlockPort): BlockPort = {
    val nId = mapId(bp.id)
    BlockPort(nId, bp.words, bp.annotations)
  }

  def mapPin(pin: Pin): Pin = {
    val nId = mapId(pin.id)
    Pin(nId, pin.loc)
  }

  def mapInterface(bi: BlockInterface): BlockInterface = {
    val nPorts = bi.ports.map {
      (id, bp) =>
        {
          val nBp = mapPort(bp)
          (nBp.id, nBp)
        }
    }

    BlockInterface(nPorts, bi.pbName, bi.clocked)
  }

  def updateInterconnect(links: List[Link]): List[Link] = {
    links.map {
      case d @ Direct(srcLoc, dstLoc, delay) => {
        val nSrcPin = mapPin(srcLoc.pin)
        val nSrcLoc = PbLoc(srcLoc.pbName, nSrcPin)

        val nDstPin = mapPin(dstLoc.pin)
        val nDstLoc = PbLoc(dstLoc.pbName, nDstPin)

        Direct(nSrcLoc, nDstLoc, delay)
      }

      case m @ Mux(sources, dstLoc, delay) => {
        val nSources = sources.map {
          srcLoc =>
            {
              val nSrcPin = mapPin(srcLoc.pin)
              val nSrcLoc = PbLoc(srcLoc.pbName, nSrcPin)

              nSrcLoc
            }
        }

        val nDstPin = mapPin(dstLoc.pin)
        val nDstLoc = PbLoc(dstLoc.pbName, nDstPin)

        Mux(nSources, nDstLoc, delay)
      }

      case other => other
    }
  }

  def apply(pb: PbType): PbType = {
    val nSubs = pb.subBlocks.map(LegalizeHandhake(_))

    val nBi = mapInterface(pb.bi)
    val nInterconnect = updateInterconnect(pb.links)
    val nModeInterconnect = updateInterconnect(pb.modeLinks)

    pb match {
      case RootPb(bi, links, modeLinks, c, annotations, name, subBlocks, vprConfig) => {
        RootPb(nBi, nInterconnect, nModeInterconnect, c, annotations, name, nSubs, vprConfig)
        ???
      }

      case InterPb(bi, links, modeLinks, c, annotations, name, subBlocks) => {
        InterPb(nBi, nInterconnect, nModeInterconnect, c, annotations, name, nSubs)
      }

      case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
        PrimPb(nBi, nInterconnect, nModeInterconnect, c, annotations, name, prim, pinMap, dagIds)
      }
    }
  }
}
