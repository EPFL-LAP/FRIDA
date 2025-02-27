package arch

object CanonicalizeArch {
  def getLocMap(rootPb: RootPb): Map[PbLoc, PbLoc] = {
    def recLocMap(pb: PbType): Seq[(PbLoc, PbLoc)] = {
      pb.bi.ports
        .map(_._2)
        .map(_.toPins())
        .flatten
        .groupBy(_.id.canonicalize())
        .map {
          (idCanon, pins) =>
            {
              pins.zipWithIndex.map {
                (pin, i) =>
                  {
                    val oldLoc = PbLoc(pb.name, pin)
                    val nLoc = PbLoc(pb.name, Pin(idCanon, i))

                    (oldLoc -> nLoc)
                  }
              }
            }
        }
        .flatten
        .toSeq
    }

    def rec(pb: PbType): Seq[(PbLoc, PbLoc)] = {
      pb.subBlocks.map(rec(_)).flatten ++ recLocMap(pb)
    }

    val seq = rec(rootPb)
    val res = seq.toMap
    assert(seq.size == res.size)

    res
  }

  def mapLinks(links: List[Link], locMap: Map[PbLoc, PbLoc]): List[Link] = {
    links.map {
      case Direct(srcLoc, dstLoc, delay) => Direct(locMap(srcLoc), locMap(dstLoc), delay)
      case Mux(sources, dstLoc, delay) => {
        val nSources = sources.map(locMap(_))
        Mux(nSources, locMap(dstLoc), delay)
      }
      case clk: CLK => clk
    }
  }

  def mapBi(pb: PbType, bi: BlockInterface, locMap: Map[PbLoc, PbLoc]): BlockInterface = {
    val nPorts = bi.ports
      .map(_._2)
      .map(
        bp => bp.toPins()
      )
      .flatten
      .map(PbLoc(pb.name, _))
      .map(locMap(_))
      .map(_.pin)
      .groupBy(_.id)
      .map {
        (id, pins) =>
          {
            (id, BlockPort(id, pins.size, Set()))
          }
      }
      .toMap

    BlockInterface(nPorts, bi.pbName, bi.clocked)
  }

  def mapPinMap(primPb: PrimPb, locMap: Map[PbLoc, PbLoc]): Map[Pin, Pin] = {
    val locPinMap = locMap
      .filter(_._1.pbName == primPb.name)
      .map(
        (oldLoc, nLoc) => (oldLoc.pin, nLoc.pin)
      )

    primPb.pinMap.map {
      (srcPin, dstPin) =>
        {
          (locPinMap(srcPin), dstPin)
        }
    }
  }

  def mapAST(pb: PbType, locMap: Map[PbLoc, PbLoc]): PbType = {
    pb match {
      case primPb @ PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
        assert(modeLinks.isEmpty)

        val nBi = mapBi(pb, bi, locMap)
        val nLinks = mapLinks(links, locMap)
        val nPinMap = mapPinMap(primPb, locMap)

        PrimPb(nBi, nLinks, Nil, c, annotations, name, prim, nPinMap, dagIds)
      }

      case InterPb(bi, links, modeLinks, c, annotations, name, subBlocks) => {
        assert(modeLinks.isEmpty)

        val nBi = mapBi(pb, bi, locMap)
        val nLinks = mapLinks(links, locMap)
        val nSubBlocks = subBlocks.map(mapAST(_, locMap))

        InterPb(nBi, nLinks, Nil, c, annotations, name, nSubBlocks)
      }

      case RootPb(bi, links, modeLinks, c, annotations, name, subBlocks, vprConfig) => {
        assert(modeLinks.isEmpty)

        val nBi = mapBi(pb, bi, locMap)
        val nLinks = mapLinks(links, locMap)

        val nSubBlocks = subBlocks.map(mapAST(_, locMap))

        RootPb(nBi, nLinks, Nil, c, annotations, name, nSubBlocks, vprConfig)
      }
    }
  }

  def idMap(pb: PbType): Seq[(PbLoc, PbLoc)] = {
    pb.bi.ports
      .map(_._2)
      .map(_.toPins())
      .flatten
      .map {
        pin =>
          {
            val oldLoc = PbLoc(pb.name, pin)
            val nLoc = oldLoc

            (oldLoc -> nLoc)
          }
      }
      .toSeq
  }

  def getCastLocMap(rootPb: RootPb): Map[PbLoc, PbLoc] = {
    def recLocMap(pb: PbType): Seq[(PbLoc, PbLoc)] = {
      pb match {
        case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
          pinMap.toSeq.map {
            (pbPin, primPin) =>
              {
                val pbLoc = PbLoc(pb.name, pbPin)
                val nLoc = PbLoc(pb.name, primPin)

                (pbLoc -> nLoc)
              }
          }
        }

        case other => {
          idMap(other)
        }
      }
    }

    def rec(pb: PbType): Seq[(PbLoc, PbLoc)] = {
      pb.subBlocks.map(rec(_)).flatten ++ recLocMap(pb)
    }

    val seq = rec(rootPb)
    val res = seq.toMap
    assert(seq.size == res.size)

    res
  }

  def apply(rootPb: RootPb): RootPb = {
    val locMap = getLocMap(rootPb)
    val noMatchers = mapAST(rootPb, locMap).asInstanceOf[RootPb]

    val castLocMap = getCastLocMap(noMatchers)
    mapAST(noMatchers, castLocMap).asInstanceOf[RootPb]
  }
}
