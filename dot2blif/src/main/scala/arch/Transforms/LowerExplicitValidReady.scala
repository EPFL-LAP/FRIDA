package arch

// TODO also change the direction of the rdy links

object LowerExplicitValidReady {
  def mapId(id: BlockPortID): Seq[BlockPortID] = {
    id.pmw.pb match {
      case D => id :: Nil
      case Hs => {
        val vld = BlockPortID(id.width, id.pt, PortMeaningWrapper(id.pmw.pm, Vld), id.dummy)
        val rdy = BlockPortID(id.width, id.pt.flipped, PortMeaningWrapper(id.pmw.pm, Rdy), id.dummy)

        vld :: rdy :: Nil
      }

      case other => scala.sys.error("Unexpected PortBundle.")
    }
  }

  def mapPin(pin: Pin): Seq[Pin] = {
    mapId(pin.id).map {
      nId =>
        {
          Pin(nId, pin.loc)
        }
    }
  }

  def mapLoc(pbLoc: PbLoc): Seq[PbLoc] = {
    mapPin(pbLoc.pin).map {
      nPin =>
        {
          PbLoc(pbLoc.pbName, nPin)
        }
    }
  }

  def mapBlockPort(bp: BlockPort): Seq[BlockPort] = {
    mapId(bp.id).map {
      nId =>
        {
          BlockPort(nId, bp.words, bp.annotations)
        }
    }
  }

  def mapLinks(links: List[Link]): List[Link] = {
    links.map {
      case origD @ Direct(srcLoc, dstLoc, delay) => {
        val nSrcLocs = mapLoc(srcLoc)
        val nDstLocs = mapLoc(dstLoc).groupBy(_.pin.id.pmw.pb)

        nSrcLocs.map {
          nSrcLoc =>
            {
              val nDstLoc = nDstLocs(nSrcLoc.pin.id.pmw.pb)
              assert(nDstLoc.size == 1)

              nSrcLoc.pin.id.pmw.pb match {
                case Rdy => {
                  Direct(nDstLoc.head, nSrcLoc, delay)
                }

                case other => {
                  Direct(nSrcLoc, nDstLoc.head, delay)
                }
              }
            }
        }.toList
      }

      case Mux(sources, dstLoc, delay) => {
        // If ready, replace mux by one to many Direct
        // Ready muxs inserted by another function
        val nSrcLocs = sources.map(mapLoc(_)).flatten.groupBy(_.pin.id.pmw.pb)
        val nDstLocs = mapLoc(dstLoc)

        nDstLocs
          .map {
            nDstLoc =>
              {
                nDstLoc.pin.id.pmw.pb match {
                  case Rdy => {
                    val nDsts = nSrcLocs(nDstLoc.pin.id.pmw.pb)
                    val nSrc = nDstLoc

                    nDsts.map {
                      nDst =>
                        {
                          Direct(nSrc, nDst, None)
                        }
                    }
                  }

                  case other => {
                    val nSources = nSrcLocs(nDstLoc.pin.id.pmw.pb)

                    Mux(nSources, nDstLoc, delay) :: Nil
                  }
                }

              }
          }
          .flatten
          .toList
      }

      case clk: CLK => clk :: Nil
    }.flatten
  }

  def insertRdyMuxs(links: List[Link]): List[Link] = {
    val directs = links.collect {
      case d: Direct => d
    }
    val others = links.filter(!_.isInstanceOf[Direct])

    val nDirects = directs
      .groupBy(_.dstLoc)
      .map {
        (dstLoc, directs) =>
          {
            dstLoc.pin.id.pmw.pb match {
              case Rdy => {
                if (directs.size == 1) {
                  directs
                } else {
                  val srcLocs = directs.map(_.srcLoc)

                  Mux(srcLocs, dstLoc, None) :: Nil
                }
              }

              case other => directs
            }
          }
      }
      .flatten

    (others ++ nDirects).toList
  }

  def mapInterface(bi: BlockInterface): BlockInterface = {
    val nPorts = bi.ports
      .map(_._2)
      .map(mapBlockPort(_))
      .flatten
      .groupBy(_.id)
      .map(
        (id, bps) => (id, bps.head)
      )
      .toMap

    BlockInterface(nPorts, bi.pbName, bi.clocked)
  }

  def mapPinMap(pinMap: Map[Pin, Pin]): Map[Pin, Pin] = {
    pinMap
      .map {
        (srcPin, dstPin) =>
          {
            val nSrcPins = mapPin(srcPin)
            val nDstPins = mapPin(dstPin).groupBy(_.id.pmw.pb)

            nSrcPins.map {
              nSrcPin =>
                {
                  val nDstPin = nDstPins(nSrcPin.id.pmw.pb)
                  assert(nDstPin.size == 1)

                  (nSrcPin, nDstPin.head)
                }
            }
          }
      }
      .flatten
      .toMap
  }

  def apply(pb: PbType): PbType = {
    val nSubBlocks = pb.subBlocks.map(LowerExplicitValidReady(_))
    assert(pb.modeLinks.isEmpty, pb.modeLinks.mkString("\n"))

    pb match {
      case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
        val nBi = mapInterface(bi)
        val nLinks = insertRdyMuxs(mapLinks(links))

        val nPrim = LowerExplicitValidReady(prim)
        val nPinMap = mapPinMap(pinMap)

        PrimPb(nBi, nLinks, Nil, c, annotations, name, nPrim, nPinMap, dagIds)
      }

      case InterPb(bi, links, modeLinks, c, annotations, name, _) => {
        val nBi = mapInterface(bi)
        val nLinks = insertRdyMuxs(mapLinks(links))

        InterPb(nBi, nLinks, Nil, c, annotations, name, nSubBlocks)
      }

      case RootPb(bi, links, modeLinks, c, annotations, name, _, vprConfig) => {
        val nBi = mapInterface(bi)
        val nLinks = insertRdyMuxs(mapLinks(links))

        RootPb(nBi, nLinks, Nil, c, annotations, name, nSubBlocks, vprConfig)
      }
    }
  }

  def verify(rootPb: RootPb): Boolean = {
    def checkPb(pb: PbType): Boolean = {
      pb.links.forall {
        case d @ Direct(srcLoc, dstLoc, delay) => {
          srcLoc.pin.id.pt match {
            case PTInput => {
              val isCorrect = (srcLoc.pbName == pb.name) && (dstLoc.pbName == pb.subBlocks.head.name)
              assert(isCorrect, d)

              isCorrect
            }

            case PTOutput => {
              dstLoc.pin.id.pt match {
                case PTInput => {
                  val isCorrect = (srcLoc.pbName != pb.name) && (dstLoc.pbName != pb.name)
                  assert(isCorrect)

                  isCorrect
                }

                case PTOutput => {
                  val isCorrect = (srcLoc.pbName == pb.subBlocks.last.name) && (dstLoc.pbName == pb.name)
                  assert(isCorrect)

                  isCorrect
                }

                case other => scala.sys.error("Expected defined port direction.")
              }

            }

            case other => scala.sys.error("Expected defined port direction.")
          }
        }

        case Mux(sources, dstLoc, delay) => true
        case CLK(srcPb, dstPb)           => true
      }
    }

    def rec(pb: PbType): Boolean = {
      checkPb(pb) && pb.subBlocks.forall(rec(_))
    }

    rec(rootPb)
  }

  def apply(pb: RootPb): RootPb = {
    LowerExplicitValidReady(pb.asInstanceOf[PbType]).asInstanceOf[RootPb]
  }

  def apply(b: TBlock): TBlock = {
    val nBi = mapInterface(b.blockInterface)

    Block(b.prim, nBi, b.physicalInfo, b.annotations, b.namedAttrs)
  }
}
