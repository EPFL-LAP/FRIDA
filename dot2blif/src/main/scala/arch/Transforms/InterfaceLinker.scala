package arch

import collection.mutable.{Map => MMap}

sealed trait LinkDirection
case object In2In extends LinkDirection
case object Out2Out extends LinkDirection
case object Out2In extends LinkDirection

object InterfaceLinker {
  case class BlockPortMatchID(width: Int, pmw: PortMeaningWrapper, dummy: DummyType)
  type BlockPortLinks = Seq[Either[BlockPort, (BlockPort, BlockPort)]]

  object PortLocs { // shouldn't this be just ports?
    def apply(ports: Map[BlockPortID, BlockPort], isPrimLocs: Boolean) = {
      new PortLocs(
        MMap() ++= ports.toSeq.map(
          (bpId, bp) => (bpId, 0)
        ),
        isPrimLocs
      )
    }
  }

  class PortLocs(val locs: MMap[BlockPortID, Int], isPrimLocs: Boolean) {
    def apply(bp: BlockPort, literal: Boolean): Int = {
      val id = if (isPrimLocs) bp.id else InterfaceCombiner.canonBlockPort(bp, literal).id
      locs(id)
    }

    def update(ports: Map[BlockPortID, BlockPort], literal: Boolean): Unit = {
      val canonPorts = if (isPrimLocs) {
        ports
      } else {
        ports.map {
          (_, bp) =>
            {
              val nBp = InterfaceCombiner.canonBlockPort(bp, literal)
              (nBp.id, nBp)
            }
        }
      }

      canonPorts.foreach {
        (k, v) =>
          {
            locs(k) = locs(k) + v.words
          }
      }
    }

    def update(bp: BlockPort, literal: Boolean, words: Int): Unit = {
      val id = if (isPrimLocs) bp.id else InterfaceCombiner.canonBlockPort(bp, literal).id
      locs(id) = locs(id) + words
    }
  }

  def linkPorts(
      src: BlockInterface,
      srcPort: BlockPort,
      dst: BlockInterface,
      dstPort: BlockPort,
      ld: LinkDirection,
      locs: PortLocs,
      literal: Boolean
  ): Seq[Direct] = {
    assert(!srcPort.isEmpty && !srcPort.isEmpty)

    val (at0, at1) = ld match {
      case In2In   => (locs(srcPort, literal), 0)
      case Out2Out => (0, locs(dstPort, literal))
      case Out2In  => (0, 0)
    }

    val words = Direct.defaultWords(srcPort, at0, dstPort, at1)

    val srcLoc = PbLoc(src.pbName, Pin(srcPort.id, at0))
    val dstLoc = PbLoc(dst.pbName, Pin(dstPort.id, at1))

    Direct.genDirects(srcLoc, dstLoc, words)
  }

  def bpMatchingID(bp: BlockPort): BlockPortMatchID = {
    bp.pmw.pb match {
      case Impl => BlockPortMatchID(bp.width, bp.pmw, bp.dummy)
      case D    => BlockPortMatchID(bp.width, bp.pmw, bp.dummy)
      case Hs => {
        if (bp.pmw.pm.matcher.nonEmpty) {
          BlockPortMatchID(0, bp.pmw, bp.dummy)
        } else {
          BlockPortMatchID(bp.width, bp.pmw, bp.dummy)
        }
      }

      case Vld => ???
      case Rdy => ???
    }
  }

  def joinInterfaces(srcPorts: Map[BlockPortID, BlockPort], dstPorts: Map[BlockPortID, BlockPort]): BlockPortLinks = {
    (srcPorts.map(_._2).toSeq ++ dstPorts.map(_._2).toSeq)
      .groupBy(bpMatchingID)
      .map {
        (_, bps) =>
          {
            bps match {
              case bp0 :: bp1 :: Nil => {
                if (srcPorts contains bp0.id) {
                  Right((bp0, bp1))
                } else {
                  Right((bp1, bp0))
                }
              }

              case bp0 :: Nil => {
                Left(bp0)
              }
            }
          }
      }
      .toSeq
  }

  def linkInterfacePorts(
      src: BlockInterface,
      srcPorts: Map[BlockPortID, BlockPort],
      dst: BlockInterface,
      dstPorts: Map[BlockPortID, BlockPort],
      ld: LinkDirection,
      at: PortLocs,
      top: Option[BlockInterface],
      topLoc: Option[PortLocs],
      literal: Boolean,
      log: Boolean
  ): List[Direct] = {
    joinInterfaces(srcPorts, dstPorts)
      .map {
        case Right((srcPort, dstPort)) => {
          if ((srcPort.words == dstPort.words) || (ld != Out2In)) {
            Some(linkPorts(src, srcPort, dst, dstPort, ld, at, literal))
          } else {
            if (srcPort.words > dstPort.words) {
              val words = srcPort.words - dstPort.words
              val canonPort = InterfaceCombiner.canonBlockPort(srcPort, literal)
              val sinkPort = top.get.ports(canonPort.id)
              val sinkLoc = topLoc.get(srcPort, literal)

              val extWords = Direct.defaultWords(srcPort, dstPort.words, sinkPort, sinkLoc)

              val srcExtLoc = PbLoc(src.pbName, Pin(srcPort.id, dstPort.words))
              val dstExtLoc = PbLoc(top.get.pbName, Pin(sinkPort.id, sinkLoc))

              val extD = Direct.genDirects(srcExtLoc, dstExtLoc, extWords)

              val intWords = Direct.defaultWords(srcPort, 0, dstPort, 0)

              val srcInLoc = PbLoc(src.pbName, Pin(srcPort.id, 0))
              val dstInLoc = PbLoc(dst.pbName, Pin(dstPort.id, 0))

              // val intD = Direct(src, srcPort, 0, dst, dstPort, 0, None, None, intWords)
              val intD = Direct.genDirects(srcInLoc, dstInLoc, intWords)

              topLoc.get.update(srcPort, literal, words)

              if (log) {
                println
                println("from: " + dstPort.words + ", to: " + sinkLoc)
                println(extD)
              }

              extD :: intD :: Nil
            } else {
              val words = dstPort.words - srcPort.words
              val canonPort = InterfaceCombiner.canonBlockPort(dstPort, literal)
              val fromPort = top.get.ports(canonPort.id)
              val fromLoc = topLoc.get(dstPort, literal)

              val extWords = Direct.defaultWords(fromPort, fromLoc, dstPort, srcPort.words)

              val srcExtLoc = PbLoc(top.get.pbName, Pin(fromPort.id, fromLoc))
              val dstExtLoc = PbLoc(dst.pbName, Pin(dstPort.id, srcPort.words))

              val extD = Direct.genDirects(srcExtLoc, dstExtLoc, extWords)

              val intWords = Direct.defaultWords(srcPort, 0, dstPort, 0)

              val srcInLoc = PbLoc(src.pbName, Pin(srcPort.id, 0))
              val dstInLoc = PbLoc(dst.pbName, Pin(dstPort.id, 0))

              val intD = Direct.genDirects(srcInLoc, dstInLoc, intWords)

              topLoc.get.update(dstPort, literal, words)

              if (log) {
                println
                println("from: " + fromLoc + ", to: " + srcPort.words + ", with: " + words)
                println(extD)
              }

              extD :: intD :: Nil
            }
          }
        }

        case Left(bp) => {
          if (ld == Out2In) {
            bp.pt match {
              case PTInput => {
                val bi = dst
                val canonPort = InterfaceCombiner.canonBlockPort(bp, literal)
                val toTop = linkPorts(top.get, top.get.inPorts()(canonPort.id), bi, bp, In2In, topLoc.get, literal)

                topLoc.get.update(Map((bp.id -> bp)), literal)

                toTop :: Nil
              }

              case PTOutput => {
                val bi = src
                val canonPort = InterfaceCombiner.canonBlockPort(bp, literal)
                val toTop = linkPorts(bi, bp, top.get, top.get.outPorts()(canonPort.id), Out2Out, topLoc.get, literal)

                topLoc.get.update(Map((bp.id -> bp)), literal)

                toTop :: Nil
              }

              case other => ???
            }
          } else {
            Nil
          }
        }
      }
      .flatten
      .flatten
      .toList
  }

  def oneToOneAt(top: BlockInterface, child: BlockInterface, at: PortLocs, literal: Boolean): List[Link] = {
    val (sourceInPorts, sourceOutPorts) = top.ports.partition(_._2.pt == PTInput)
    val (destInPorts, destOutPorts) = child.ports.partition(_._2.pt == PTInput)

    val ins = linkInterfacePorts(top, sourceInPorts, child, destInPorts, In2In, at, None, None, literal, false)
    val outs = linkInterfacePorts(child, destOutPorts, top, sourceOutPorts, Out2Out, at, None, None, literal, false)

    (ins ++ outs)
  }

  def linkInterfacesAndOr(
      tc: TileCombinator,
      topLevel: BlockInterface,
      interfaces: List[BlockInterface],
      literal: Boolean
  ): List[Link] = {
    val locs = PortLocs(topLevel.ports, false)

    interfaces.map {
      (i: BlockInterface) =>
        {
          val links = oneToOneAt(topLevel, i, locs, literal)

          if (tc == TileAnd) {
            locs.update(i.ports, literal)
          }

          // if(i.clocked) {
          //   CLK(topLevel.pbName, i.pbName) +: links
          // } else {
          //   links
          // }

          links
        }
    }.flatten
  }

  def linkInterfacesAndThen(
      top: BlockInterface,
      interfaces: List[BlockInterface],
      literal: Boolean,
      log: Boolean
  ): List[Link] = {
    val topLocs = PortLocs(top.ports, false)
    val right = linkInterfacePorts(
      interfaces.last,
      interfaces.last.outPorts(),
      top,
      top.outPorts(),
      Out2Out,
      topLocs,
      None,
      None,
      literal,
      log
    )

    topLocs.update(interfaces.last.outPorts(), literal)

    val left = linkInterfacePorts(
      top,
      top.inPorts(),
      interfaces.head,
      interfaces.head.inPorts(),
      In2In,
      topLocs,
      None,
      None,
      literal,
      log
    )

    topLocs.update(interfaces.head.inPorts(), literal)

    val links = interfaces
      .drop(1)
      .foldLeft[(List[Link], BlockInterface)] {
        ((right ++ left), interfaces.head)
      } {
        case ((links, prevInt), i) => {
          val nLinks = linkInterfacePorts(
            prevInt,
            prevInt.outPorts(),
            i,
            i.inPorts(),
            Out2In,
            topLocs,
            Some(top),
            Some(topLocs),
            literal,
            log
          )

          if (log) {
            println
            println("                " + prevInt.pbName + " -> " + i.pbName)
            // println(nLinks.mkString("\n"))
          }

          // val clk = if(i.clocked) {
          //   Some(CLK(top.pbName, i.pbName))
          // } else {
          //   None
          // }

          // (clk.fold(nLinks ++ links)(lClk => lClk :: (nLinks ++ links)), i)

          (nLinks ++ links, i)
        }
      }
      ._1

    // if(interfaces.head.clocked) {
    //   CLK(top.pbName, interfaces.head.pbName) :: links
    // } else {
    //   links
    // }

    links
  }

  def findModeInterconnect(
      mode: PbElem,
      topLevel: BlockInterface,
      interfaces: List[BlockInterface],
      literal: Boolean
  ): List[Link] = {
    mode.modes.head.combinator match {
      case TileAnd => linkInterfacesAndOr(TileAnd, topLevel, interfaces, literal)
      case TileOr => {
        linkInterfacesAndOr(TileOr, topLevel, interfaces, literal)
      }
      case TileAndThen => {
        val log = false // topLevel.blockName == "Arith__"
        val t = linkInterfacesAndThen(topLevel, interfaces, literal, log)

        // if(log) {
        //   ???
        // }

        t
      }
    }
  }
}
