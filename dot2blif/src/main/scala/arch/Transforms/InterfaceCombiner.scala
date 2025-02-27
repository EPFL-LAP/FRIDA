package arch

import math.max
import core.Namer
import util.Util
import collection.mutable.{Map => MMap}

object InterfaceCombiner {
  type PortUnion = Map[BlockPortID, Either[BlockPort, (BlockPort, BlockPort)]]
  type BlockPortInstance = (BlockInterface, BlockPort, Int)

  // BlockPort Operations

  def mergeAnd(bp0: BlockPort, bp1: BlockPort): BlockPort = {
    assert(bp0.id == bp1.id)

    if (bp1.isEmpty) {
      return BlockPort(bp0.id, bp0.words, Set())
    }

    if (bp0.isEmpty) {
      return BlockPort(bp1.id, bp1.words, Set())
    }

    val nId = bp0.id // bp0.id == bp1.id
    BlockPort(nId, bp0.words + bp1.words, Set())
  }

  // Returns the new mapping for the matchers
  def mergeOr(bp0: BlockPort, bp1: BlockPort): BlockPort = {
    assert(bp0.id == bp1.id)

    if (bp1.isEmpty) {
      return BlockPort(bp0.id, bp0.words, Set())
    }

    if (bp0.isEmpty) {
      return BlockPort(bp1.id, bp1.words, Set())
    }

    val nId = bp0.id // bp0.id == bp1.id
    BlockPort(nId, max(bp0.words, bp1.words), Set())
  }

  // BlockInterface Operations

  def join(bi0: BlockInterface, bi1: BlockInterface): PortUnion = {
    (bi0.ports.toSeq ++ bi1.ports.toSeq)
      .map(_._2)
      .groupBy(_.id)
      .map(_._2)
      .map {
        (bps: Seq[BlockPort]) =>
          bps match {
            case bp0 :: bp1 :: Nil => (bp0.id, Right((bp0, bp1)))
            case bp0 :: Nil        => (bp0.id, Left(bp0))
          }
      }
      .toMap
  }

  def mergeRec(
      bi0: BlockInterface,
      bi1: BlockInterface,
      comb: (BlockPort, BlockPort) => BlockPort
  ): Map[BlockPortID, BlockPort] = {
    join(bi0, bi1)
      .map(_._2)
      .map {
        case Left(bp0) => {
          BlockPort(bp0.id, bp0.words, Set())
        }

        case Right((bp0, bp1)) => {
          if (bi0.ports.values.toList.contains(bp0)) {
            comb(bp0, bp1)
          } else {
            comb(bp1, bp0)
          }
        }
      }
      .map(
        bp => (bp.id, bp)
      )
      .toMap
  }

  def mergeOr(bi0: BlockInterface, bi1: BlockInterface): Map[BlockPortID, BlockPort] = {
    def comb(bp0: BlockPort, bp1: BlockPort): BlockPort = {
      mergeOr(bp0, bp1)
    }

    mergeRec(bi0, bi1, comb)
  }

  def mergeAnd(bi0: BlockInterface, bi1: BlockInterface): Map[BlockPortID, BlockPort] = {
    def comb(bp0: BlockPort, bp1: BlockPort): BlockPort = {
      mergeAnd(bp0, bp1)
    }

    mergeRec(bi0, bi1, comb)
  }

  // Whole level merge semantic

  def findChainTopInterface(m: PbElem, children: List[BlockInterface]): BlockInterface = {
    def recLocal(locChain: List[BlockInterface]): List[BlockPortInstance] = {
      locChain match {
        case Nil => Nil
        case locInt :: xs => {
          if (xs.isEmpty) {
            Nil
          } else {
            val locOutPorts = locInt.ports.filter(_._1.pt == PTOutput)
            val nInPorts = xs.head.ports.filter(_._1.pt == PTInput)

            val unMatchedPorts = (locOutPorts.toSeq ++ nInPorts.toSeq)
              .map(_._2)
              .groupBy(
                bp => (bp.width, bp.pmw, bp.dummy)
              )
              .map(_._2)
              .map {
                case bp0 :: bp1 :: Nil => {
                  if (bp0.words == bp1.words) {
                    None
                  } else if (bp0.words > bp1.words) {
                    if (bp0.pt == PTOutput) {
                      // Some output port here is not matched
                      Some(locInt, bp0, bp0.words - bp1.words)
                    } else {
                      // Some input port here is not matched
                      Some(xs.head, bp1, bp1.words - bp0.words)
                    }
                  } else {
                    if (bp0.pt == PTOutput) {
                      // Some input port here is not matched
                      Some(xs.head, bp1, bp1.words - bp0.words)
                    } else {
                      // Some output port here is not matched
                      Some(locInt, bp0, bp0.words - bp1.words)
                    }
                  }
                }

                case bp0 :: Nil => {
                  if (bp0.pt == PTOutput) {
                    Some(xs.head, bp0, 0)
                  } else {
                    Some(locInt, bp0, 0)
                  }
                }
              }
              .flatten
              .toList

            unMatchedPorts ++ recLocal(xs)
          }
        }
      }
    }

    // While matching accross the chain without matcher is possible, it is heavily discouraged
    def recGlobal(unmatchedPorts: List[BlockPortInstance]): List[BlockPortInstance] = {
      def rec(
          locChain: List[BlockInterface],
          seenBlock: Boolean,
          port: BlockPortInstance
      ): Option[BlockPortInstance] = {
        locChain match {
          case Nil => Some(port)
          case locInt :: xs => {
            val nSeen = (locInt.pbName == port._1.pbName) || seenBlock
            if (((port._2.pt == PTInput) && !nSeen) || (port._2.pt == PTOutput) && nSeen) {
              val ports = locInt.ports.filter(_._1.pt == PortType.not(port._2.pt))
              val pKey = (port._2.width, port._2.pmw, port._2.dummy)

              ports
                .map(_._2)
                .map(
                  bp => ((bp.width, bp.pmw, bp.dummy), bp)
                )
                .toMap
                .get(pKey)
                .fold(rec(xs, nSeen, port)) {
                  bp =>
                    {
                      val remainingWords = port._2.words - (bp.words - port._3)
                      if (remainingWords != 0) {
                        rec(xs, nSeen, port)
                      } else {
                        // Check that bp is not used locally already
                        unmatchedPorts
                          .filter {
                            p =>
                              {
                                (p._1.pbName == locInt.pbName) && (p._2 == bp)
                              }
                          }
                          .headOption
                          .fold(rec(xs, nSeen, port))(
                            _ => None
                          )
                      }
                    }
                }
            } else {
              rec(xs, nSeen, port)
            }
          }
        }
      }

      unmatchedPorts.map(rec(children, false, _)).flatten
    }

    val unmatchedPorts = recLocal(children)
    val externalPorts = recGlobal(unmatchedPorts).map(
      (bi, bp, at) => (bp, at)
    )

    val inPorts = children.head.ports
      .filter(_._1.pt == PTInput)
      .map(_._2)
      .map(
        bp => (bp, 0)
      )
    val outPorts = children.last.ports
      .filter(_._2.pt == PTOutput)
      .map(_._2)
      .map(
        bp => (bp, 0)
      )

    val ports = (inPorts.toSeq ++ outPorts.toSeq ++ externalPorts.toSeq)
      .groupBy(_._1.id)
      .map {
        case (id, bps) if (bps.size == 1) => {
          val bp = bps.head._1
          val start = bps.head._2

          val nBp = BlockPort(bp.id, bp.words - start, bp.annotations)

          (id, nBp)
        }

        case (id, bps) if (bps.size > 1) => {
          val words = bps
            .map(
              (bp, at) => (bp.words - at)
            )
            .reduce(_ + _)
          (id, BlockPort(id, words, Set()))
        }

        case other => scala.sys.error("unexpected match")
      }
      .toMap

    // if(m.name == "muxMergesbrbufsrccst__0") {
    //   // println(unmatchedPorts.mkString("\n"))
    //   // println("--")
    //   // println(externalPorts.mkString("\n"))

    //   // println
    //   // println

    //   // println(ports.mkString("\n"))

    //   println(children)

    //   ???
    // }

    val clocked = children.map(_.clocked).reduceOption(_ || _).getOrElse(false)

    BlockInterface(ports, m.name, clocked)
  }

  def canonBlockPort(bp: BlockPort, literal: Boolean): BlockPort = {
    bp.pmw.pb match {
      case Impl => bp
      case D    => bp
      case Hs => {
        if (bp.pmw.pm.matcher.nonEmpty && !literal) {
          val nId = BlockPortID(0, bp.id.pt, bp.id.pmw, bp.id.dummy)
          BlockPort(nId, bp.words, Set())
        } else {
          bp
        }
      }

      case Vld => ???
      case Rdy => ???
    }
  }

  def canonicalizeInterface(bi: BlockInterface, literal: Boolean): BlockInterface = {
    val canonPorts = bi.ports.map {
      (id, bp) =>
        {
          val nBp = canonBlockPort(bp, literal)

          (nBp.id, nBp)
        }
    }

    BlockInterface(canonPorts, bi.pbName, bi.clocked)
  }

  def findOrTopInterface(
      m: PbElem,
      children: List[BlockInterface],
      tc: TileCombinator,
      literal: Boolean
  ): BlockInterface = {
    children.drop(1).foldLeft(children.head.withName(m.name)) {
      (accInt, i) =>
        {
          val nPorts = mergeOr(accInt, i)
          BlockInterface(nPorts, m.name, accInt.clocked || i.clocked)
        }
    }
  }

  def findAndTopInterface(m: PbElem, children: List[BlockInterface], tc: TileCombinator): BlockInterface = {
    children.drop(1).foldLeft(children.head.withName(m.name)) {
      (accInt, i) =>
        {
          val nPorts = mergeAnd(accInt, i)
          BlockInterface(nPorts, m.name, accInt.clocked || i.clocked)
        }
    }
  }

  def findTopInterface(
      m: PbElem,
      childrenPbs: List[PbType],
      tc: TileCombinator,
      literal: Boolean
  ): (BlockInterface, List[PbType]) = {
    val canonChildren = childrenPbs.map(_.bi).map(canonicalizeInterface(_, literal))

    val (int, nChildren) = tc match {
      case TileOr      => (findOrTopInterface(m, canonChildren, tc, literal), childrenPbs)
      case TileAnd     => (findAndTopInterface(m, canonChildren, tc), childrenPbs)
      case TileAndThen => (findChainTopInterface(m, canonChildren), childrenPbs)
    }

    val nInterface = canonicalizeInterface(int, literal)

    (nInterface, nChildren)
  }
}
