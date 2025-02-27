package packerv2

import collection.mutable.{Map => MMap}
import collection.mutable.{Set => MSet}

object CollapseRRGCMuxs {
  def getIns(rrg: RRG, cmuxs: Set[RRCMux]): Set[RR] = {
    cmuxs
      .map(rrg.preds(_))
      .flatten
      .filter(
        rr => !cmuxs.asInstanceOf[Set[RR]].contains(rr)
      )
      .toSet
  }

  def getConnectedCMuxs(rrg: RRG, cmux: RRCMux): Set[RRCMux] = {
    rrg
      .preds(cmux)
      .collect {
        case cmux: RRCMux => cmux
      }
      .filter(rrg.succs(_).size == 1)
      .map(getConnectedCMuxs(rrg, _))
      .flatten + cmux
  }

  def apply(rrg: RRG): RRG = {
    val nSuccs = MMap() ++= rrg.succs
    val nPreds = MMap() ++= rrg.preds
    val nRRs = MSet() ++= rrg.rrs

    rrg.rrs
      .collect {
        case cmux: RRCMux => cmux
      }
      .filter(
        cmux => rrg.succs(cmux).filter(_.isInstanceOf[RRCMux]).isEmpty
      )
      .map {
        cmux =>
          {
            val predCmuxs = getConnectedCMuxs(rrg, cmux)
            val nIns = getIns(rrg, predCmuxs)

            val toRemove = (predCmuxs - cmux).asInstanceOf[Set[RR]]

            nSuccs --= toRemove
            nPreds --= toRemove
            nRRs --= toRemove

            nPreds(cmux) = nIns

            nIns.foreach {
              rr =>
                {
                  nSuccs(rr) = nSuccs(rr).filter(
                    nrr => !toRemove.contains(nrr)
                  ) + cmux
                }
            }
          }
      }

    RRG(rrg.molName, rrg.prims, nRRs.toSet, nPreds.toMap, nSuccs.toMap, rrg.srcSinkToPrim, rrg.t, rrg.pb, rrg.pinMap)
  }
}
