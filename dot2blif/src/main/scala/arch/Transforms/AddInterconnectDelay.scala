package arch

import archs.MuxConfigParams
import archs.MuxConfig
import frontend.GlobalParamsInst

object AddInterconnectDelay {
  def sDelayToDouble(s: String): Double = {
    val splitted = s.split("e-")
    val delay = splitted(0)
    val unit = splitted(1)

    assert(unit == "9") // we only support one unit for now

    delay.toDouble
  }

  def maxDelay(t0Opt: Option[CombTiming], t1: CombTiming): CombTiming = {
    t0Opt.fold(t1) {
      t0 =>
        {
          assert(t1.maxDelay == t1.minDelay)
          assert(t0.maxDelay == t0.minDelay)

          val t0Delay = sDelayToDouble(t0.maxDelay)
          val t1Delay = sDelayToDouble(t1.maxDelay)

          if (t0Delay > t1Delay) {
            t0
          } else {
            t1
          }
        }
    }
  }

  def getMuxWorseDelay(width: Int, num: Int)(implicit params: GlobalParamsInst): CombTiming = {
    val minNum = if (num < 2) 2 else num
    val muxP = MuxConfigParams(width, minNum, 0, 0)

    val fullTimings = MuxConfig(muxP).getTimings(params)

    val criticalPath = fullTimings.timings.foldLeft[Option[CombTiming]](None) {
      (acc, t) =>
        {
          t match {
            case c: CombTiming => {
              Some(maxDelay(acc, c))
            }

            case other => {
              scala.sys.error("Expected Mux as combinational component")
            }
          }
        }
    }

    criticalPath.get
  }

  def mapInterconnect(links: List[Link])(implicit params: GlobalParamsInst): List[Link] = {
    links.map {
      case Mux(sources, dstLoc, _) => {
        val width = dstLoc.pin.id.concreteWidth
        val delay = getMuxWorseDelay(width, sources.size)

        Mux(sources, dstLoc, Some(delay))
      }

      case other => other
    }
  }

  def apply(tile: PbType, params: GlobalParamsInst): PbType = {
    implicit val p = params

    def rec(pb: PbType, parent: Option[PbType]): PbType = {
      assert(pb.modeLinks.isEmpty)

      val recPbs = pb.subBlocks.map(rec(_, Some(pb)))
      val nInterconnect = mapInterconnect(pb.links)

      pb match {
        case prim: PrimPb => {
          prim
        }

        case InterPb(bi, links, modeLinks, c, annotations, name, _) => {
          InterPb(bi, nInterconnect, modeLinks, c, annotations, name, recPbs)
        }

        case RootPb(bi, links, modeLinks, c, annotations, name, _, vprConfig) => {
          RootPb(bi, nInterconnect, modeLinks, c, annotations, name, recPbs, vprConfig)
        }
      }
    }

    rec(tile, None)
  }

  def apply(tile: RootPb, params: GlobalParamsInst): RootPb = {
    AddInterconnectDelay(tile.asInstanceOf[PbType], params).asInstanceOf[RootPb]
  }
}
