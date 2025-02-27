package printers

import arch._

// TODO this should probably be deprecated

object TimingFilter {
  def filterTimings(timings: List[Timing], bi: BlockInterface): List[Timing] = timings.filter {
    case c @ CombTiming(src, dst, _, _) => {
      // if(!(bi.ports.contains(src.id) && bi.ports.contains(dst.id))) {
      //   println(bi.blockName + ": dropping " + c)
      // }

      (bi.ports.contains(src.id) && bi.ports.contains(dst.id))
    }

    case r @ RegTiming(loc, _, _, _, _) => {
      // if(!bi.ports.contains(loc.id)) {
      //   println(bi.blockName + ": dropping " + r)
      // }

      bi.ports.contains(loc.id)
    }
  }

  def apply(b: TBlock): TBlock = {
    ???
  }
}
