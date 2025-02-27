package arch

import archs.MuxConfigParams
import archs.MuxConfig
import frontend.GlobalParamsInst
import analysis.AreaExtractor
import analysis.RRGConInfo

object AddConnectionBlockDelay {
  def getMuxIn(params: GlobalParamsInst, arch: Arch, width: Int): Int = {
    arch.fcIn.getFc(arch.chanWidth) // conInfo(arch).cMuxIns
  }

  def getMuxSizes(pb: RootPb, archs: List[Arch], params: GlobalParamsInst): Map[(PortType, Int), CombTiming] = {
    val ioInfo = pb.ioInfo()
    // println(ioInfo)

    val muxes = ioInfo.filter(_._1._1 == PTInput).map {
      case ((pt, width), num) => {
        val muxs = archs.filter(_.width == width).map {
          arch =>
            {
              val muxIn = getMuxIn(params, arch, width)
              val muxDelay = AddInterconnectDelay.getMuxWorseDelay(width, muxIn)(params)

              // println(arch.name + " -> " + width + ", " + muxIn + " : " + muxDelay)

              assert(AddInterconnectDelay.sDelayToDouble(muxDelay.maxDelay) > 0)

              ((pt, width), muxDelay)
            }
        }

        assert(muxs.size == 1, muxs)
        muxs.head
      }
    }

    muxes
  }

  def updateInterconnect(links: List[Link], muxes: Map[(PortType, Int), CombTiming]): List[Link] = {
    links.map {
      case d @ Direct(srcLoc, dstLoc, origDelay) => {
        assert(origDelay.isEmpty)

        val pb = srcLoc.pin.id.pmw.pb
        val pt = srcLoc.pin.id.pt

        val width = srcLoc.pin.id.concreteWidth
        val delay = muxes((PTInput, width))

        pt match {
          case PTInput => {
            pb match {
              case D     => Direct(srcLoc, dstLoc, Some(delay))
              case Vld   => Direct(srcLoc, dstLoc, Some(delay))
              case Rdy   => Direct(srcLoc, dstLoc, Some(delay))
              case other => scala.sys.error("Expected lowered architecture.")
            }
          }

          case PTOutput => {
            pb match {
              case D     => d
              case Vld   => d
              case Rdy   => d
              case other => scala.sys.error("Expected lowered architecture.")
            }
          }

          case other => scala.sys.error("Expected lowered architecture.")
        }
      }

      case other => other
    }
  }

  def apply(tile: RootPb, archs: List[Arch], params: GlobalParamsInst): RootPb = {
    implicit val p = params

    assert(tile.modeLinks.isEmpty, "Need to run MuxExpliciter before this pass.")

    val muxes = getMuxSizes(tile, archs, params)
    val nInterconnect = updateInterconnect(tile.links, muxes)

    RootPb(
      tile.bi,
      nInterconnect,
      Nil,
      tile.c,
      tile.annotations,
      tile.name,
      tile.subBlocks,
      tile.vprConfig
    )
  }
}
