package packerv2

import arch._
import frontend.GlobalParamsInst
import archs.MuxConfig
import archs.MuxConfigParams

import collection.mutable.{Map => MMap}
import collection.mutable.{Set => MSet}

// TODO add equivalent ports annotations

object RRGToPbType {
  def getPrimPbs(rrg: RRG): Set[PrimPb] = {
    rrg.prims.filter(!_.isIo).map {
      pn =>
        {
          val pinMap = pn.pinToSrcSink
            .map(_._1)
            .map(
              pin => (pin, pin)
            )
            .toMap

          PrimPb(
            pn.block.blockInterface,
            Nil,
            Nil,
            TileAnd,
            pn.annos,
            pn.name,
            pn.block,
            pinMap,
            pn.dagIds
          )
        }
    }
  }

  def getIOs(rrg: RRG): Set[PbLoc] = {
    rrg.prims
      .filter(_.isIo)
      .map {
        pn =>
          {
            pn.pinToSrcSink
              .map(_._1)
              .map(
                pin => PbLoc(rrg.molName, pin)
              )
          }
      }
      .flatten
  }

  def getMuxs(rrg: RRG): List[Mux] = {
    rrg.rrs
      .collect {
        case cmux: RRCMux => cmux
      }
      .toList
      .map {
        cmux =>
          {
            val preds = rrg.preds(cmux).map {
              case pin: RRPin   => PbLoc(rrg.getPrim(pin).name, Pin(pin.rrType, pin.loc))
              case cmux: RRCMux => PbLoc(cmux.name, Pin(cmux.rrType, 0)) // TODO double check legality of this in VPR
              case other        => ???
            }

            val succs = rrg.succs(cmux).map {
              case pin: RRPin   => PbLoc(rrg.getPrim(pin).name, Pin(pin.rrType, pin.loc))
              case cmux: RRCMux => PbLoc(cmux.name, Pin(cmux.rrType, 0)) // TODO double check legality of this in VPR
              case other        => ???
            }

            if (succs.size == 1) {
              Mux(preds.toList, succs.head, None)
            } else {
              succs.map {
                loc =>
                  {
                    PbLoc(cmux.name, Pin(cmux.rrType, 0))
                  }
              }
              ???
            }
          }
      }
  }

  def getDirect(rrg: RRG): List[Direct] = {
    rrg.rrs
      .collect {
        case pin: RRPin => pin
      }
      .toList
      .filter(_.rrType.pt == PTOutput)
      .map {
        oPin =>
          {
            val srcPrim = rrg.getPrim(oPin)
            rrg
              .succs(oPin)
              .collect {
                case pin: RRPin => pin
              }
              .map {
                iPin =>
                  {
                    val dstPim = rrg.getPrim(iPin)
                    Direct(
                      PbLoc(srcPrim.name, Pin(iPin.rrType, iPin.loc)),
                      PbLoc(dstPim.name, Pin(oPin.rrType, oPin.loc)),
                      None
                    )
                  }
              }
          }
      }
  }.flatten

  def getLinks(rrg: RRG): List[Link] = {
    getMuxs(rrg) ++ getDirect(rrg)
  }

  def absorbBypassMuxs(prims: Set[PrimPb], rrg: RRG): Set[InterPb] = {
    prims.map {
      primPb =>
        {
          val bypassingMuxs = rrg.rrs
            .collect {
              case cmux: RRCMux => cmux
            }
            .filter(
              rrg
                .getBypassing(_)
                .fold(false)(
                  prim => prim.name == primPb.name
                )
            )

          if (bypassingMuxs.isEmpty) {
            primPb
          } else {
            ???
          }
          ???
        }
    }
  }

  def apply(params: GlobalParamsInst, rrg: RRG): PbType = {
    val prims = getPrimPbs(rrg)
    val ios = getIOs(rrg)

    val links = getLinks(rrg)

    val topBiPorts = ios
      .map(_._2)
      .groupBy(_.id)
      .map(
        (id, pins) => (id, BlockPort(id, pins.size, Set()))
      )
      .toMap
    val topBi = BlockInterface(topBiPorts, rrg.molName, true)

    // TOOD the vpr config should come from the toplevel
    // TODO we assume there is only one molecule per RootPb here...
    println(rrg.molName)
    RootPb(topBi, links, Nil, TileAnd, Set(), rrg.molName, prims.toList, ???)
  }
}
