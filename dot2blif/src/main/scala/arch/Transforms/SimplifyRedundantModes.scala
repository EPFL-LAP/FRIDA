package arch

object SimplifyRedundantModes {
  def apply(pb: RootPb): RootPb = {
    def rec(pb: PbType, update: Boolean): (PbType, List[Link]) = {
      pb match {
        case primPb @ PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
          if (update) {
            (PrimPb(bi, links, Nil, TileAnd, annotations, name, prim, pinMap, dagIds), modeLinks)
          } else {
            (pb, Nil)
          }
        }

        case nonLeaf: NonLeafPb => {
          val simplifyChild = (pb.subBlocks.head.c == TileOr) && (pb.subBlocks.size == 1)
          val (recPbs, recInter) = pb.subBlocks.map(rec(_, simplifyChild)).unzip

          val nInterconnect = pb.links ++ recInter.flatten

          if (update) {
            if (pb.c == TileOr) {
              nonLeaf match {
                case InterPb(bi, links, modeLinks, c, annotations, name, _) => {
                  (InterPb(bi, nInterconnect, Nil, TileAnd, annotations, name, recPbs), modeLinks)
                }

                case RootPb(bi, links, modeLinks, c, annotations, name, _, vprConfig) => {
                  (RootPb(bi, nInterconnect, Nil, TileAnd, annotations, name, recPbs, vprConfig), modeLinks)
                }
              }
            } else {
              // TODO Don't touch the other ones for now
              // TODO flatten could also be implemented here
              ???
            }
          } else {
            nonLeaf match {
              case InterPb(bi, links, modeLinks, c, annotations, name, _) => {
                (InterPb(bi, nInterconnect, modeLinks, c, annotations, name, recPbs), Nil)
              }

              case RootPb(bi, links, modeLinks, c, annotations, name, _, vprConfig) => {
                (RootPb(bi, nInterconnect, modeLinks, c, annotations, name, recPbs, vprConfig), Nil)
              }
            }
          }
        }
      }
    }

    rec(pb, false)._1.asInstanceOf[RootPb]
  }
}
