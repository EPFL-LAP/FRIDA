package arch

object MuxExpliciter {
  def partitionDirects(subModeLinks: Seq[Link]): (Seq[Seq[Direct]], Seq[Link]) = {
    assert(
      !subModeLinks.exists(
        l => !l.isInstanceOf[Direct] && !l.isInstanceOf[CLK]
      )
    )

    val directInterconnects = subModeLinks.collect {
      case d: Direct => d
    }

    /* Group direct connections by sink ID and only keep those that have more than one common sink */
    val (implicitMuxs, keptDirects) = directInterconnects.groupBy(_.dstLoc).values.partition(_.size >= 2)

    (implicitMuxs.toSeq, keptDirects.flatten.toSeq)
  }

  def pbWithExplicitMuxes(
      pb: PbType,
      nSubPbs: Seq[PbType],
      subModeLinks: Seq[Link]
  ): PbType = {
    /* Challenge is filter out direct connections in the old block that were transformed into a mux */
    val (directsAsMux, nonMuxes) = partitionDirects(subModeLinks)

    /* For each detected mux */
    val muxes = directsAsMux.map {
      directsOneMux =>
        {
          assert(directsOneMux.size > 1)

          val sources = directsOneMux.map(_.srcLoc).toList

          Mux(sources, directsOneMux.head.dstLoc, None)
        }
    }

    val nLinks = (pb.links ++ nonMuxes ++ muxes)

    pb match {
      case p: PrimPb => {
        PrimPb(p.bi, nLinks, Nil, p.c, p.annotations, p.name, p.prim, p.pinMap, p.dagIds)
      }

      case p: InterPb => {
        InterPb(p.bi, nLinks, Nil, p.c, p.annotations, p.name, nSubPbs)
      }

      case p: RootPb => {
        RootPb(p.bi, nLinks, Nil, p.c, p.annotations, p.name, nSubPbs, p.vprConfig)
      }
    }
  }

  def explicitInBlock(pb: PbType): (PbType, List[Link]) = {
    pb match {
      case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
        (PrimPb(bi, links, Nil, c, annotations, name, prim, pinMap, dagIds), modeLinks)
      }

      case other => {
        val (nSubPbs, subModeLinks) = pb.subBlocks.map(explicitInBlock(_)).unzip
        (pbWithExplicitMuxes(pb, nSubPbs, subModeLinks.flatten), pb.modeLinks)
      }
    }
  }

  def verify(root: RootPb): Unit = {
    def rec(pb: PbType): Unit = {
      pb.subBlocks.map(rec(_))

      assert(pb.modeLinks.isEmpty)
    }

    rec(root)
  }

  def apply(tile: RootPb): RootPb = {
    val nRoot = explicitInBlock(tile)._1.asInstanceOf[RootPb]
    verify(nRoot)

    nRoot
  }
}
