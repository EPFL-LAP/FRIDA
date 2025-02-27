package dummy

import packerv2._
import arch._
import frontend.GlobalParamsInst
import crkt.PortNodeID
import dummy.InsertDummiesArch.lDummyS

import collection.mutable.{Map => MMap}
import collection.mutable.{Set => MSet}
import crkt.ElasticGraph

object InsertDummiesImplementedMolecule {
  def getWrappedPrimNames(mol: ImplementedMolecule): Map[String, PrimPb] = {
    def rec(opened: List[PbType], acc: List[PrimPb]): List[PrimPb] = {
      if (opened.isEmpty) {
        acc
      } else {
        val pb = opened.head

        pb match {
          case primPb @ PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
            if (prim.physicalInfo.extended) {
              rec(opened.tail, primPb :: acc)
            } else {
              rec(opened.tail, acc)
            }
          }

          case other => rec(opened.tail ++ pb.subBlocks, acc)
        }
      }
    }

    rec(mol.tile :: Nil, Nil)
      .map(
        pb => (pb.name, pb)
      )
      .toMap
  }

  def toWrappedLoc(pbLoc: PbLoc, wrappedPrims: Set[String]): PbLoc = {
    if (wrappedPrims.contains(pbLoc.pbName)) {
      PbLoc(pbLoc.pbName + InsertDummiesArch.wrapperS, pbLoc.pin)
    } else {
      pbLoc
    }
  }

  def renameWrapperLocs(mol: ImplementedMolecule, wrappedPrims: Map[String, PrimPb]): Map[PbLoc, Option[Producer]] = {
    mol.locMap.map {
      (loc, prod) =>
        {
          val nLoc = toWrappedLoc(loc, wrappedPrims.keySet)

          prod match {
            case None => (nLoc, prod)
            case Some(SourceProducer(srcLoc, value)) => {
              (nLoc, Some(SourceProducer(toWrappedLoc(srcLoc, wrappedPrims.keySet), value)))
            }

            case Some(LinkProducer(srcLoc, link)) => {
              val nLink = link match {
                case Direct(srcLoc, dstLoc, delay) => {
                  val nSrc = toWrappedLoc(srcLoc, wrappedPrims.keySet)
                  val nDst = toWrappedLoc(dstLoc, wrappedPrims.keySet)

                  Direct(nSrc, nDst, delay)
                }

                case Mux(sources, dstLoc, delay) => {
                  val nSources = sources.map {
                    srcLoc =>
                      {
                        toWrappedLoc(srcLoc, wrappedPrims.keySet)
                      }
                  }

                  val nDst = toWrappedLoc(dstLoc, wrappedPrims.keySet)

                  Mux(nSources, nDst, delay)
                }

                case clk: CLK => clk
              }

              (nLoc, Some(LinkProducer(toWrappedLoc(srcLoc, wrappedPrims.keySet), nLink)))
            }
          }
        }
    }
  }

  def getExtDummyLink(loc: PbLoc, wrappedPb: PbType): (Direct, PbLoc) = {
    val candidateLinks = wrappedPb.links
      .collect {
        case d: Direct => d
      }
      .filter(
        d => (d.srcLoc == loc) || (d.dstLoc == loc)
      )
    assert(candidateLinks.size == 1)

    val d = candidateLinks.head

    val targetLoc = if (d.srcLoc == loc) d.dstLoc else d.srcLoc

    (d, targetLoc)
  }

  def getIntDummyLink(extLoc: PbLoc, wrappedPb: PbType): List[Direct] = {
    val intLoc = PbLoc(extLoc.pbName, extLoc.pin.flipped)

    val candidateLinks = wrappedPb.links
      .collect {
        case d: Direct => d
      }
      .filter(
        d => (d.srcLoc == intLoc) || (d.dstLoc == intLoc)
      )
    assert(candidateLinks.size == 1)

    val d = candidateLinks.head

    val intDummyLoc = PbLoc(extLoc.pbName, extLoc.pin.flipped.asDummy)
    val candidateDummyLinks = wrappedPb.links
      .collect {
        case d: Direct => d
      }
      .filter(
        d => (d.srcLoc == intDummyLoc) || (d.dstLoc == intDummyLoc)
      )

    if (candidateDummyLinks.isEmpty) {
      d :: Nil
    } else {
      assert(candidateDummyLinks.size == 1)

      d :: candidateDummyLinks.head :: Nil
    }
  }

  def getExtDummyValue(value: PortNodeID, dummyLoc: PbLoc): PortNodeID = {
    if (dummyLoc.pbName.contains(InsertDummiesArch.lDummyS)) {
      PortNodeID(value.nodeName + InsertDummiesArch.lDummyS, value.pId, value.loc)
    } else {
      assert(dummyLoc.pbName.contains(InsertDummiesArch.rDummyS))
      PortNodeID(value.nodeName + InsertDummiesArch.rDummyS, value.pId, value.loc)
    }
  }

  def getExternalLocMap(pinMappings: List[PinMapping], wrappedPb: PbType): List[PinMapping] = {
    pinMappings.map {
      case pm @ PinMapping(loc, prod) => {
        val (dummyLink, dummyLoc) = getExtDummyLink(loc, wrappedPb)

        prod match {
          case None => {
            pm :: PinMapping(dummyLoc, None) :: Nil
          }

          case Some(SourceProducer(srcLoc, value)) => {
            assert(loc == srcLoc)

            val nValue = getExtDummyValue(value, dummyLoc)

            val nSrcMapping = PinMapping(dummyLoc, Some(SourceProducer(dummyLoc, nValue)))
            val nLocMapping = PinMapping(loc, Some(LinkProducer(dummyLoc, dummyLink)))

            nSrcMapping :: nLocMapping :: Nil
          }

          case Some(LinkProducer(_, _)) => {
            val nMapping = PinMapping(dummyLoc, Some(LinkProducer(loc, dummyLink)))

            pm :: nMapping :: Nil
          }
        }
      }
    }.flatten
  }

  def getWrappedNodeName(pinMappings: List[PinMapping]): String = {
    val dummySuffixs = Set(InsertDummiesArch.lDummyS, InsertDummiesArch.rDummyS)

    val candidateNames = pinMappings
      .map(_.prod)
      .flatten
      .collect {
        case sp: SourceProducer => sp
      }
      .map(_.value.nodeName)
      .toSet
      .filter(!dummySuffixs.contains(_))

    assert(candidateNames.size == 1)

    candidateNames.head
  }

  def getInternalMapping(intLink: Direct, wrappedNodeName: String, origLoc: Int): List[PinMapping] = {
    val srcLocName = if (intLink.srcLoc.pbName.contains(InsertDummiesArch.lDummyS)) {
      wrappedNodeName + InsertDummiesArch.lDummyS
    } else if (intLink.srcLoc.pbName.contains(InsertDummiesArch.rDummyS)) {
      wrappedNodeName + InsertDummiesArch.rDummyS
    } else {
      wrappedNodeName
    }

    val value = PortNodeID(srcLocName, intLink.srcLoc.pin.id, origLoc)
    val sp = SourceProducer(intLink.srcLoc, value)

    val lp = LinkProducer(intLink.srcLoc, intLink)

    val srcMap = PinMapping(intLink.srcLoc, Some(sp))
    val dstMap = PinMapping(intLink.dstLoc, Some(lp))

    srcMap :: dstMap :: Nil
  }

  def getInternalLocMap(
      mol: ImplementedMolecule,
      pinMappings: List[PinMapping],
      wrappedPb: PbType,
      wrappedNodeName: String
  ): List[PinMapping] = {
    val dummySuffixs = Set(InsertDummiesArch.lDummyS, InsertDummiesArch.rDummyS)

    pinMappings
      .map(
        pm => (pm.loc, pm.prod)
      )
      .filter(_._2.nonEmpty)
      .filter(
        (loc, _) =>
          dummySuffixs.exists(
            s => loc.pbName.contains(s)
          )
      )
      .map {
        (loc, prod) =>
          {
            prod match {
              case Some(SourceProducer(srcLoc, value)) => {
                val dLinks = getIntDummyLink(loc, wrappedPb)

                dLinks.map(getInternalMapping(_, wrappedNodeName, value.loc)).flatten
              }

              case Some(LinkProducer(srcLoc, link)) => {
                val dLinks = getIntDummyLink(loc, wrappedPb)
                val intLink = dLinks.filter(_.srcLoc.pin.id.dummy == Regular)
                assert(intLink.size == 1)

                val origLoc =
                  if (
                    dummySuffixs.exists(
                      s => intLink.head.srcLoc.pbName.contains(s)
                    )
                  ) {
                    mol.locToValue(intLink.head.dstLoc).loc
                  } else {
                    mol.locToValue(intLink.head.srcLoc).loc
                  }

                dLinks.map(getInternalMapping(_, wrappedNodeName, origLoc)).flatten
              }

              case None => ???
            }
          }
      }
      .flatten
  }

  def locToVal(primName: String, archPin: Pin, valueName: String, valueLoc: Int): (PbLoc, PortNodeID) = {
    (PbLoc(primName, archPin), PortNodeID(valueName, archPin.id, valueLoc))
  }

  // TODO cleanup this function....
  def getNewLocToValue(
      lDummy: PrimPb,
      dPrim: PrimPb,
      rDummy: PrimPb,
      locToValue: Map[PbLoc, PortNodeID],
      wrappedNodeName: String
  ): List[(PbLoc, PortNodeID)] = {
    val wrappedLDummyNodeName = wrappedNodeName + InsertDummiesArch.lDummyS
    val wrappedRDummyNodeName = wrappedNodeName + InsertDummiesArch.rDummyS

    locToValue
      .filter(_._2.nodeName == wrappedNodeName)
      .map {
        (loc, value) =>
          {
            val initLoc = (PbLoc(dPrim.name, loc.pin), value)

            val expanderLocs = loc.pin.id.pt match {
              case PTInput => {
                loc.pin.id.pmw.pb match {
                  case Rdy => {
                    (locToVal(rDummy.name, loc.pin.flipped, wrappedRDummyNodeName, value.loc)
                      :: locToVal(rDummy.name, loc.pin, wrappedRDummyNodeName, value.loc)
                      :: Nil)
                  }
                  case other => {
                    (locToVal(lDummy.name, loc.pin.flipped, wrappedLDummyNodeName, value.loc)
                      :: locToVal(lDummy.name, loc.pin, wrappedLDummyNodeName, value.loc)
                      :: Nil)
                  }
                }
              }

              case PTOutput => {
                loc.pin.id.pmw.pb match {
                  case Rdy => {
                    (locToVal(lDummy.name, loc.pin.flipped, wrappedLDummyNodeName, value.loc)
                      :: locToVal(lDummy.name, loc.pin, wrappedLDummyNodeName, value.loc)
                      :: Nil)
                  }
                  case other => {
                    (locToVal(rDummy.name, loc.pin.flipped, wrappedRDummyNodeName, value.loc)
                      :: locToVal(rDummy.name, loc.pin, wrappedRDummyNodeName, value.loc)
                      :: Nil)
                  }
                }
              }

              case other => scala.sys.error("Expected port direction.")
            }

            val nDummyLocs = if (dPrim.bi.ports.contains(loc.pin.id.toDummy)) {
              loc.pin.id.pt match {
                case PTInput => {
                  loc.pin.id.pmw.pb match {
                    case Rdy => {
                      (locToVal(dPrim.name, loc.pin.asDummy, wrappedNodeName, value.loc)
                        :: locToVal(rDummy.name, loc.pin.asDummy.flipped, wrappedRDummyNodeName, value.loc)
                        :: Nil)
                    }
                    case other => {
                      (locToVal(dPrim.name, loc.pin.asDummy, wrappedNodeName, value.loc)
                        :: locToVal(lDummy.name, loc.pin.asDummy.flipped, wrappedLDummyNodeName, value.loc)
                        :: Nil)
                    }
                  }
                }

                case PTOutput => {
                  loc.pin.id.pmw.pb match {
                    case Rdy => {
                      (locToVal(dPrim.name, loc.pin.asDummy, wrappedNodeName, value.loc)
                        :: locToVal(lDummy.name, loc.pin.asDummy.flipped, wrappedLDummyNodeName, value.loc)
                        :: Nil)
                    }
                    case other => {
                      (locToVal(dPrim.name, loc.pin.asDummy, wrappedNodeName, value.loc)
                        :: locToVal(rDummy.name, loc.pin.asDummy.flipped, wrappedRDummyNodeName, value.loc)
                        :: Nil)
                    }
                  }
                }

                case other => scala.sys.error("Expected port direction.")
              }
            } else {
              Nil
            }

            initLoc :: (expanderLocs ++ nDummyLocs)
          }
      }
      .flatten
      .toList
  }

  def insertDummyLocs(
      params: GlobalParamsInst,
      prim: PrimPb,
      pinMappings: List[PinMapping],
      mol: ImplementedMolecule
  ): (List[PinMapping], List[(PrimPb, String)], List[(PbLoc, PortNodeID)]) = {
    val wrappedPb = InsertDummiesArch.updatePb(params, prim)

    val lDummy = wrappedPb.subBlocks.head.asInstanceOf[PrimPb]
    val dPrim = wrappedPb.subBlocks.tail.head.asInstanceOf[PrimPb]
    val rDummy = wrappedPb.subBlocks.tail.tail.head.asInstanceOf[PrimPb]

    val wrappedNodeName = getWrappedNodeName(pinMappings)
    val wrappedLDummyNodeName = wrappedNodeName + InsertDummiesArch.lDummyS
    val wrappedRDummyNodeName = wrappedNodeName + InsertDummiesArch.rDummyS

    val externalLocMap = getExternalLocMap(pinMappings, wrappedPb)
    val internalLocMap = getInternalLocMap(mol, externalLocMap, wrappedPb, wrappedNodeName)

    val nMappings = List(
      (lDummy -> wrappedLDummyNodeName),
      (dPrim -> wrappedNodeName),
      (rDummy -> wrappedRDummyNodeName)
    )

    val nLocToValue = getNewLocToValue(lDummy, dPrim, rDummy, mol.locToValue, wrappedNodeName)

    (internalLocMap ++ externalLocMap, nMappings, nLocToValue)
  }

  def updateValueMap(mol: ImplementedMolecule, nValueMap: List[(PbLoc, PortNodeID)]): Map[PbLoc, PortNodeID] = {
    val updatedValues = nValueMap.map(_._2).toSet

    val keptValues = mol.locToValue.filter {
      (loc, value) =>
        {
          !updatedValues.contains(value)
        }
    }

    keptValues ++ nValueMap
  }

  def insertDummies(
      params: GlobalParamsInst,
      locMap: Map[PbLoc, Option[Producer]],
      wrappedPrims: Map[String, PrimPb],
      mol: ImplementedMolecule
  ): (Map[PbLoc, Option[Producer]], List[(PrimPb, String)], Map[PbLoc, PortNodeID]) = {
    val (nLocMap, nodeMap, valueMap) = locMap
      .map(
        (loc, prod) => PinMapping(loc, prod)
      )
      .groupBy(_.loc.pbName)
      .map {
        (locName, pinMappings) =>
          {
            val origName = locName.replace(InsertDummiesArch.wrapperS, "")

            if (!locName.contains(InsertDummiesArch.wrapperS) || pinMappings.forall(_.prod.isEmpty)) {
              (pinMappings, Nil, Nil)
            } else {
              insertDummyLocs(params, wrappedPrims(origName), pinMappings.toList, mol)
            }
          }
      }
      .unzip3

    val nValueMap = updateValueMap(mol, valueMap.flatten.toList)

    (
      nLocMap.flatten
        .map(
          pinMap => (pinMap.loc, pinMap.prod)
        )
        .toMap,
      nodeMap.flatten.toList,
      nValueMap
    )
  }

  def fixupInputSourceProducers(
      tile: RootPb,
      locMap: Map[PbLoc, Option[Producer]],
      dummyG: ElasticGraph
  ): Map[PbLoc, Option[Producer]] = {
    locMap.map {
      (loc, prod) =>
        {
          if (loc.pbName == tile.name) {
            prod match {
              case Some(SourceProducer(srcLoc, value)) => {
                val srcPort = dummyG(value.nodeName).getPin(Pin(value.pId, value.loc))

                val isValueDummy = (
                  (srcPort.distPorts.size == 1)
                    &&
                      ((srcPort.distPorts.head._2.thisNode == (srcPort.thisNode + InsertDummiesArch.lDummyS))
                        || (srcPort.distPorts.head._2.thisNode == (srcPort.thisNode + InsertDummiesArch.rDummyS)))
                )

                if (isValueDummy) {
                  val nNodeName = srcPort.distPorts.head._2.thisNode
                  val nSp = SourceProducer(srcLoc, PortNodeID(nNodeName, value.pId, value.loc))

                  (loc, Some(nSp))
                } else {
                  (loc, prod)
                }
              }

              case other => (loc, prod)
            }
          } else {
            (loc, prod)
          }
        }
    }
  }

  def addMissingOpenLocs(mol: ImplementedMolecule): ImplementedMolecule = {
    def rec(pbs: MSet[PbType], acc: MMap[PbLoc, Option[Producer]]): Map[PbLoc, Option[Producer]] = {
      if (pbs.isEmpty) {
        acc.toMap
      } else {
        val pb = pbs.head

        val pbMap = pb.bi.ports.map(_._2).map(_.toPins()).flatten.map {
          pin =>
            {
              val loc = PbLoc(pb.name, pin)

              if (mol.locMap.contains(loc)) {
                (loc -> mol.locMap(loc))
              } else {
                (loc -> None)
              }
            }
        }

        pbs -= pb
        pbs ++= pb.subBlocks
        acc ++= pbMap

        rec(pbs, acc)
      }
    }

    val fullLocMap = rec(MSet(mol.tile), MMap())

    ImplementedMolecule(mol.tile, fullLocMap.toMap, mol.mm, mol.blockMap, mol.primMap, mol.locToValue)
  }

  def apply(params: GlobalParamsInst, mol: ImplementedMolecule, dummyG: ElasticGraph): ImplementedMolecule = {
    val nTile = InsertDummiesArch(params, mol.tile).asInstanceOf[RootPb]
    val wrappedPrims = getWrappedPrimNames(mol)

    val wrappedLocMap = renameWrapperLocs(mol, wrappedPrims)
    val (fullLocMap, dummyMappings, fullValueMap) = insertDummies(params, wrappedLocMap, wrappedPrims, mol)

    val finalLocMap = fixupInputSourceProducers(nTile, fullLocMap, dummyG)

    val dummyBlockMap = dummyMappings
      .map(
        (primPb, nName) => (primPb.prim, nName)
      )
      .toSet
    val dummyPrimMap = dummyMappings
      .map(
        (primPb, nName) => (primPb.name, nName)
      )
      .toSet

    val fullBlockMap = mol.blockMap.filter(
      (b, nName) => !dummyMappings.exists(_._2 == nName)
    ) ++ dummyBlockMap
    val fullPrimMap = mol.primMap.filter(
      (prim, nName) => !dummyMappings.exists(_._2 == nName)
    ) ++ dummyPrimMap

    val nMol = addMissingOpenLocs(
      ImplementedMolecule(nTile, finalLocMap, None, fullBlockMap, fullPrimMap, fullValueMap)
    )

    assert(ImplementMolecule.allLocsExist(mol))

    nMol
  }
}
