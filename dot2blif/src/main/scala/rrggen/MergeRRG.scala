package rrggen

import util.Util
import arch.PbType
import arch.Pin
import arch.RootPb
import arch.HSValid
import arch.HSReady
import arch.BlockPort
import core.Namer
import arch.Impl
import arch.Hs
import arch.D
import arch.Vld
import arch.Rdy
import arch.HSType
import arch.PTInput
import arch.PortType
import arch.PTUndef
import arch.PTOutput
// import packer.PackPrinter.clkPP
// import packer.PackPrinter.clkIoName
import frontend.GlobalParamsInst
import collection.mutable.{Set => MSet}
import collection.mutable.{Map => MMap}

case class PinID(var id: Int) {
  def incr(): Unit = {
    id = id + 1
  }
}

case class PinLocationInfo(rrgId: Int, blockTypeId: Int, ptc: Int)
case class NodeInfo(rrgId: Int, nodeId: Int)

case class NodeLocInfo(blockTypeId: Int, ptc: Int, nType: String)

object MergeRRG {
  // Add number of channels of each architecture
  def mergeChannels(rrGraphs: List[scala.xml.Elem]): xml.Elem = {
    // Assumes uniform
    val maxChanWidth = rrGraphs
      .map {
        rrg =>
          {
            (rrg \ "channels" \ "channel" \ "@chan_width_max").text.toInt
          }
      }
      .reduceOption(_ + _)
      .getOrElse(0)

    val nMaxChan =
      <channel chan_width_max={maxChanWidth.toString}
                 x_max={maxChanWidth.toString} x_min={maxChanWidth.toString}
                 y_max={maxChanWidth.toString} y_min={maxChanWidth.toString}/>

    val nChanInfo = rrGraphs
      .map {
        rrg =>
          {
            ((rrg \\ "channels").head.child
              .filter(
                n => !(n.label == "channel")
              ))
              .filter(
                n => (n \@ "index").nonEmpty
              )
              .map {
                list =>
                  {
                    val index = (list \@ "index").toInt
                    val info = (list \@ "info").toInt

                    if (list.label == "x_list") {
                      (("x", index), info)
                    } else {
                      (("y", index), info)
                    }
                  }
              }
          }
      }
      .flatten
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2))
      )
      .map {
        case ((a, b), c) => {
          (a, b, c.sum)
        }
      }
      .toList
      .sortBy(
        (t, index, info) => (t + index)
      )
      .map {
        case ("x", index, info) =>
          <x_list index={index.toString()} info={info.toString()}/>
        case ("y", index, info) => <y_list index={index.toString()} info={info.toString()}/>
      }

    <channels>
      {nMaxChan}
      {nChanInfo}
    </channels>
  }

  // Returns rrg id, switch id to new switch id
  def mergeSwitchs(rrgs: List[RRGInfo]): (xml.Elem, Map[(Int, Int), Int]) = {
    var swId = 2

    val (nSwitches, nMappings) = rrgs.zipWithIndex
      .map {
        (rrg, i) =>
          {
            (rrg.rrg \ "switches").head.child
              .filter(_ \@ "name" != "__vpr_delayless_switch__")
              .filter(_ \@ "name" != "input_mux")
              .filter(
                n => (n \@ "id").nonEmpty
              )
              .map {
                switch =>
                  {
                    val child = switch.child // Unchanged timing properties
                    val name = switch \@ "name"
                    val stype = switch \@ "type"
                    val oldId = (switch \@ "id").toInt

                    val nSwitch = {
                      <switch id={swId.toString()} name={name} type={stype}>
                  {child}
                </switch>
                    }

                    val nMapping = (i, oldId) -> swId
                    swId = swId + 1

                    (nSwitch, nMapping)
                  }
              }
          }
      }
      .flatten
      .unzip

    val delayLessSwitch = {
      <switch id="0" name="__vpr_delayless_switch__" type="mux">
        <timing/>
        <sizing buf_size="0" mux_trans_size="0"/>
      </switch>
    }

    val inputMux = {
      <switch id="1" name="input_mux" type="mux">
        <timing Tdel="0"/>
        <sizing buf_size="0" mux_trans_size="0"/>
      </switch>
    }

    val allSwitchs = delayLessSwitch :: inputMux :: nSwitches
    val allSwitchsXml = {
      <switches>
        {allSwitchs}
      </switches>
    }

    val defaultMappings = (0 until rrgs.size).map(
      i => ((i, 0) -> 0)
    )
    val allMappings = (nMappings ++ defaultMappings).toMap

    (allSwitchsXml, allMappings)
  }

  // Segments have no timing modeling in our case
  // We duplicate per architecture for simplicity, but in principle they are all the same
  def mergeSegments(rrgs: List[RRGInfo]): (xml.Elem, Map[(Int, Int), Int]) = {
    var segId = 0

    val (nSegments, nMappings) = rrgs.zipWithIndex
      .map {
        (rrg, i) =>
          {
            (rrg.rrg \ "segments").head.child
              .filter(
                n => (n \@ "id").nonEmpty
              )
              .map {
                segment =>
                  {
                    val child = segment.child // Unchanged timing properties
                    val name = segment \@ "name"
                    val oldId = (segment \@ "id").toInt

                    val nSegment = {
                      <segment id={segId.toString()} name={name}>
                {child}
              </segment>
                    }

                    val nMapping = (i, oldId) -> segId
                    segId = segId + 1

                    (nSegment, nMapping)
                  }
              }
          }
      }
      .flatten
      .unzip

    val segmentsXml = {
      <segments>
        {nSegments}
      </segments>
    }

    (segmentsXml, nMappings.toMap)
  }

  def getPinDir(bp: BlockPort, hsType: Option[HSType]): PortType = {
    hsType match {
      case None          => bp.pt
      case Some(HSValid) => bp.pt
      case Some(HSReady) => PortType.not(bp.pt)
    }
  }

  def getPinClass(
      tile: PbType,
      pin: Pin,
      c: Int,
      totalCapacity: Int
  )(implicit pinID: PinID): (xml.Elem, (String, Int)) = {
    val pName = Namer(pin)

    val pinName = if (totalCapacity == 1) {
      tile.name + "." + pName + "[0]"
    } else {
      tile.name + "[" + c + "]" + "." + pName + "[0]"
    }

    val ptS = pin.id.pt match {
      case PTInput  => "INPUT"
      case PTOutput => "OUTPUT"
      case PTUndef  => scala.sys.error("Unexpected pin direction.")
    }

    val ptc = pinID.id.toString()

    val e = <pin_class type={ptS}><pin ptc={ptc}>{pinName}</pin></pin_class>

    val pinToId = (pinName -> pinID.id)
    pinID.incr()

    (e, pinToId)
  }

  // TileName -> Id
  def getTileIds(tiles: Map[String, PbType]): Map[String, Int] = {
    tiles
      .map(_._1)
      .zipWithIndex
      .toMap
      .map(
        (tName, id) => (tName, id + 1)
      )
  }

  // def getBlockTypes(params: GlobalParamsInst): (xml.Elem, Map[String, Int]) = {
  //   val placeArch = params.buildDir + "/arch_.xml"
  // }

  def getArchInfo(tiles: Map[String, RootPb]): (xml.Elem, Map[String, Int]) = {
    val tileIds = getTileIds(tiles)

    val (blocks, pinToIds) = tiles
      .map(_._2)
      .map {
        tile =>
          {
            implicit val pinId = PinID(0)
            val config = tile.vprConfig

            val (pinXml, pinIds) = (0 until config.capacity)
              .map {
                c =>
                  {
                    val pins = (PTInput :: PTOutput :: Nil).map {
                      pt =>
                        {
                          tile.bi.ports
                            .filter(_._1.pt == pt)
                            .map(_._2)
                            .map(_.toPins())
                            .flatten
                            .toList
                            .sortBy(Namer(_))
                            .map {
                              pin =>
                                {
                                  getPinClass(tile, pin, c, config.capacity)
                                }
                            }
                        }
                    }.flatten

                    if (tile.hasClockPin()) {
                      val clkId = pinId.id.toString

                      val clkName = if (config.capacity == 1) {
                        tile.name + ".clk[0]"
                      } else {
                        tile.name + "[" + c + "]" + ".clk[0]"
                      }

                      val clkPin = <pin_class type="INPUT"><pin ptc={clkId}>{clkName}</pin></pin_class>

                      val clkPinToId = (clkName -> pinId.id)
                      pinId.incr()

                      val clkPair = (clkPin, clkPinToId)

                      pins.toSeq ++ Seq(clkPair)
                    } else {
                      pins.toSeq
                    }
                  }
              }
              .flatten
              .unzip

            val tileId = tileIds(tile.name).toString()
            val tileName = tile.name
            val height = config.height.toString()
            val width = config.width.toString()

            val blockT = <block_type id={tileId} name={tileName} height={height} width={width}>
            {pinXml}
          </block_type>

            (blockT, pinIds)
          }
      }
      .unzip

    val emptyBlock = <block_type height="1" id="0" name="EMPTY" width="1"></block_type>

    val allBlocks = emptyBlock :: blocks.toList

    val blocksXml = <block_types>
        {allBlocks}
      </block_types>

    (blocksXml, pinToIds.flatten.toMap)
  }

  // for each x,y loc, check which tile is there
  def getGridInfo(rrg: scala.xml.Node): Map[(Int, Int), Int] = {
    (rrg \ "grid").head.child
      .filter(
        n => (n \@ "block_type_id").nonEmpty
      )
      .map {
        gridLoc =>
          {
            val block_type_id = gridLoc \@ "block_type_id"
            val x = (gridLoc \@ "x").toInt
            val y = (gridLoc \@ "y").toInt

            ((x, y) -> block_type_id.toInt)
          }
      }
      .toMap
  }

  // Max rrg id to channels
  def rrgToChanWidth(rrgs: List[xml.Elem]): Map[Int, Int] = {
    rrgs.zipWithIndex.map {
      (rrg, i) =>
        {
          (i, (rrg \ "channels" \ "channel" \ "@chan_width_max").text.toInt)
        }
    }.toMap
  }

  // Returns a map from rrgId, tileName, ptc to new ptc
  def getPinMapping(
      rrgs: List[scala.xml.Elem],
      nPins: Map[String, Int]
  ): (Map[PinLocationInfo, Int], Set[PinLocationInfo]) = {
    val clockPins = MSet[PinLocationInfo]()

    val allPins = rrgs.zipWithIndex
      .map {
        (rrg, i) =>
          {
            (rrg \ "block_types").head.child.map {
              blockType =>
                {
                  val id = blockType \@ "id"

                  blockType.child
                    .filter(
                      pc => (pc \@ "type").nonEmpty
                    )
                    .map {
                      pinClass =>
                        {
                          pinClass.child
                            .filter(
                              p => (p \@ "ptc").nonEmpty
                            )
                            .map {
                              pin =>
                                {
                                  val ptc = pin \@ "ptc"
                                  val pinInfo = PinLocationInfo(i, id.toInt, ptc.toInt)

                                  if (pin.text contains "clk") {
                                    clockPins += (pinInfo)
                                  }

                                  (pinInfo -> nPins(pin.text))
                                }
                            }
                        }
                    }
                    .flatten
                }
            }.flatten
          }
      }
      .flatten
      .toMap

    (allPins, clockPins.toSet)
  }

  def getChanOffset(rrgLoc: Int, chanInfo: Map[Int, Int]): Int = {
    (0 until rrgLoc).map {
      i =>
        {
          chanInfo(i)
        }
    }.sum
  }

  // Returns (rrgId, nodeId) -> newNodeId
  def renameNodes(
      rrgs: List[xml.Elem],
      pins: Map[PinLocationInfo, Int],
      clockPins: Set[PinLocationInfo],
      gridToBlockId: Map[(Int, Int), Int]
  ): (xml.Elem, Map[NodeInfo, (Int, Boolean)]) = {
    val encountered = MSet[(Int, Int, Int, Int, Int, String)]()

    val chanInfo = rrgToChanWidth(rrgs)
    var nodeId = 0
    val clockLocToId = MMap[NodeLocInfo, Int]()

    val nSegments = (rrgs.head \ "segments").head.child
      .filter(
        n => (n \@ "id").nonEmpty
      )
      .size

    val (nNodes, nNodeMap) = rrgs.zipWithIndex
      .map {
        (rrg, i) =>
          {
            (rrg \ "rr_nodes").head.child
              .filter(
                n => (n \@ "id").nonEmpty
              )
              .map {
                node =>
                  {
                    val id = node \@ "id"
                    val nodeType = node \@ "type"
                    var subClockInfo: Option[NodeLocInfo] = None

                    val nChilds = node.child.map {
                      sub =>
                        {
                          if (sub.label contains "loc") {
                            val ptc = sub \@ "ptc"
                            val xhigh = sub \@ "xhigh"
                            val xlow = sub \@ "xlow"
                            val yhigh = sub \@ "yhigh"
                            val ylow = sub \@ "ylow"

                            if (nodeType contains "CHAN") {
                              val chanOffset = getChanOffset(i, chanInfo)
                              val nPtc = chanOffset + ptc.toInt

                              <loc ptc={nPtc.toString} xhigh={xhigh} xlow={xlow} yhigh={yhigh} ylow={ylow}/>
                            } else {
                              val blockId = gridToBlockId((xhigh.toInt, yhigh.toInt))
                              val pinLocInfo = PinLocationInfo(i, blockId, ptc.toInt)

                              pins
                                .get(pinLocInfo)
                                .fold {
                                  scala.sys.error("Found unknown pin name: " + pinLocInfo)
                                } {
                                  nPtc =>
                                    {
                                      val uniquePinId =
                                        (xlow.toInt, xhigh.toInt, ylow.toInt, yhigh.toInt, nPtc, nodeType)
                                      subClockInfo = if (clockPins contains pinLocInfo) {
                                        Some(NodeLocInfo(blockId, nPtc, nodeType))
                                      } else {
                                        None
                                      }

                                      if (nodeType contains "PIN") {
                                        val side = sub \@ "side"
                                        <loc ptc={nPtc.toString} side={side} xhigh={xhigh} xlow={xlow} yhigh={
                                          yhigh
                                        } ylow={ylow}/>
                                      } else {
                                        <loc ptc={nPtc.toString} xhigh={xhigh} xlow={xlow} yhigh={yhigh} ylow={ylow}/>
                                      }
                                    }
                                }
                            }
                          } else if ((nodeType contains "CHAN") && (sub.label contains "segment")) {
                            val segId = sub \@ "segment_id"
                            val nSegId = i * nSegments + segId.toInt

                            <segment segment_id={nSegId.toString()}/>
                          } else {
                            sub
                          }
                        }
                    }

                    val capacity = node \@ "capacity"
                    val direction = node \@ "direction"

                    val candidateNode = if (direction != "") {
                      <node id={nodeId.toString} type={nodeType} capacity={capacity} direction={direction}>
                      {nChilds}
                      </node>
                    } else {
                      <node id={nodeId.toString} type={nodeType} capacity={capacity}>
                      {nChilds}
                      </node>
                    }

                    val (nNode, nNodeMapping) = if (subClockInfo.nonEmpty) {
                      if (i == 0) {
                        val m = NodeInfo(i, id.toInt) -> (nodeId, true)

                        clockLocToId += (subClockInfo.get -> nodeId)
                        nodeId = nodeId + 1

                        (Some(candidateNode), m)
                      } else {
                        val m = NodeInfo(i, id.toInt) -> (clockLocToId(subClockInfo.get), true)
                        (None, m)
                      }
                    } else {
                      val m = NodeInfo(i, id.toInt) -> (nodeId, false)
                      nodeId = nodeId + 1
                      (Some(candidateNode), m)
                    }

                    (nNode, nNodeMapping)
                  }
              }
          }
      }
      .flatten
      .unzip

    val nNodesXml = <rr_nodes>
        {nNodes.flatten}
      </rr_nodes>

    (nNodesXml, nNodeMap.toMap)
  }

  def renameEdges(rrgs: List[xml.Elem], nodeInfo: Map[NodeInfo, (Int, Boolean)]): xml.Elem = {
    val nEdges = rrgs.zipWithIndex.map {
      (rrg, i) =>
        {
          (rrg \ "rr_edges").head.child
            .filter(
              n => (n \@ "sink_node").nonEmpty
            )
            .map {
              edge =>
                {
                  val sinkNode = edge \@ "sink_node"
                  val srcNode = edge \@ "src_node"
                  val switchId = edge \@ "switch_id"

                  assert(sinkNode.nonEmpty)
                  assert(srcNode.nonEmpty)

                  val (nSrc, isSrcClock) = nodeInfo(NodeInfo(i, srcNode.toInt))
                  val (nSink, isSinkClock) = nodeInfo(NodeInfo(i, sinkNode.toInt))

                  assert(
                    nSrc != nSink,
                    "Node connecting to itself(" + i + "): " + nSrc + ", " + nSink
                      + "; from: origSrc: " + srcNode + ", origSink: " + sinkNode
                  )

                  if ((i > 0) && isSrcClock && isSinkClock) {
                    None
                  } else {
                    Some(<edge sink_node={nSink.toString()} src_node={nSrc.toString()} switch_id={switchId}/>)
                  }
                }
            }
            .flatten
        }
    }

    <rr_edges>
      {nEdges}
    </rr_edges>
  }

  def apply(tiles: Map[String, RootPb], rrgs: List[RRGInfo]): scala.xml.Elem = {
    val nChannels = mergeChannels(rrgs.map(_._1))
    val (nSwitches, switchMap) = mergeSwitchs(rrgs)
    val (nSegments, segMap) = mergeSegments(rrgs)

    val (nBlockTypes, pinNameToId) = getArchInfo(tiles)

    val (pinMap, clockPins) = getPinMapping(rrgs.map(_._1), pinNameToId)
    val gridInfo = getGridInfo(rrgs.map(_._1).head)
    val nGrid = rrgs.map(_._1).head \ "grid" // Should do it on the handshake rrg

    val (nNodes, nodeMap) = renameNodes(rrgs.map(_._1), pinMap, clockPins, gridInfo)
    val nEdges = renameEdges(rrgs.map(_._1), nodeMap)

    val v = rrgs.map(_._1).head \@ "tool_version"

    <rr_graph tool_comment="Unified RRG" tool_name="vpr" tool_version={v}>
      {nChannels}
      {nSwitches}
      {nSegments}
      {nBlockTypes}
      {nGrid}
      {nNodes}
      {nEdges}
    </rr_graph>
  }

}
