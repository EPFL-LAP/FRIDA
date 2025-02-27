package printers

import arch._
import crkt._
import core.Namer
import core.AIo
import util.Util

object LegalizeIO {
  def insertOutpad(inPort: xml.Elem, prim: TBlock): (xml.Elem, xml.Elem) = {
    val wrapperName = inPort \@ "name"

    val fromOutpad = wrapperName + ".outpad"
    val outpadInterName = fromOutpad + "2outpad.outpad"

    val fromWrapper = Util.validVprName(prim.name) + "." + wrapperName
    val wrapperInterName = fromWrapper + "2" + fromOutpad

    val wrapper =
      <pb_type name={wrapperName} num_pb="1">
        <input name="outpad" num_pins="1"/>
        <pb_type name="outpad" num_pb="1" blif_model=".output">
          <input name="outpad" num_pins="1"/>
        </pb_type>
        <interconnect>
          <direct name={outpadInterName} input={fromOutpad} output="outpad.outpad"/>
        </interconnect>
      </pb_type>

    val wrapperIntercnnect =
      <direct name={wrapperInterName} input={fromWrapper} output={fromOutpad}/>

    (wrapper, wrapperIntercnnect)
  }

  def insertInpad(outPort: xml.Elem, prim: TBlock): (xml.Elem, xml.Elem) = {
    val wrapperName = outPort \@ "name"

    val toInpad = wrapperName + ".inpad"
    val inpadInterName = "inpad.inpad" + "2" + toInpad

    val toWrapper = Util.validVprName(prim.name) + "." + wrapperName
    val wrapperInterName = toInpad + "2" + toWrapper

    val wrapper =
      <pb_type name={wrapperName} num_pb="1">
        <output name="inpad" num_pins="1"/>
        <pb_type name="inpad" num_pb="1" blif_model=".input">
          <output name="inpad" num_pins="1"/>
        </pb_type>
        <interconnect>
          <direct name={inpadInterName} input="inpad.inpad" output={toInpad}/>
        </interconnect>
      </pb_type>

    val wrapperIntercnnect =
      <direct name={wrapperInterName} input={toInpad} output={toWrapper}/>

    (wrapper, wrapperIntercnnect)
  }

  def handleArchIoPrim(e: xml.Elem, prim: TBlock): xml.Elem = {
    val (wrappers, links) = e.child.collect {
      case in: xml.Elem if (in.label == "input")    => insertOutpad(in, prim)
      case out: xml.Elem if (out.label == "output") => insertInpad(out, prim)
    }.unzip

    val nInterconnect =
      <interconnect>
        {links}
      </interconnect>

    val nChilds = e.child ++ wrappers ++ (nInterconnect :: Nil)
    val nAttrs = e.attributes.remove("blif_model")

    e.copy(child = nChilds, attributes = nAttrs)
  }

  def traverseArch(e: xml.Elem, blocks: Map[String, TBlock]): xml.Elem = {
    val untouchedLabels = Set("inputs", "outputs", "interconnect")

    val nChild = e.child.map {
      case untouched: xml.Elem if (untouchedLabels contains untouched.label) => {
        untouched
      }

      case block: xml.Elem if (block.label == "pb_type") => {
        if (blocks.keySet contains (block \@ "name")) {
          val prim = blocks(block \@ "name")

          if (prim.annotations contains AIo) {
            handleArchIoPrim(block, prim)
          } else {
            block
          }
        } else {
          traverseArch(block, blocks)
        }
      }

      case other => other
    }

    e.copy(child = nChild)
  }

  def handlePackedInputIO(block: xml.Elem, prim: TBlock, port: xml.Node, g: ElasticGraph): xml.Elem = {
    val bName = block \@ "name"
    val wrapperName = (port \@ "name")
    val instance = wrapperName + "[0]"

    val primPort = Util.validVprName(prim.name) + "." + wrapperName
    val wrapperPort = wrapperName + ".outpad"
    val wrapperInter = primPort + "2" + wrapperPort
    val wrapperInterS = primPort + "->" + wrapperInter

    val padPort = "outpad.outpad"
    val padInter = wrapperPort + "2" + padPort
    val padInterS = wrapperPort + "->" + padInter

    val refNode = g.nodes(bName)

    val pin = BlockPortID.PortInstanceParser(wrapperName)
    val refP = refNode.ports(pin.id).filter(_.loc == pin.loc).head
    assert(refP.distPorts.size == 1)

    val prodP = refP.distPorts.head._2

    val primName = "out:" + Namer(prodP)

    <block name={bName} instance={instance} mode="default">
      <attributes/>
      <parameters/>
      <inputs>
        <port name="outpad">{wrapperInterS}</port>
      </inputs>
      <outputs>
      </outputs>
      <clocks/>
      <block name={primName} instance="outpad[0]">
        <attributes/>
        <parameters/>
        <inputs>
          <port name="outpad">{padInterS}</port>
        </inputs>
        <outputs>
        </outputs>
        <clocks/>
      </block>
    </block>
  }

  def handlePackedOutputIO(block: xml.Elem, prim: TBlock, port: xml.Node, g: ElasticGraph): xml.Elem = {
    val bName = block \@ "name"
    val wrapperName = (port \@ "name")
    val instance = wrapperName + "[0]"

    val primPort = Util.validVprName(prim.name) + "." + wrapperName
    val wrapperPort = wrapperName + ".inpad"

    val padPort = "inpad.inpad"
    val wrapperInter = padPort + "2" + wrapperPort
    val wrapperInterS = padPort + "->" + wrapperInter

    val primName = if (bName == "clk") {
      "clk"
    } else {
      val refNode = g.nodes(bName)
      val pin = BlockPortID.PortInstanceParser(wrapperName)
      val refP = refNode.ports(pin.id).filter(_.loc == pin.loc).head

      Namer(refP)
    }

    <block name={bName} instance={instance} mode="default">
      <attributes/>
      <parameters/>
      <inputs>
      </inputs>
      <outputs>
        <port name="inpad">{wrapperInterS}</port>
      </outputs>
      <clocks/>
      <block name={primName} instance="inpad[0]">
        <attributes/>
        <parameters/>
        <inputs>
        </inputs>
        <outputs>
          <port name="inpad">{primName}</port>
        </outputs>
        <clocks/>
      </block>
    </block>
  }

  def handlePackedIoPrim(e: xml.Elem, prim: TBlock, g: ElasticGraph): xml.Elem = {
    val untouchedLabels = Set("inputs", "attributes", "parameters", "clocks")

    val nIo = e.child.map {
      case untouched: xml.Elem if (untouchedLabels contains untouched.label) => untouched
      case output: xml.Elem if (output.label == "outputs") => {
        val nOutPorts = (output \ "port").map {
          port =>
            {
              if (port.text == "open") {
                port
              } else {
                val wrapperName = port \@ "name"
                val wrapperPort = wrapperName + ".inpad"
                val thisPort = Util.validVprName(prim.name) + "." + wrapperName

                val wrapperInter = wrapperPort + "2" + thisPort
                val wrapperInterS = wrapperPort + "->" + wrapperInter

                <port name={wrapperName}>{wrapperInterS}</port>
              }
            }
        }

        <outputs>
          {nOutPorts}
        </outputs>
      }

      case other => other
    }

    val ins = e.child.collect { case ins: xml.Elem if (ins.label == "inputs") => ins }
    val outs = e.child.collect { case ins: xml.Elem if (ins.label == "outputs") => ins }

    val (insOpen, insToPads) = (ins \ "port").partition(_.text == "open")
    val (outsOpen, outsToPads) = (outs \ "port").partition(_.text == "open")

    val insPads = insToPads.map(
      p => handlePackedInputIO(e, prim, p, g)
    )
    val outsPads = outsToPads.map(
      p => handlePackedOutputIO(e, prim, p, g)
    )

    e.copy(child = (nIo ++ insOpen ++ insPads ++ outsOpen ++ outsPads))
  }

  def traversePacked(e: xml.Elem, blocks: Map[String, TBlock], g: ElasticGraph): xml.Elem = {
    val untouchedLabels = Set("inputs", "outputs", "attributes", "parameters", "clocks")

    val nChild = e.child.map {
      case untouched: xml.Elem if (untouchedLabels contains untouched.label) => {
        untouched
      }

      case block: xml.Elem if (block.label == "block") => {
        val rawInstance = (block \@ "instance").replace("[0]", "")
        if (blocks.keySet contains rawInstance) {
          val prim = blocks(rawInstance)
          val nodeName = block \@ "name"

          if (prim.annotations contains AIo) {
            handlePackedIoPrim(block, prim, g)
          } else {
            block
          }
        } else {
          traversePacked(block, blocks, g)
        }
      }

      case other => other
    }

    e.copy(child = nChild)
  }

  def apply(
      e: xml.Elem,
      isArch: Boolean,
      isPack: Boolean,
      blocks: Map[String, TBlock],
      g: Option[ElasticGraph]
  ): xml.Elem = {
    val vprBlocks = blocks.map {
      (_, b) =>
        {
          (b.prim.vprNameString, b)
        }
    }

    if (isArch) {
      traverseArch(e, vprBlocks)
    } else {
      traversePacked(e, vprBlocks, g.get)
    }
  }
}
