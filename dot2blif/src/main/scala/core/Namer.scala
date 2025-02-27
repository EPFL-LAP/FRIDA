package core

import arch._
import crkt._
import util.Util

import math.min

object Namer {
  def apply(id: BlockPortID): String = {
    val mStr = id.pmw.pm.matcher.fold("")(
      str => "_" + str
    )

    id.pmw.pb.str + id.pt.str + id.pmw.pm.str + id.width + id.dummy.str + mStr
  }

  def apply(pin: Pin): String = {
    Namer(pin.id) + "_" + pin.loc
  }

  def giveName(p: Port): String = {
    p.thisNode + "__" + Namer(p.id)
  }

  def apply(value: PortNodeID): String = {
    val pin = Pin(value.pId, value.loc)
    value.nodeName + "__" + Namer(pin)
  }

  def apply(p: Port): String = {
    Namer(p.nodeID())
  }

  def apply(pbLoc: PbLoc): String = {
    val pinName = Namer(pbLoc.pin)
    val pbName = Util.validVprName(pbLoc.pbName)

    pbName + "." + pinName
  }

  def apply(d: Direct): (String, String, String) = {
    d match {
      case Direct(srcLoc, dstLoc, delay) => {
        val srcStr = Namer(srcLoc)
        val dstStr = Namer(dstLoc)

        val dName = srcStr + "2" + dstStr

        (dName, srcStr, dstStr)
      }
    }
  }

  def apply(m: Mux): (String, String, String) = {
    m match {
      case Mux(sources, dstLoc, delay) => {
        val srcStr = sources
          .map(
            srcLoc => Namer(srcLoc)
          )
          .mkString("_")
        val dstStr = Namer(dstLoc)

        val mName = srcStr + "2" + dstStr

        (mName, srcStr, dstStr)
      }
    }
  }

  def apply(c: CLK): (String, String, String) = c match {
    case CLK(srcName, dstName) => {
      val inC = Util.validVprName(srcName) + ".clk"
      val outC = Util.validVprName(dstName) + ".clk"

      val cName = inC + "2" + outC

      (inC, outC, cName)
    }
  }
}
