package arch

import scala.collection.Iterator
import java.io.FileReader
import math.floor
import archs.VPRFC
import archs.Frac

// The Arch class acts as a mask on the underlying architecture,
// and selects only the elements with targetSize data width

// TODO Maybe would be better to do something like this:
// https://stackoverflow.com/questions/17684023/different-types-in-map-scala
case class Container[+T](e: T) {
  def get[T]: T = {
    e.asInstanceOf[T]
  }
}

object Arch {
  def place(chanWidth: Int) = Arch("", -1, Set(), true, chanWidth, VPRFC(Frac, 0.5), VPRFC(Frac, 0.5))
}

sealed case class Arch(
    name: String,
    width: Int,
    pbs: Set[PortBundle], // TODO probably just want to check the concrete width no?
    place: Boolean,
    chanWidth: Int,
    fcIn: VPRFC,
    fcOut: VPRFC
) {
  def contains(id: BlockPortID): Boolean = {
    if (place) {
      true
    } else {
      (id.concreteWidth == width) && (pbs.exists(_ == id.pmw.pb))
    }
  }
}
