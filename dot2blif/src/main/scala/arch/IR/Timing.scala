package arch

import core.Namer
import util.Util

sealed trait DummyType(val str: String)
case object Dummy extends DummyType("Dum")
case object Regular extends DummyType("Reg")

object PhysicalInfo {
  def empty(): PhysicalInfo = PhysicalInfo(0, Nil, Map())
}

case class PhysicalInfo(area: Double, timings: List[Timing], attrs: Map[String, String]) {
  def getDelay(delay: String): Double = {
    delay.replace("e-9", "").toDouble
  }

  lazy val clocked = {
    timings.collect {
      case r: RegTiming => r
    }.nonEmpty
  }

  lazy val conbinational = !clocked

  // Assume setup is the input delay
  // clock to q is the output delay
  // comb timing is between any pair of pin / register
  lazy val criticalPath: Double = {
    val delays = timings.map {
      case c @ CombTiming(_, _, _, maxDelay) => getDelay(maxDelay)
      case r @ RegTiming(loc, t_setup, t_hold, t_clock_to_q_min, t_clock_to_q_max) => {
        val setup = if (t_setup.nonEmpty) getDelay(t_setup) else 0
        val clockToQ = if (t_clock_to_q_max.nonEmpty) getDelay(t_clock_to_q_max) else 0

        assert((setup == 0) || (clockToQ == 0))

        setup + clockToQ
      }
    }

    if (delays.isEmpty) {
      0.0
    } else {
      delays.max
    }
  }

  def extended: Boolean = {
    timings.filter {
      case CombTiming(source, dest, minDelay, maxDelay) => {
        (source.id.dummy == Dummy) || (dest.id.dummy == Dummy)
      }

      case RegTiming(loc, t_setup, t_hold, t_clock_to_q_min, t_clock_to_q_max) => {
        loc.id.dummy == Dummy
      }
    }.nonEmpty
  }

  def containsDummy(pi: PortInstance): Boolean = {
    timings.exists {
      case CombTiming(source, dest, minDelay, maxDelay) => {
        (source == pi) || (dest == pi)
      }

      case RegTiming(loc, t_setup, t_hold, t_clock_to_q_min, t_clock_to_q_max) => {
        loc == pi
      }
    }
  }
}

object Timing {
  def clocked(timings: List[Timing]): Boolean = {
    timings.collect {
      case reg: RegTiming => reg
    }.nonEmpty
  }
}

// TODO everything here should use Pin instead of PortInstance

sealed trait Timing {
  def validUnderArch(arch: Arch): Boolean
  def crossingUnderArch(arch: Arch): Boolean
  def hasDummy: Boolean
  def flipped: Timing
}

case class CombTiming(source: PortInstance, dest: PortInstance, minDelay: String, maxDelay: String) extends Timing {
  def validUnderArch(arch: Arch): Boolean = {
    arch.contains(source.id) && arch.contains(dest.id)
  }

  def crossingUnderArch(arch: Arch): Boolean = {
    (arch.contains(source.toPin.id) && !arch.contains(dest.toPin.id))
    || (!arch.contains(source.toPin.id) && arch.contains(dest.toPin.id))
  }

  def hasDummy: Boolean = source.isDummy || dest.isDummy
  def flipped = CombTiming(dest, source, minDelay, maxDelay)
}

case class RegTiming(
    loc: PortInstance,
    t_setup: String,
    t_hold: String,
    t_clock_to_q_min: String,
    t_clock_to_q_max: String
) extends Timing {
  def validUnderArch(arch: Arch): Boolean = {
    arch.contains(loc.id)
  }

  def crossingUnderArch(arch: Arch): Boolean = {
    false
  }

  def hasDummy: Boolean = loc.isDummy

  def flipped = RegTiming(loc, t_setup, t_hold, t_clock_to_q_min, t_clock_to_q_max)
}
