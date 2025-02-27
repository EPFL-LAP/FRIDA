package components

import arch.{Mux => AMux, Pin => APin, _}

sealed trait TimingEntity
sealed trait AbsPin extends TimingEntity
// TODO Rename this to avoid conflicts in arch
case class Pin(name: String, port: PortInstance) extends AbsPin
case class Register(name: String) extends TimingEntity
case object ResetPin extends AbsPin

object Delay {
  lazy val zero = Delay(0, 0, 0, 0, None, None)
}

// Delay setup applies to the destination
// Delay clockToQ applies to the source
// Everything cached to keep same rounding errors
case class Delay(
    clock: Double,
    requiredTime: Double,
    slack: Double,
    arrivalTime: Double,
    setup: Option[Double],
    clockToQ: Option[Double]
)

// Attrs is to allow adding some Dot attributes depending on some form of analysis
case class TimingEdge(
    source: TimingEntity,
    dest: TimingEntity,
    delay: Delay,
    attrs: Map[String, String]
)

// edges = source -> targets
case class TimingGraphBuilder(
    nodes: Set[TimingEntity],
    edges: Map[TimingEntity, List[TimingEdge]]
)
case class TimingGraph(
    nodes: Set[TimingEntity],
    edges: Set[TimingEdge],
    attrs: Map[String, String]
)
