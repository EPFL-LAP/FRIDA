package components

import java.io.File
import java.io.FileWriter
import TimingParser.round3

// All timings here are in ns

import arch.{Mux => AMux, Pin => APin, _}
import core.Namer

object TimingPrinter {
  val ns = "e-9"

  def regToPin(
      g: TimingGraph
  ): Map[Register, Set[Either[Pin, ResetPin.type]]] = {
    g.edges
      .filter(e =>
        ((e.source.isInstanceOf[Pin] || (e.source == ResetPin)) && e.dest
          .isInstanceOf[Register])
          || (e.source.isInstanceOf[Register] && (e.dest
            .isInstanceOf[Pin] || (e.dest == ResetPin)))
      )
      .groupBy(e => (if (e.source.isInstanceOf[Register]) e.source else e.dest))
      .map {
        case (r, es) => {
          (
            r.asInstanceOf[Register],
            es.map { e =>
              {
                if (e.source.isInstanceOf[Pin]) {
                  Left(e.source.asInstanceOf[Pin])
                } else if (e.dest.isInstanceOf[Pin]) {
                  Left(e.dest.asInstanceOf[Pin])
                } else if (e.source == ResetPin) {
                  Right(ResetPin)
                } else if (e.dest == ResetPin) {
                  Right(ResetPin)
                } else {
                  scala.sys.error("Unexpected type")
                }
              }
            }
          )
        }
      }
  }

  // TODO is copy pasted..............
  protected def flipPT(pt: PortType): PortType = pt match {
    case PTInput  => PTOutput
    case PTOutput => PTInput
    case PTUndef  => PTUndef
  }

  protected def flipped(pt: PortType, hsType: Option[HSType]): PortType = {
    hsType.fold(pt) {
      case HSValid => pt
      case HSReady => flipPT(pt)
    }
  }

  // TODO this should go once flipped is gone, need to fix legalization
  def rightDir(p: PortInstance): PortInstance = {
    val rPt = flipped(p.id.pt, p.hsType)

    val nId = BlockPortID(p.width, rPt, p.pmw, p.id.dummy)
    PortInstance(nId, p.word, p.hsType)
  }

  def printEdge(
      e: TimingEdge,
      r2pin: Map[Register, Set[Either[Pin, ResetPin.type]]]
  ): String = e match {
    case e @ TimingEdge(Pin(n0, p0), Pin(n1, p1), delay, attrs) => {
      val attrsStr = attrs
        .filter { case (k, v) => k != "color" }
        .map { case (k, v) => k + "=\"" + v + "\"" }
        .mkString(", ")
      val link = if (attrsStr.isEmpty) "" else ", "

      val p0RightDir = rightDir(p0)
      val p1RightDir = rightDir(p1)

      p0RightDir.physicalString() + " -> " + p1RightDir
        .physicalString() + " : { minDelay=\"" +
        round3(delay.arrivalTime).toString() + ns + "\", " +
        "maxDelay=\"" + round3(delay.arrivalTime)
          .toString() + ns + "\"" + link + attrsStr + " }"
    }

    case e @ TimingEdge(Pin(n0, p), Register(n1), delay, attrs) => {
      assert(delay.setup.isDefined) // registers must define a setup value
      val attrsStr = attrs
        .filter { case (k, v) => k != "color" }
        .map { case (k, v) => k + "=\"" + v + "\"" }
        .mkString(", ")

      val pRightDir = rightDir(p)

      val setup = delay.arrivalTime + delay.setup.get
      pRightDir.physicalString() + " : { " + "T_setup=\"" + round3(
        setup
      ) + ns + "\" " + attrsStr + " }"
    }

    case e @ TimingEdge(Register(n0), Pin(n1, p), delay, attrs) => {
      val attrsStr = attrs
        .filter { case (k, v) => k != "color" }
        .map { case (k, v) => k + "=\"" + v + "\"" }
        .mkString(", ")

      val pRightDir = rightDir(p)

      pRightDir.physicalString() + " : { " + "T_clock_to_Q_min=\"" + round3(
        delay.arrivalTime
      ) + ns + "\"" +
        ", T_clock_to_Q_max=\"" + round3(
          delay.arrivalTime
        ) + ns + "\" " + attrsStr + " }"
    }

    case e @ TimingEdge(r0 @ Register(n0), r1 @ Register(n1), delay, attrs) => {
      val fromPins = r2pin(r0)
      val destPins = r2pin(r1)

      val pairList: Set[(Pin, Pin)] = fromPins
        .map { (p0: Either[Pin, ResetPin.type]) =>
          p0 match {
            case Left(pin0) => {
              destPins.map { (p1: Either[Pin, ResetPin.type]) =>
                p1 match {
                  case Left(pin1) => {
                    Some(pin0, pin1)
                  }

                  case Right(_) => None
                }
              }
            }

            case Right(_) => None :: Nil
          }
        }
        .flatten
        .filter(_.isDefined)
        .map(_.get)
        .toSet

      pairList
        .map {
          case (p0: Pin, p1: Pin) => {
            val d = round3(delay.arrivalTime + delay.setup.getOrElse(0.0))

            val attrsStr = attrs
              .filter { case (k, v) => k != "color" }
              .map { case (k, v) => k + "=\"" + v + "\"" }
              .mkString(", ")

            val p0RightDir = rightDir(p0.port)
            val p1RightDir = rightDir(p1.port)
            p0RightDir.physicalString() + " -> " + p1RightDir.physicalString() +
              " : { minDelay=\"" + d + ns + "\", " + "maxDelay=\"" + d + ns + "\" " + attrsStr + " }"
          }
        }
        .mkString(",\n")
    }

    case _ => println("skipping reset pin as not represented in VPR"); ""
  }

  def printGraph(g: TimingGraph): String = {
    val r2pin = regToPin(g)
    g.edges.map(printEdge(_, r2pin)).filter(_.nonEmpty).mkString(",\n")
  }

  def apply(
      compName: String,
      g: TimingGraph,
      area: Area,
      fname: String
  ): Unit = {
    val file = new File(fname)
    file.getParentFile().mkdirs()
    val writer = new FileWriter(file)

    val attrs =
      g.attrs.map { case (k, v) => k + "=\"" + v + "\"" }.mkString(", ")

    writer.write(compName + "(" + area.total + ") { " + attrs + " } : [\n")
    writer.write(printGraph(g))
    writer.write("\n]\n")
    writer.close()
  }
}
