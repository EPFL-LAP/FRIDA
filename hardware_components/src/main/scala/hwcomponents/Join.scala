package components

import arch.{Mux => AMux, _}
import archs.{Join => Jn, JoinStaticInfo => Jni, JoinParams, Params}

import chisel3._
import chisel3.util.Decoupled
import circt.stage.ChiselStage

object Join extends ChiselComponent[JoinParams] {
  def wantedConfigurations: List[JoinParams] = {
    JoinParams(2) :: JoinParams(4) :: JoinParams(8) :: JoinParams(16) :: Nil
  }

  def apply(p: JoinParams, prefix: String = ""): Join = {
    new Join(p, prefix)
  }

  val clocked = Jni.clocked

  def unpackParams(moduleName: String): JoinParams = {
    assert(moduleName.startsWith(Jni.typeString))

    val params =
      moduleName.replace(Jni.typeString, "").replace("_optimized", "")
    Jni.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: JoinParams,
      word: Int
  ): PortInstance = {
    portName match {
      case "dIn" =>
        PortInstance(
          BlockPortID(0, flipped(PTInput, hsType), pmD, Regular),
          word,
          hsType
        )
      case "dOut" =>
        PortInstance(
          BlockPortID(0, flipped(PTOutput, hsType), pmD, Regular),
          0,
          hsType
        )
    }
  }

  def emitVerilog(p: JoinParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new Join(p))
    val moduleName = Jn(p).libName

    (v, moduleName)
  }
}

// TODO regroup ready valid into decoupled with 0 width data path
class Join(p: JoinParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + Jn(p).libName

  val dIn = IO(Flipped(Vec(p.num, Decoupled(UInt(0.W)))))
  val dOut = IO(Decoupled(UInt(0.W)))

  for (i <- 0 until p.num) {
    // sets each input's ready to and of all others' valids
    dIn(i).ready := (0 until p.num)
      .map { j =>
        {
          if (i == j) 1.B
          else dIn(j).valid
        }
      }
      .reduce(_ && _) && dOut.ready
  }

  dOut.valid := dIn.map(_.valid).reduce(_ & _)

  dOut.bits := DontCare
}

object JoinGenerator {
  def main(args: Array[String]) = {
    val v = ChiselStage.emitSystemVerilog(new Join(JoinParams(2)))
    println(v)
  }
}
