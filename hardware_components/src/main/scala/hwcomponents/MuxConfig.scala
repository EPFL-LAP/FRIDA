package components

import frontend.GlobalParams
import arch.{Mux => AMux, _}
import archs.{
  MuxConfig => MxC,
  MuxConfigStaticInfo => MxCi,
  MuxConfigParams,
  Params
}

import chisel3._
import chisel3.util.DecoupledIO
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import chisel3.util.MuxLookup
import chisel3.util.RegEnable
import chisel3.util.HasBlackBoxPath
import circt.stage.ChiselStage
import chisel3.util.HasBlackBoxInline

object MuxConfig extends ChiselComponent[MuxConfigParams] {
  def wantedConfigurations: List[MuxConfigParams] = {
    // MuxConfigParams(1, 42, 120, 2) :: MuxConfigParams(1, 42, 160, 2) :: MuxConfigParams(1, 36, 130, 2) ::
    //   MuxConfigParams(1, 36, 120, 2) :: MuxConfigParams(1, 36, 150, 2) :: MuxConfigParams(1, 36, 160, 2) ::
    //   MuxConfigParams(1, 30, 120, 1) :: MuxConfigParams(1, 30, 150, 2) :: MuxConfigParams(1, 30, 150, 1) ::
    //   MuxConfigParams(1, 42, 120, 2) :: MuxConfigParams(1, 42, 150, 2) :: MuxConfigParams(1, 30, 160, 1) ::
    //   MuxConfigParams(1, 32, 130, 2) :: MuxConfigParams(1, 32, 150, 2) :: MuxConfigParams(1, 32, 160, 2) ::
    //   MuxConfigParams(1, 39, 120, 2) :: MuxConfigParams(1, 39, 160, 2) :: MuxConfigParams(1, 34, 130, 2) ::
    //   MuxConfigParams(1, 34, 120, 2) :: MuxConfigParams(1, 34, 150, 2) :: MuxConfigParams(1, 34, 160, 2) ::
    //   MuxConfigParams(1, 34, 170, 2) :: MuxConfigParams(1, 31, 130, 1) :: MuxConfigParams(1, 31, 160, 1) ::
    //   MuxConfigParams(1, 42, 130, 2) :: MuxConfigParams(1, 42, 160, 2) :: MuxConfigParams(1, 34, 130, 2) ::
    //   MuxConfigParams(1, 33, 120, 2) :: MuxConfigParams(1, 31, 160, 1) :: MuxConfigParams(1, 31, 170, 1) ::
    //   MuxConfigParams(1, 32, 170, 2) :: MuxConfigParams(1, 42, 170, 2) ::
    //   MuxConfigParams(1, 40, 120, 2) :: MuxConfigParams(1, 40, 170, 2) ::
    //   MuxConfigParams(1, 31, 130, 1) :: MuxConfigParams(1, 31, 160, 1) ::
    //   MuxConfigParams(1, 31, 140, 1) :: MuxConfigParams(1, 31, 170, 1) ::
    //   MuxConfigParams(1, 39, 130, 2) :: MuxConfigParams(1, 39, 170, 2) ::
    //   MuxConfigParams(1, 42, 140, 2) :: MuxConfigParams(1, 42, 170, 2) ::
    //   MuxConfigParams(1, 42, 130, 2) :: MuxConfigParams(1, 42, 160, 2) ::
    //   MuxConfigParams(1, 22, 180, 1) :: MuxConfigParams(1, 29, 180, 2) ::
    //   MuxConfigParams(1, 5, 180, 1) :: MuxConfigParams(1, 6, 180, 2) ::
    //   MuxConfigParams(1, 30, 170, 1) :: MuxConfigParams(1, 33, 170, 2) ::
    //   MuxConfigParams(1, 31, 180, 1) :: MuxConfigParams(1, 42, 180, 2) ::
    //   MuxConfigParams(32, 5, 180, 1) :: MuxConfigParams(32, 6, 180, 2) ::
    //   MuxConfigParams(1, 33, 130, 2) :: MuxConfigParams(1, 33, 170, 2) ::
    //   MuxConfigParams(1, 31, 120, 2) ::
    //   MuxConfigParams(1, 31, 120, 1) :: MuxConfigParams(1, 31, 170, 2) :: Nil
    // (1 :: 32 :: Nil).map {
    //   width => {
    //     (2 until 30).map {
    //       num => {
    //         val noLoad = MuxConfigParams(width, num, 0, 0)

    //         noLoad :: (1 :: 2 :: Nil).map {
    //           length => {
    //             (100 until 180 by 10).map {
    //               tile => {
    //                 MuxConfigParams(width, num, tile, length)
    //               }
    //             }
    //           }
    //         }.flatten
    //       }
    //     }.flatten
    //   }
    // }.flatten

    MxCi.defaultConfigs
  }

  def apply(p: MuxConfigParams, prefix: String = ""): MuxConfig = {
    new MuxConfig(p, prefix)
  }

  val clocked = MxCi.clocked

  def unpackParams(moduleName: String): MuxConfigParams = {
    assert(moduleName.startsWith(MxCi.typeString))

    val params =
      moduleName.replace(MxCi.typeString, "").replace("_optimized", "")
    MxCi.unpackLibRep(params)
  }

  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: MuxConfigParams,
      word: Int
  ): PortInstance = {
    val condSize = log2Ceil(p.num)

    portName match {
      case "dIn" =>
        PortInstance(BlockPortID(p.width, PTInput, pmD, Regular), word, hsType)
      case "dOut" =>
        PortInstance(BlockPortID(p.width, PTOutput, pmD, Regular), 0, hsType)
      case "conf" =>
        PortInstance(BlockPortID(condSize, PTInput, pmConf, Regular), 0, None)
    }
  }

  def emitVerilog(p: MuxConfigParams): (String, String) = {
    val v = this.emitVerilogNotSV(() => new MuxConfig(p))
    val moduleName = MxC(p).libName

    (v, moduleName)
  }
}

case class VerilogMux(width: Int, prefix: String = "")
    extends BlackBox
    with HasBlackBoxInline {
  private val baseName = s"Mux${width}to1"
  override val desiredName = (if (prefix != "") prefix else "") + baseName
  val condSize = log2Ceil(width)

  val io = IO(
    new Bundle {
      val d = Input(UInt(width.W))
      val s = Input(UInt(condSize.W))
      val out = Output(Bool())
    }
  )

  private val cond = if (condSize == 1) {
    ""
  } else {
    "[" + (condSize - 1) + ":0]"
  }

  setInline(
    s"$desiredName.v",
    s"module $desiredName (\n" +
      s"  input [${width - 1}:0] d,\n" +
      s"  input $cond s,\n" +
      s"  output  out\n" +
      s");\n" +
      s"assign out = d[s];\n" +
      s"endmodule"
  )
}

class MuxConfig(p: MuxConfigParams, prefix: String = "") extends Module {
  override val desiredName = {
    if (p.recursivelyExtendPrefix) prefix else ""
  } + MxC(p).libName
  val condSize = log2Ceil(p.num)

  val dIn = IO(Input(Vec(p.num, UInt(p.width.W))))
  val dOut = IO(Output(UInt(p.width.W)))
  val conf = IO(Input(UInt(condSize.W)))

  val outs = (0 until p.width).map { i =>
    {
      val muxi = Module(
        VerilogMux(
          p.num,
          if (p.recursivelyExtendPrefix) MxC(p).libName + "_" + prefix else ""
        )
      )
      val ins = (0 until p.num).map { inI =>
        {
          dIn(inI)(i)
        }
      }

      muxi.io.d := VecInit(ins).asUInt
      muxi.io.s := conf
      muxi.io.out
    }
  }

  dOut := VecInit(outs).asUInt
}

object MuxConfigGenerator {
  def main(args: Array[String]) = {
    val v = emitVerilog(new MuxConfig(MuxConfigParams(2, 4, 0, 0)))
    println(v)
  }
}
