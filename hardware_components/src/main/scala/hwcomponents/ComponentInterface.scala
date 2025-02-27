package components

import arch.{Pin => APin, _}
import archs.Params
import archs.Blocks$package.TPrim
import archs._
import circt.stage.ChiselStage
import chisel3.RawModule
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.FirtoolOption

// import timing.{TimingPin, Register, ResetPin, TimingEntity}
// import readers.ToolsetFlowType

trait WireComponent[P <: Params] {
  def defaultDelays(p: P): String
}

trait ChiselComponent[P <: Params] {
  val pmD = PortMeaningWrapper(PMData(None), Impl)
  val pmC = PortMeaningWrapper(PMCond(None), Impl)
  val pmConf = PortMeaningWrapper(PMConf(None), D)

  val clockName = "clock"

  def disableChiselOptimisations = false

  // protected def chiselArgs: Array[String] = Array[String]("--target", "verilog")
  protected def chiselArgs: Array[String] = Array[String]()

  // TODO double check why these are necessary...
  protected def firtoolOpts: Array[String] = Array[String](
    "--lowering-options=noAlwaysComb,disallowPackedArrays,disallowLocalVariables",
    "--disable-all-randomization"
  ) ++ { if (disableChiselOptimisations) Seq("--disable-opt") else Seq() }

  protected def emitVerilogNotSV(comp: () => RawModule) = {
    ChiselStage.emitSystemVerilog(
      comp(),
      args = chiselArgs,
      firtoolOpts = firtoolOpts
    )
  }

  def emitVerilog(p: P): (String, String)
  def clocked: Boolean
  def portNameToPortInstance(
      portName: String,
      hsType: Option[HSType],
      p: P,
      word: Int
  ): PortInstance
  def unpackParams(moduleName: String): P
  def wantedConfigurations: List[P]

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

  // Up to _reg is the name prefix
  // After _reg is either
  //   _bitIndex_
  //   _wordIndex__bitIndex_
  def translate(moduleName: String, verilogName: String): TimingEntity = {
    if (verilogName contains "/") { // Is not a top level Pin
      val prefix = verilogName.split("_reg")(0)

      val r = """_\d+__""".r
      val suffix = if (r.findFirstIn(verilogName.split("_reg")(1)).isDefined) {
        verilogName.split("_reg")(1).split("__")(0)
      } else {
        ""
      }

      Register(prefix + suffix)
    } else if (verilogName == "reset") {
      ResetPin
    } else {
      val splitName = verilogName.split("_").map { s =>
        {
          val r = """\[\d+\]""".r
          val withoutBit = r.findFirstIn(s) match {
            case Some(m) => s.replace(m, "")
            case None    => s
          }

          withoutBit
        }
      }

      val params = unpackParams(moduleName)
      val hsloc = if (splitName.size == 2) 1 else 2

      val ready = splitName.size > 1 && (splitName(hsloc) == "ready")
      val valid = splitName.size > 1 && (splitName(hsloc) == "valid")

      val word =
        if ((splitName.size > 1) && splitName(1).forall(c => c.isDigit))
          splitName(1).toInt
        else 0

      val hsType =
        if (ready) Some(HSReady) else if (valid) Some(HSValid) else None
      val pi = portNameToPortInstance(splitName(0), hsType, params, word)

      Pin(splitName(0), pi)
    }
  }
}

object ChiselComponent {
  def primToChiselComp: Map[String, ChiselComponent[_ <: Params]] = Map(
    archs.JoinStaticInfo.typeString -> components.Join,
    archs.BranchStaticInfo.typeString -> components.Branch,
    archs.ForkStaticInfo.typeString -> components.Fork,
    archs.MergeStaticInfo.typeString -> components.Merge,
    archs.MuxConfigStaticInfo.typeString -> components.MuxConfig,
    archs.MuxStaticInfo.typeString -> components.Mux,
    archs.SelectStaticInfo.typeString -> components.Select,
    archs.CntrlMergeStaticInfo.typeString -> components.CntrlMerge,
    // archs.Entry->components.
    // archs.Exit->components.
    archs.ConstantStaticInfo.typeString -> components.Cst,
    archs.SinkStaticInfo.typeString -> components.Sink,
    archs.SourceStaticInfo.typeString -> components.Source,
    archs.EBStaticInfo.typeString -> components.EB,
    archs.TEHBStaticInfo.typeString -> components.TEHB,
    archs.OEHBStaticInfo.typeString -> components.OEHB,
    archs.ComparatorStaticInfo.typeString -> components.Icmp,
    archs.MultStaticInfo.typeString -> components.Multiplier,
    archs.OperatorStaticInfo.typeString -> components.ALU,
    archs.ConfigurationBitsStaticInfo.typeString -> components.ConfigurationBits
    // archs.Undef->components.
    // archs.DummyType->components.
  )

  def emitVerilog[P <: Params](p: P): (String, String) = {
    // would like to make this more generic though
// def typeSpecificAction[P <: Params, C<:ChiselComponent[P], R](p: P, action: ChiselComponent[P].type => (P=>R)): R = {
    // def v[C<:ChiselComponent[P]](x: C): P=>(String, String)=x.emitVerilog
    // def f[C<:ChiselComponent[P]] = (a: C) => a.emitVerilog _

    p match {
      case ps: JoinParams       => components.Join.emitVerilog(ps)
      case ps: BranchParams     => components.Branch.emitVerilog(ps)
      case ps: ComparatorParams => components.Icmp.emitVerilog(ps)
      case ps: ForkParams       => components.Fork.emitVerilog(ps)
      case ps: MergeParams      => components.Merge.emitVerilog(ps)
      case ps: MuxConfigParams  => components.MuxConfig.emitVerilog(ps)
      case ps: MuxParams        => components.Mux.emitVerilog(ps)
      case ps: SelectParams     => components.Select.emitVerilog(ps)
      case ps: ConstantParams   => components.Cst.emitVerilog(ps)
      case ps: SinkParams       => components.Sink.emitVerilog(ps)
      case ps: CntrlMergeParams => components.CntrlMerge.emitVerilog(ps)
      case ps: SourceParams     => components.Source.emitVerilog(ps)
      case ps: EBParams         => components.EB.emitVerilog(ps)
      case ps: TEHBParams       => components.TEHB.emitVerilog(ps)
      case ps: OEHBParams       => components.OEHB.emitVerilog(ps)
      case ps: MultParams       => components.Multiplier.emitVerilog(ps)
      case ps: OperatorParams   => components.ALU.emitVerilog(ps)
    }
  }
}

