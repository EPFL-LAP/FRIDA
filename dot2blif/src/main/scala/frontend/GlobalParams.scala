package frontend

import archs.Architectures
import archs.ArchDef

import sys.process._

sealed trait Mode(val str: String)

case object PackNoBuffer extends Mode("packnb")
case object Pack extends Mode("pack")
case object Sim extends Mode("sim")
case object Vis extends Mode("visualize")
case object Place extends Mode("place")
case object Route extends Mode("route")
case object All extends Mode("all")

object VPRParams {
  val defRlim = 1.0
  val defPlaceInnerNum = 50
  val defPlacePinCost = true
  val defRouterIterations = 500
  val defPlaceDelayModel = DeltaSidesMatrix
  val defPack = false

  def default = VPRParams(
    All,
    defPlaceDelayModel,
    defRouterIterations,
    defPlacePinCost,
    defPlaceInnerNum,
    defRlim,
    defPack
  )
}

case class VPRParams(
    mode: Mode,
    placeModel: PlaceDelayModel,
    routerIterations: Int,
    placePinCost: Boolean,
    placeInnerNum: Int,
    placeRlimEscape: Double,
    packerv2: Boolean
)

// TODO remove any dot reference from this file

object GlobalParams {
  lazy val root = sys.env("ELASTIC_ROOT")

  val defaultExp = "Default"
  val defaultSeed = 1

  val hwLib = root + "/build/hardware/lib/"
  val verilog = root + "/build/hardware/verilog/"
  val constraints = root + "/build/hardware/constraints/"

  val dynTimings = root + "/build/hardware/dyn_timings/"
  val dynRTLConfigVHDL = root + "/build/rtl-config.json"
  val dynRTLConfigVerilog = root + "/build/rtl-config-verilog.json"

  val rrgLib = root + "/build/rrgs/"

  val analysisBaseLoc = root + "/build/analysis/"

  val benchsPath = root + "/build/benchmarks/"
  val dotPath = benchsPath + "/dot/"
  val fpgaPath = benchsPath + "/fpga/"

  val dynPath = root + "/submodules/dynamatic/"

  val integrationTestPath = dynPath + "/integration-test/"

  val frontendScript = GlobalParams.root + "/scripts/dyn-frontend.sh"
  val bufferScript = GlobalParams.root + "/scripts/dyn-buffer.sh"
  val canonScript = GlobalParams.root + "/scripts/dyn-canonicalize.sh"
  val writeHDLScript = GlobalParams.root + "/scripts/dyn-write-hdl.sh"
  val visualizeScript = GlobalParams.root + "/scripts/dyn-visualize.sh"
  val simulateScript = GlobalParams.root + "/scripts/dyn-simulate.sh"
  val synScript = GlobalParams.root + "/scripts/dyn-synthesize.sh"

  def defaultBaseDir(archName: String): String = {
    root + "/build/" + archName + "/Default/"
  }

  def apply(
      archName: String,
      dotNames: List[String],
      bDir: String,
      bufCPObj: List[Double],
      withBuffers: Boolean = true,
      enableDisplay: Boolean = false,
      timings: Boolean = true,
      seeds: List[Int] = List(1),
      vprSettings: List[VPRParams] = List(VPRParams.default),
      forcePack: Boolean = false,
      forceRun: Boolean = false,
      parallel: Boolean = false,
      fpga: Boolean = false,
      fromDot: Boolean = false,
      mode: Mode = All
  ): GlobalParams = {
    val arch = if (fpga) {
      Architectures.archs.head._2 // Will be unused in any case...
    } else {
      Architectures(archName)
    }

    GlobalParams(
      arch,
      dotNames,
      bDir,
      withBuffers,
      enableDisplay,
      timings,
      seeds,
      bufCPObj,
      vprSettings,
      forcePack,
      forceRun,
      parallel,
      fpga,
      fromDot,
      mode
    )
  }
}

case class GlobalParams(
    scArch: ArchDef, // Architecture Description
    dotNames: List[String], // input circuit
    baseDir: String, // Build directory of current benchmark
    withBuffers: Boolean, // keep or remove buffers
    enableDisplay: Boolean, // enable VPR display
    timings: Boolean, // Run with or without timings
    seeds: List[Int], // Run experiments and average placer seeds
    bufCPObj: List[Double], // clock period objectives for the buffer placement
    vprSettings: List[VPRParams], // All VPR Settings
    forcePack: Boolean, // Do not use the cached packing
    forceRun: Boolean,
    parallel: Boolean,
    fpga: Boolean,
    fromDot: Boolean,
    mode: Mode
) {
  def getInstances: Seq[GlobalParamsInst] = {
    dotNames.map {
      dotName =>
        {
          vprSettings.map {
            vprSetting =>
              {
                seeds.map {
                  seed =>
                    {
                      val bDir = baseDir + "/" + dotName + "/base/"
                      val aDir = bDir + "/csv/"

                      val mlir = baseDir + "/" + dotName + "/mlir/"

                      // TODO do I really care about this?
                      // TODO find a way to name the VPR parameters properly
                      // Dot name and arch name should be included or excluded form there?
                      def name(): String = {
                        val seedStr = if (seeds.isEmpty) "" else "_s" + seed
                        // val b = if(withBuffers) "" else "_wb"
                        val t = if (timings) "" else "_wt"
                        t + seedStr
                      }

                      GlobalParamsInst(
                        scArch,
                        dotName,
                        bDir,
                        mlir,
                        aDir,
                        withBuffers,
                        enableDisplay,
                        timings,
                        seed,
                        bufCPObj,
                        vprSetting,
                        forcePack,
                        forceRun,
                        parallel,
                        fpga,
                        fromDot,
                        mode,
                        name
                      )
                    }
                }
              }
          }.flatten
        }
    }.flatten
  }
}

// TODO decouple this depending on the tool used.. Could have exactly one per tool...
// TODO Should be a trait with subclasses...
case class GlobalParamsInst(
  scArch: ArchDef, // Architecture Description
  bench: String, // input circuit
  buildDir: String, // Build directory of current benchmark
  mlirDir: String, // Where to place generated mlir
  analysisDir: String, // Output directory for all analysis passes
  withBuffers: Boolean, // keep or remove buffers
  enableDisplay: Boolean, // enable VPR display
  timings: Boolean, // Run with or without timings
  seed: Int, // Run experiments and average placer seeds
  cp: List[Double], // Target clock period for buffer placement
  vprSettings: VPRParams, // All VPR Settings
  forcePack: Boolean, // Do not use the cached packing
  forceRun: Boolean, // Force re-regeneration of all output files (VPR outputs, etc...)
  parallel: Boolean, // runs the flow in parallel
  fpga: Boolean, // Runs the FPGA flow, not the DHLS-RAs one
  fromDot: Boolean, // Reads the benchmark from a dot file and not an mlir one
  mode: Mode, // Enables / Disables phases in the compiler
  config: () => String // Function uniquely naming this GlobalParamsInst -> TODO should drop this?
) {
  val archPref = "arch"
  val circuitPref = "circuit"
  val packedPref = "packed"

  val archAll = archPref + ".xml"
  val circuitAll = circuitPref + ".eblif"
  val packedAll = packedPref + ".net"

  val archName = scArch.name

  val benchSrcDir = GlobalParams.dynPath + "/integration-test/" + bench + "/"
  val buildMlirDir = (buildDir + "/mlir/").replace("//", "/") // Dynamatic sometimes have troubles with //
  val buildHdlDir = (buildDir + "/hdl/").replace("//", "/")
  val buildSimDir = (buildDir + "/sim/").replace("//", "/")

  val benchUnbufferedLoc = mlirDir + "/" + bench + ".mlir"
  val preBuf = mlirDir + "/" + bench + "_prebuf.mlir"

  val preBufAnnos = buildMlirDir + "/" + bench + "_prebuf_annos.mlir"

  val packingCacheDir = buildDir + "/PackCache/"

  val vprDir = buildDir + "/vpr/"
  val dotPath = GlobalParams.dotPath + "/" + bench + ".dot"

  lazy val fpgaBuild = {
    assert(cp.size == 1)
    val cpH = cp.head

    GlobalParams.fpgaPath + "/" + bench + "/" + f"cp$cpH%2.2f" + "/"
  }

  def withoutTimings = {
    GlobalParamsInst(
      scArch,
      bench,
      buildDir,
      mlirDir,
      analysisDir,
      withBuffers,
      enableDisplay,
      false,
      seed,
      cp,
      vprSettings,
      forcePack,
      forceRun,
      parallel,
      fpga,
      fromDot,
      mode,
      config
    )
  }

  def withoutBuffers = {
    GlobalParamsInst(
      scArch,
      bench,
      buildDir,
      mlirDir,
      analysisDir,
      false,
      enableDisplay,
      timings,
      seed,
      cp,
      vprSettings,
      forcePack,
      forceRun,
      parallel,
      fpga,
      fromDot,
      mode,
      config
    )
  }
}
