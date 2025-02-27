package frontend

import arch.Arch
import rrggen.RRGUnifier

import sys.process._

sealed trait VPRRunningMode
case object VPRPlace extends VPRRunningMode
case object VPRRoute extends VPRRunningMode
case object VPRRouteFull extends VPRRunningMode
case object VPRPlaceFull extends VPRRunningMode

sealed trait PlaceDelayModel(val str: String)
case object DeltaMatrix extends PlaceDelayModel("delta")
case object DeltaSidesMatrix extends PlaceDelayModel("delta_sides")

object VprUtils {
  lazy val defaultVprArgs: Seq[String] = {
    Seq(
      "--verify_file_digests",
      "off",
      "--sweep_dangling_blocks",
      "off",
      "--sweep_dangling_nets",
      "off",
      "--full_stats",
      "on",
      "--constant_net_method",
      "route",
      "--clustering_pin_feasibility_filter",
      "off"
    )
  }

  def runVpr(params: GlobalParamsInst, arch: Arch, mode: VPRRunningMode, unifiedChanWidth: Option[Int]): String = {
    val archName = params.archPref + "_" + arch.name + ".xml"
    val circuitName = params.circuitPref + "_" + arch.name + ".eblif"
    val packedName = params.packedPref + "_" + arch.name + ".net"
    val disp = if (params.enableDisplay) "on" else "off"
    val timings = if (params.timings) "on" else "off"

    val placeName = if (unifiedChanWidth.nonEmpty) {
      params.circuitPref + ".place"
    } else {
      params.circuitPref + "_" + arch.name + ".place"
    }

    val modeStr = mode match {
      case VPRPlace     => "--place"
      case VPRPlaceFull => "--place"
      case VPRRoute     => "--route"
      case VPRRouteFull => "--route"
    }

    val commonParams = Seq(
      "vpr",
      archName,
      params.circuitPref,
      modeStr,
      "--circuit_file",
      circuitName,
      "--net_file",
      packedName,
      "--disp",
      disp,
      "--timing_analysis",
      timings,
      "--timing_report_detail",
      "detailed",
      "--place_delay_model",
      params.vprSettings.placeModel.str,
      "--inner_num",
      params.vprSettings.placeInnerNum.toString(),
      "--place_rlim_escape",
      params.vprSettings.placeRlimEscape.toString()
    )

    val chanWidth = if (mode == VPRRouteFull) {
      unifiedChanWidth.get
    } else {
      arch.chanWidth
    }

    val routeParams = if ((mode == VPRRoute) || (mode == VPRRouteFull)) {
      Seq(
        "--place_file",
        placeName,
        "--route_chan_width",
        chanWidth,
        "--max_router_iterations",
        "500"
      )
    } else if (mode == VPRPlaceFull) {
      Seq(
        "--route_chan_width",
        chanWidth,
        "--seed",
        params.seed.toString()
      )
    } else {
      Seq("--seed", params.seed.toString())
    }

    val rrgParams = if ((mode == VPRRouteFull) || (mode == VPRPlaceFull)) {
      Seq("--read_rr_graph", RRGUnifier.RRG)
    } else {
      Seq("--write_rr_graph", "rrg_" + arch.name + ".xml")
    }

    val log = mode match {
      case VPRPlace     => "place_" + arch.name + ".log"
      case VPRRoute     => "route_" + arch.name + ".log"
      case VPRPlaceFull => "place_full.log"
      case VPRRouteFull => "route_full.log"
    }

    val allParams = (commonParams ++ routeParams ++ defaultVprArgs ++ rrgParams).mkString(" ")

    allParams + " &> /dev/null ; cp vpr_stdout.log " + log + " ; "
  }

  def runVprPlace(params: GlobalParamsInst, arch: Arch, placeMode: VPRRunningMode): String = {
    runVpr(params, arch, placeMode, None)
  }

  def place(params: GlobalParamsInst, placeArch: Arch, placeMode: VPRRunningMode): Unit = {
    val pinCostStr = if (params.vprSettings.placePinCost) {
      ""
    } else {
      "export DISABLE_PIN_COST=1; "
    }

    val placeScript = "bash -c \""
      + "cd " + GlobalParams.root + "; "
      + "source env.env ; "
      + "cd " + params.buildDir + "/vpr/ ; "
      + pinCostStr
      + runVprPlace(params, placeArch, placeMode)
      + "\"" // 2>errs_place.log

    println(placeScript)

    val logger = ProcessLogger(line => println(line), line => println(line))

    val f = util.Util.writeOpen(params.buildDir + "/place.sh")
    f.write(placeScript)
    f.close()

    placeScript.lazyLines(logger).foreach {
      println(_)
    }
  }

  def runVprRoute(params: GlobalParamsInst, arch: Arch, mode: VPRRunningMode, unifiedChanWidth: Option[Int]): String = {
    runVpr(params, arch, mode, unifiedChanWidth)
  }

  def routeUnified(params: GlobalParamsInst, archs: List[Arch]): Unit = {
    val chanWidth = archs.filter(!_.place).map(_.chanWidth).sum
    val placeArch = archs.filter(_.place).head

    val routeScript = "bash -c \""
      + "cd " + GlobalParams.root + "; "
      + "source env.env ; "
      + "cd " + params.buildDir + "/vpr/ ; "
      + runVprRoute(params, placeArch, VPRRouteFull, Some(chanWidth)) + "\"" // 2>errs_route.log

    println(routeScript)

    val logger = ProcessLogger(line => println(line), line => println(line))

    routeScript.lazyLines(logger).foreach {
      println(_)
    }
  }

  def route(params: GlobalParamsInst, archs: List[Arch]): Unit = {
    archs.foreach {
      arch =>
        {
          if (!arch.place) {
            val routeScript = "bash -c \""
              + "cd " + GlobalParams.root + "; "
              + "source env.env ; "
              + "cd " + params.buildDir + "/vpr/ ; "
              + runVprRoute(params, arch, VPRRoute, None)
              + "\"" // 2>errs_place.log

            val f = util.Util.writeOpen(params.buildDir + "/route_" + arch.name + ".sh")
            f.write(runVprRoute(params, arch, VPRRoute, None))
            f.close()

            println(routeScript)

            val logger = ProcessLogger(line => println(line), line => println(line))

            routeScript.lazyLines(logger).foreach {
              println(_)
            }

            if (params.timings) {
              val moveResults = "bash -c \""
                + "cd " + GlobalParams.root + "; "
                + "source env.env ;"
                + "cd " + params.buildDir + "/vpr/ ; "
                + "mv report_timing.setup.rpt report_timing_setup_" + arch.name + ".rpt" + "\"" // 2>errs_route.log

              moveResults.!!
            }
          }
        }
    }
  }
}
