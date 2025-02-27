package analysis

import frontend.GlobalParamsInst
import arch._
import archs.Params
import archs.Primitive
import util.Util
import core.Namer
import readers.PathDir
import readers.Backward
import readers.Mixed
import readers.Forward
import archs.Branch
import archs.MuxConfig
import archs.MuxConfigParams
import printers.BlockXMLPrinter

type PrimTimingInfo = Map[Primitive, PhysicalInfo]

object LibraryAnalysis {
  def getDelay(delay: String): Double = {
    delay.replace("e-9", "").toDouble
  }

  def compareMuxs(a: MuxConfig, b: MuxConfig): Boolean = {
    val aInstName = a.libName
    val bInstName = b.libName

    val aConfig = a.unpackLibRep(a.libName.replace("CMux", ""))
    val bConfig = a.unpackLibRep(b.libName.replace("CMux", ""))

    if (aConfig.width < bConfig.width) {
      true
    } else if (aConfig.width > bConfig.width) {
      false
    } else {
      if (aConfig.num > bConfig.num) {
        true
      } else if (aConfig.num < bConfig.num) {
        false
      } else {
        if (aConfig.tileSideLength > bConfig.tileSideLength) {
          true
        } else if (aConfig.tileSideLength < bConfig.tileSideLength) {
          false
        } else {
          if (aConfig.wireLength > bConfig.wireLength) {
            true
          } else {
            false
          }
        }
      }
    }
  }

  def getAllTimingGraphs(params: GlobalParamsInst): PrimTimingInfo = {
    // typeNameToType.map(_._2).map {
    //   prim => {
    //     prim.defaultConfigs.map{
    //       p => {
    //         val primInst = PrimInstance(prim, p)

    //         (primInst, prim.getTimings(p, params))
    //       }
    //     }
    //   }
    // }.flatten.toMap

    ???
  }

  def saveCriticalPatahs(csvDir: String, tgs: PrimTimingInfo): Unit = {
    val header = "Primitive Instance, Critical Path (ns)\n"
    val rows = tgs.toList
      .sortBy(-_._2.criticalPath)
      .map {
        (pi, tg) =>
          {
            (pi.libName + ", " + tg.criticalPath)
          }
      }
      .mkString("\n")

    val f = Util.writeOpen(csvDir + "/primitive_critical_paths.csv")
    f.write(header)
    f.write(rows + "\n")
    f.close()
  }

  def saveCriticalPatahsCBMuxs(csvDir: String, tgs: PrimTimingInfo, widths: List[Int]): Unit = {
    val header = "Primitive Instance, Critical Path (ns)\n"
    val rows = tgs.toList
      .filter(_._1 == MuxConfig)
      .filter { // TODO this should be a collect
        (pi, tg) =>
          {
            pi.libName.contains("T0_L0") && widths.exists(
              w => pi.libName.contains("W" + w)
            )
          }
      }
      .filter(_._2.area != 0)
      .sortWith {
        (a, b) =>
          {
            val aInst = a._1
            val bInst = b._1

            compareMuxs(aInst.asInstanceOf[MuxConfig], bInst.asInstanceOf[MuxConfig])
          }
      }
      .map {
        (pi, tg) =>
          {
            (pi.libName + ", " + tg.criticalPath)
          }
      }
      .mkString("\n")

    val fName = csvDir + "/CB_critical_paths_" + widths.mkString("_") + ".csv"
    println(fName)

    val f = Util.writeOpen(fName)
    f.write(header)
    f.write(rows + "\n")
    f.close()
  }

  def saveAreasCBMuxs(csvDir: String, tgs: PrimTimingInfo, widths: List[Int]): Unit = {
    val header = "Primitive Instance, Area (um2)\n"
    val rows = tgs.toList
      .filter(_._1 == MuxConfig)
      .filter {
        (pi, tg) =>
          {
            pi.libName.contains("T0_L0") && widths.exists(
              w => pi.libName.contains("W" + w)
            )
          }
      }
      .filter(_._2.area != 0)
      .sortWith {
        (a, b) =>
          {
            val aInst = a._1
            val bInst = b._1

            compareMuxs(aInst.asInstanceOf[MuxConfig], bInst.asInstanceOf[MuxConfig])
          }
      }
      .map(
        (pi, tg) => (pi.libName + ", " + tg.area)
      )
      .mkString("\n")

    val fName = csvDir + "/" + "CB_areas_" + widths.mkString("_") + ".csv"
    println(fName)

    val f = Util.writeOpen(fName)
    f.write(header)
    f.write(rows + "\n")
    f.close()
  }

  def saveCriticalPathsSBMuxs(
      csvDir: String,
      tgs: PrimTimingInfo,
      widths: List[Int],
      lengths: List[Int],
      tileSides: List[Int],
      ins: List[Int]
  ): Unit = {
    val header = "Primitive Instance, Critical Path (ns)\n"
    val rows = tgs.toList
      .filter(_._1 == MuxConfig)
      .filter {
        (pi, tg) =>
          {
            val config = pi.unpackLibRep(pi.libName.replace("CMux", "")).asInstanceOf[MuxConfigParams]

            widths.contains(config.width)
            && lengths.contains(config.wireLength)
            && tileSides.contains(config.tileSideLength)
            && ins.contains(config.num)
          }
      }
      .sortWith {
        (a, b) =>
          {
            val aInst = a._1
            val bInst = b._1

            compareMuxs(aInst.asInstanceOf[MuxConfig], bInst.asInstanceOf[MuxConfig])
          }
      }
      .map {
        (pi, tg) =>
          {
            (pi.libName + ", " + tg.criticalPath)
          }
      }
      .mkString("\n")

    val fName = csvDir + "/SB_critical_paths_W" + widths.mkString("_")
      + "_L" + lengths.mkString("_")
      + "_T" + tileSides.mkString("_")
      + "_N" + ins.mkString("_") + ".csv"

    println(fName)
    val f = Util.writeOpen(fName)

    f.write(header)
    f.write(rows + "\n")
    f.close()
  }

  def saveAreasSBMuxs(
      csvDir: String,
      tgs: PrimTimingInfo,
      widths: List[Int],
      lengths: List[Int],
      tileSides: List[Int],
      ins: List[Int]
  ): Unit = {
    val header = "Primitive Instance, Area (um2)\n"
    val rows = tgs.toList
      .filter(_._1 == MuxConfig)
      .filter {
        (pi, tg) =>
          {
            val config = pi.unpackLibRep(pi.libName.replace("CMux", "")).asInstanceOf[MuxConfigParams]

            widths.contains(config.width)
            && lengths.contains(config.wireLength)
            && tileSides.contains(config.tileSideLength)
            && ins.contains(config.num)
          }
      }
      .sortWith {
        (a, b) =>
          {
            val aInst = a._1
            val bInst = b._1

            compareMuxs(aInst.asInstanceOf[MuxConfig], bInst.asInstanceOf[MuxConfig])
          }
      }
      .map(
        (pi, tg) => (pi.libName + ", " + tg.area)
      )
      .mkString("\n")

    val fName = csvDir + "/SB_areas_W" + widths.mkString("_")
      + "_L" + lengths.mkString("_")
      + "_T" + tileSides.mkString("_")
      + "_N" + ins.mkString("_") + ".csv"

    println(fName)
    val f = Util.writeOpen(fName)
    f.write(header)
    f.write(rows + "\n")
    f.close()
  }

  def saveAreas(csvDir: String, tgs: PrimTimingInfo): Unit = {
    val header = "Primitive Instance, Area (um2)\n"
    val rows = tgs.toList
      .sortBy(-_._2.area)
      .map(
        (pi, tg) => (pi.libName + ", " + tg.area)
      )
      .mkString("\n")

    val f = Util.writeOpen(csvDir + "/primitive_areas.csv")
    f.write(header)
    f.write(rows + "\n")
    f.close()
  }

  def getPins[P <: Params](params: GlobalParamsInst, pi: Primitive): List[String] = {
    val block = pi.instantiate(params)
    block.blockInterface.ports
      .map(_._2)
      .map {
        bp =>
          {
            (0 until bp.words).map {
              word =>
                {
                  bp.pmw.pb match {
                    case Impl => {
                      ???
                    }

                    case D => {
                      val pin = Pin(bp.id, word)
                      Namer(pin) :: Nil
                    }

                    case Hs => {
                      ???
                    }

                    case Vld => {
                      val pin = Pin(bp.id, word)
                      Namer(pin) :: Nil
                    }

                    case Rdy => {
                      val pin = Pin(bp.id, word)
                      Namer(pin) :: Nil
                    }
                  }
                }
            }.flatten
          }
      }
      .flatten
      .toList
  }

  def getRegs(physInfo: PhysicalInfo): List[String] = {
    physInfo.timings
      .collect {
        case r: RegTiming => r
      }
      .map {
        case RegTiming(loc, t_setup, t_hold, t_clock_to_q_min, t_clock_to_q_max) => {
          val pin = loc.toPin
          Namer(pin) + "_reg"
        }
      }
  }

  def getEdges(physInfo: PhysicalInfo, regs: List[String]): List[(String, String)] = {
    val regSet = regs.toSet

    physInfo.timings.map {
      case CombTiming(source, dest, minDelay, maxDelay) => {
        val srcPin = source.toPin
        val dstPin = dest.toPin

        val srcReg = Namer(srcPin) + "_reg"
        val dstReg = Namer(dstPin) + "_reg"

        val srcName = if (regSet.contains(srcReg)) srcReg else Namer(srcPin)
        val dstName = if (regSet.contains(dstReg)) dstReg else Namer(dstPin)

        (srcName, dstName)
      }

      case RegTiming(loc, t_setup, t_hold, t_clock_to_q_min, t_clock_to_q_max) => {
        val locPin = loc.toPin

        if (loc.id.pt == PTInput) {
          val srcName = Namer(locPin)
          val srcRegName = Namer(locPin) + "_reg"

          (srcName, srcRegName)
        } else {
          val dstName = Namer(locPin)
          val dstRegName = Namer(locPin) + "_reg"

          (dstRegName, dstName)
        }
      }
    }
  }

  def printDotDecl(names: List[String], regs: Boolean): List[String] = {
    val color = if (regs) "darkgoldenrod1" else "deepskyblue2"

    names.map {
      name =>
        {
          "\"" + name + "\" [style=\"filled\", shape=\"box\", fillcolor=\"" + color + "\"];"
        }
    }
  }

  def getDirection(src: String, dst: String): PathDir = {
    if (src.contains("rdy") && dst.contains("rdy")) {
      Backward
    } else if ((src.contains("rdy") && !dst.contains("rdy")) || ((!src.contains("rdy") && dst.contains("rdy")))) {
      Mixed
    } else {
      Forward
    }
  }

  def printEdgeDecl(names: List[(String, String)]): List[String] = {
    names.map {
      (source, dest) =>
        {
          val dir = getDirection(source, dest)
          val color = dir match {
            case Forward  => "aquamarine"
            case Mixed    => "crimson"
            case Backward => "forestgreen"
          }

          "\"" + source + "\"" + " -> " + "\"" + dest + "\"" + "[" + "color=\"" + color + "\"];"
        }
    }
  }

  def saveTimingDots(params: GlobalParamsInst, csvDir: String, tgs: PrimTimingInfo): Unit = {
    tgs.foreach {
      (pi, tg) =>
        {
          val pins = printDotDecl(getPins(params, pi), false)
          val regs = printDotDecl(getRegs(tg), true)
          val edges = printEdgeDecl(getEdges(tg, regs))

          val piName = pi.libName

          val f = Util.writeOpen(csvDir + "/dots/" + piName + ".dot")
          f.write("Digraph G {" + "\n")
          f.write(pins.mkString("\n") + "\n")
          f.write(regs.mkString("\n") + "\n")
          f.write(edges.mkString("\n") + "\n")
          f.write("}\n")
          f.close()
        }
    }
  }

  def apply(params: GlobalParamsInst): Unit = {
    val csvDir = params.analysisDir + "/hwLib/"
    val timingGraphs = getAllTimingGraphs(params)

    saveCriticalPatahs(csvDir, timingGraphs)
    saveAreas(csvDir, timingGraphs)
    saveTimingDots(params, csvDir, timingGraphs)

    saveCriticalPatahsCBMuxs(csvDir, timingGraphs, 1 :: Nil)
    saveCriticalPatahsCBMuxs(csvDir, timingGraphs, 32 :: Nil)
    saveCriticalPatahsCBMuxs(csvDir, timingGraphs, 1 :: 32 :: Nil)

    saveAreasCBMuxs(csvDir, timingGraphs, 1 :: Nil)
    saveAreasCBMuxs(csvDir, timingGraphs, 32 :: Nil)
    saveAreasCBMuxs(csvDir, timingGraphs, 1 :: 32 :: Nil)

    saveCriticalPathsSBMuxs(csvDir, timingGraphs, 1 :: Nil, 1 :: Nil, 150 :: Nil, 8 :: Nil)
    saveCriticalPathsSBMuxs(csvDir, timingGraphs, 32 :: Nil, 1 :: Nil, 150 :: Nil, 8 :: Nil)
    saveCriticalPathsSBMuxs(csvDir, timingGraphs, 1 :: 32 :: Nil, 1 :: Nil, 150 :: Nil, 8 :: Nil)
    saveCriticalPathsSBMuxs(csvDir, timingGraphs, 1 :: Nil, 0 :: 1 :: 2 :: Nil, 0 :: 150 :: Nil, 8 :: Nil)
    saveCriticalPathsSBMuxs(csvDir, timingGraphs, 1 :: Nil, 1 :: Nil, List(100, 110, 120, 130, 140, 150, 160), 8 :: Nil)
    saveCriticalPathsSBMuxs(csvDir, timingGraphs, 1 :: Nil, 1 :: Nil, 150 :: Nil, List(2, 3, 4, 5, 6, 7, 8, 12, 16, 24))
    saveCriticalPathsSBMuxs(
      csvDir,
      timingGraphs,
      32 :: Nil,
      1 :: 2 :: Nil,
      100 :: 120 :: 140 :: 160 :: Nil,
      List(2, 3, 4, 5, 6, 7, 8)
    )

    saveAreasSBMuxs(csvDir, timingGraphs, 1 :: Nil, 1 :: Nil, 150 :: Nil, 8 :: Nil)
    saveAreasSBMuxs(csvDir, timingGraphs, 32 :: Nil, 1 :: Nil, 150 :: Nil, 8 :: Nil)
    saveAreasSBMuxs(csvDir, timingGraphs, 1 :: 32 :: Nil, 1 :: Nil, 150 :: Nil, 8 :: Nil)
    saveAreasSBMuxs(csvDir, timingGraphs, 1 :: Nil, 0 :: 1 :: 2 :: Nil, 0 :: 150 :: Nil, 8 :: Nil)
    saveAreasSBMuxs(csvDir, timingGraphs, 1 :: Nil, 1 :: Nil, List(100, 110, 120, 130, 140, 150, 160), 8 :: Nil)
    saveAreasSBMuxs(csvDir, timingGraphs, 1 :: Nil, 1 :: Nil, 150 :: Nil, List(2, 3, 4, 5, 6, 7, 8, 12, 16, 24))
    saveAreasSBMuxs(
      csvDir,
      timingGraphs,
      32 :: Nil,
      1 :: 2 :: Nil,
      100 :: 120 :: 140 :: 160 :: Nil,
      List(2, 3, 4, 5, 6, 7, 8)
    )

  }
}
