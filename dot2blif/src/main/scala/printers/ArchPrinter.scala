package printers

import arch._
import util._
import frontend.GlobalParamsInst
import core.AImpl
import core.Namer

import java.io.FileWriter
import scala.io.Source

import io.AnsiColor._
import archs.ArchDef
import archs.Architectures
import archs.FixedLayout
import archs.AutoLayout
import analysis._
import archs.OEHB
import java.io.FileOutputStream
import java.io.File

object ArchPrinter {
  val OPT = "opt"

  def printModels(
    params: GlobalParamsInst,
    blocks: Map[String, TBlock],
    writer: FileWriter,
    indent: String,
    arch: Arch,
    tiles: Map[String, RootPb]
  ): String = {
    val builder = new StringBuilder()

    builder.append(indent + "<models>\n")

    val models = blocks
      .map(_._2)
      .map(
        b => BlockXMLPrinter(params, indent + "  ", b, arch)
      )
      .mkString("\n")
    builder.append(models)

    builder.append(indent + "</models>\n")
    builder.toString()
  }

  def printTiles(blocks: Map[String, TBlock], tiles: Map[String, RootPb], indent: String, arch: Arch): String = {
    val builder = new StringBuilder()

    builder.append(indent + "<tiles>\n")

    for ((_, v) <- tiles) {
      val s = TileXMLPrinter(indent + "  ", v, blocks, arch)
      builder.append(s)
    }

    builder.append(indent + "</tiles>\n")
    builder.toString()
  }

  def printLayout(blocks: Map[String, TBlock], tiles: Map[String, RootPb], indent: String, arch: Arch)(implicit
      params: GlobalParamsInst
  ): String = {
    val builder = new StringBuilder()
    builder.append(indent + "<layout>\n")
    var fixed = false

    params.scArch.layout match {
      case AutoLayout => {
        builder.append(indent + "  <auto_layout aspect_ratio=\"1.0\">\n")
        fixed = false
      }

      case FixedLayout(width, height) => {
        builder.append(
          indent + "  <fixed_layout name=\"fixed\" height=\"" + height
            + "\" width=\"" + width + "\">\n"
        )
        fixed = true
      }
    }

    tiles.foreach {
      (tName, t) =>
        {
          builder.append("    " + t.vprConfig.loc.layoutPrint(t.name))
        }
    }

    builder.append(indent + "    <corners type=\"EMPTY\" priority=\"100\"/>\n")

    if (fixed) {
      builder.append(indent + "  </fixed_layout>\n")
    } else {
      builder.append(indent + "  </auto_layout>\n")
    }

    builder.append(indent + "</layout>\n")
    builder.toString()
  }

  def printDevice(indent: String)(implicit params: GlobalParamsInst): String = {
    xml.PrettyPrinter(120, 4).format(Architectures.defaultDevice)
  }

  def printSwitchList(
      indent: String,
      tiles: Map[String, PbType],
      curArch: Arch,
      archs: List[Arch],
      blocks: Map[String, TBlock],
      gridInfo: Option[GridInfo]
  )(implicit params: GlobalParamsInst): String = {
    val switchs = Architectures.getSwitchList(gridInfo, curArch, params)

    xml.PrettyPrinter(120, 4).format(switchs)
  }

  def printSegmentList(
      indent: String,
      tiles: Map[String, PbType],
      curArch: Arch,
      archs: List[Arch],
      blocks: Map[String, TBlock]
  )(implicit params: GlobalParamsInst): String = {
    val switchLengths = params.scArch.switchLengths
    val segments = Architectures.getSegmentList(curArch, params)

    xml.PrettyPrinter(120, 4).format(segments)
  }

  def printDirectList(indent: String)(implicit params: GlobalParamsInst): String = {
    xml.PrettyPrinter(120, 4).format(Architectures.defaultDirectList)
  }

  def printComplexBlockList(
    params: GlobalParamsInst,
    blocks: Map[String, TBlock],
    tiles: Map[String, RootPb],
    writer: FileWriter,
    indent: String,
    arch: Arch
  ): String = {
    val builder = new StringBuilder()
    builder.append(indent + "<complexblocklist>\n")

    tiles.map {
      (tName, t) =>
        {
          val s = PrimitiveBlockXMLPrinter(params, t, arch, blocks)
          builder.append(s)
        }
    }

    builder.append(indent + "</complexblocklist>\n")
    builder.toString()
  }

  def addSinkRegOnDanglingPort(primitive: TBlock, arch: Arch, log: Boolean): TBlock = {
    val timings = primitive.physicalInfo.timings

    val nTimings = primitive.blockInterface.ports.map {
      (id, bp) =>
        {
          bp.toPins()
            .filter {
              pin =>
                {
                  timings
                    .filter {
                      case CombTiming(source, dest, _, _) => (source.toPin == pin) || (dest.toPin == pin)
                      case RegTiming(loc, _, _, _, _)     => (loc.toPin == pin)
                    }
                    .filter(!_.crossingUnderArch(arch))
                    .isEmpty
                }
            }
            .map {
              pin =>
                {
                  if (log) {
                    println(primitive.name + " is dangling on " + pin)
                  }

                  pin.id.pmw.pb match {
                    case D => {
                      val pi = PortInstance(pin.id, pin.loc, None)
                      RegTiming(pi, "", "", "", "")
                    }

                    case Vld => {
                      val id = BlockPortID(pin.id.width, pin.id.pt, PortMeaningWrapper(pin.id.pmw.pm, Hs), pin.id.dummy)
                      val pi = PortInstance(id, pin.loc, Some(HSValid))

                      RegTiming(pi, "", "", "", "")
                    }

                    case Rdy => {
                      val id = BlockPortID(pin.id.width, pin.id.pt, PortMeaningWrapper(pin.id.pmw.pm, Hs), pin.id.dummy)
                      val pi = PortInstance(id, pin.loc, Some(HSReady))

                      RegTiming(pi, "", "", "", "")
                    }

                    case others => scala.sys.error("Unexpected port bundle.")
                  }
                }
            }
        }
    }.flatten

    val nPhysInfo = PhysicalInfo(
      primitive.physicalInfo.area,
      timings ++ nTimings,
      primitive.physicalInfo.attrs
    )

    primitive.withTimings(nPhysInfo)
  }

  def collectBlocks(params: GlobalParamsInst, tiles: List[RootPb]): Map[String, TBlock] = {
    def rec(opened: List[PbType], acc: List[TBlock]): List[TBlock] = {
      if (opened.isEmpty) {
        acc
      } else {
        val pb = opened.head

        pb match {
          case primPb @ PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
            // println(prim)
            if(!params.timings) {
              assert(prim.blockInterface.ports.map(_._2).forall(_.id.dummy == Regular), prim)
            }

            rec(opened.tail, prim :: acc)
          }

          case other => rec(opened.tail ++ pb.subBlocks, acc)
        }
      }
    }

    tiles
      .map {
        rootPb =>
          {
            rec(rootPb :: Nil, Nil)
          }
      }
      .flatten
      .map(
        b => (b.name, b)
      )
      .toMap
  }

  def addSinkRegOnDanglingPorts(tile: RootPb, arch: Arch): RootPb = {
    def rec(pb: PbType): PbType = {
      pb match {
        case PrimPb(bi, links, modeLinks, c, annotations, name, prim, pinMap, dagIds) => {
          val log = new FileOutputStream(new File("/dev/null"))
          val nPrim = Console.withOut(log) {
            addSinkRegOnDanglingPort(prim, arch, false)
          }

          log.close()

          val nLinks = links.filter(!_.isInstanceOf[CLK])
          assert(modeLinks.isEmpty)

          PrimPb(bi, nLinks, modeLinks, c, annotations, name, nPrim, pinMap, dagIds)
        }

        case InterPb(bi, links, modeLinks, c, annotations, name, subBlocks) => {
          val nSubBlocks = subBlocks.map(rec(_))

          val nLinks = links.filter(!_.isInstanceOf[CLK])
          assert(modeLinks.isEmpty)

          InterPb(bi, nLinks, modeLinks, c, annotations, name, nSubBlocks)
        }

        case RootPb(bi, links, modeLinks, c, annotations, name, subBlocks, vprConfig) => {
          val nSubBlocks = subBlocks.map(rec(_))

          val nLinks = links.filter(!_.isInstanceOf[CLK])
          assert(modeLinks.isEmpty)

          RootPb(bi, nLinks, modeLinks, c, annotations, name, nSubBlocks, vprConfig)
        }
      }
    }

    if (tile.name.contains("ioTile")) {
      tile
    } else {
      PbTypeBuilder.addClockNets(rec(tile).asInstanceOf[RootPb])
    }
  }

  def apply(
    params: GlobalParamsInst,
    tiles: Map[String, RootPb],
    archs: List[Arch],
    gridInfo: Option[GridInfo]
  ): Unit = {
    implicit val p = params

    archs.foreach {
      (arch: Arch) =>
        {
          val archFile = params.buildDir + "/vpr/" + params.archPref + "_" + arch.name + ".xml"
          println(CYAN + "Printing : " + archFile + RESET)
          val writer: FileWriter = Util.writeOpen(archFile)

          val blocks = collectBlocks(params, tiles.map(_._2).toList)

          val archBlocks = blocks.map {
            (bName, b) =>
              {
                if (b.prim.isIo) {
                  (bName, b)
                } else {
                  (bName, addSinkRegOnDanglingPort(b, arch, true && params.timings))
                }
              }
          }

          val tilesWithSinkRegs = tiles.map(
            (tName, t) => (tName, addSinkRegOnDanglingPorts(t, arch))
          )

          writer.write("<architecture>\n")

          writer.write(printModels(params, archBlocks, writer, "  ", arch, tilesWithSinkRegs))
          writer.write("\n")
          writer.write(printTiles(archBlocks, tilesWithSinkRegs, "  ", arch))
          writer.write("\n")
          writer.write(printLayout(archBlocks, tilesWithSinkRegs, "  ", arch))
          writer.write("\n")
          writer.write(printDevice("  "))
          writer.write("\n")
          writer.write(printSwitchList("  ", tilesWithSinkRegs, arch, archs, blocks, gridInfo))
          writer.write("\n")
          writer.write(printSegmentList("  ", tilesWithSinkRegs, arch, archs, blocks))
          writer.write("\n")
          writer.write(printDirectList("  "))
          writer.write("\n")
          writer.write(printComplexBlockList(params, archBlocks, tilesWithSinkRegs, writer, "  ", arch))
          writer.write("\n")

          writer.write("</architecture>\n")
          writer.close()
        }
    }
  }
}
