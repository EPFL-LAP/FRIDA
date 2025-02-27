package printers

import arch.TBlock
import frontend.GlobalParamsInst
import frontend.GlobalParams

import java.io.File
import readers.TimingReader
import arch._
import util.Util

// VR: the combinational delay from any valid input to a ready output pin
// CV: the combinational delay from the condition input pin (for example in the mux) to any valid output pin
// CR: the combinational delay from the condition input pin to any ready output pin
// VC: the combinational delay from any valid input pin to the condition output pin
// VD: the combinational delay from any valid input to any data output

object DynamaticTimingPrinter {
  val compNameToDynCompName = Map[String, String](
    "handshake.cmpi" -> "Comparator",
    "handshake.addi" -> "Operator",
    "handshake.subi" -> "Operator",
    "handshake.muli" -> "Mult",
    "handshake.extsi" -> "",
    "handshake.addf" -> "", // No support for handshakemetic operators for floats yet
    "handshake.subf" -> "",
    "handshake.mulf" -> "",
    "handshake.divui" -> "",
    "handshake.divsi" -> "",
    "handshake.divf" -> "",
    "handshake.cmpf" -> "",
    "handshake.andi" -> "Operator",
    "handshake.ori" -> "Operator",
    "handshake.xori" -> "Operator",
    "handshake.shli" -> "Operator",
    "handshake.shrsi" -> "Operator",
    "handshake.shrui" -> "Operator",
    "handshake.select" -> "Select",
    "handshake.mc_load" -> "",
    "handshake.lsq_load" -> "",
    "handshake.mc_store" -> "",
    "handshake.lsq_store" -> "",
    "handshake.merge" -> "Merge",
    "handshake.control_merge" -> "CntrlMerge",
    "handshake.fork" -> "Fork",
    "handshake.d_return" -> "",
    "handshake.cond_br" -> "Branch",
    "handshake.end" -> "",
    "handshake.mux" -> "Mux"
  )

  val inport: String = {
    "\"inport\": {\n"
      + "  \"transparentBuffer\": 0,\n"
      + "  \"opaqueBuffer\": 0,\n"
      + "  \"delay\": {\n"
      + "    \"data\": {\n"
      + "      \"64\": 0\n"
      + "    },\n"
      + "    \"valid\": {\n"
      + "      \"1\": 0\n"
      + "    },\n"
      + "    \"ready\": {\n"
      + "      \"1\": 0\n"
      + "     },\n"
      + "     \"VR\": 0,\n"
      + "     \"CV\": 0,\n"
      + "     \"CR\": 0,\n"
      + "     \"VC\": 0,\n"
      + "     \"VD\": 0\n"
      + "  }\n"
      + "}"
  }

  val outport: String = {
    "\"outport\": {\n"
      + "  \"transparentBuffer\": 0,\n"
      + "  \"opaqueBuffer\": 0,\n"
      + "  \"delay\": {\n"
      + "    \"data\": {\n"
      + "      \"64\": 0\n"
      + "    },\n"
      + "    \"valid\": {\n"
      + "      \"1\": 0\n"
      + "    },\n"
      + "    \"ready\": {\n"
      + "      \"1\": 0\n"
      + "     },\n"
      + "     \"VR\": 0,\n"
      + "     \"CV\": 0,\n"
      + "     \"CR\": 0,\n"
      + "     \"VC\": 0,\n"
      + "     \"VD\": 0\n"
      + "  }\n"
      + "}"
  }

  def emptyComp(compName: String, latency: Int) = {
    "\"" + compName + "\": {\n"
      + "  \"latency\": {\n"
      + "    \"64\": " + latency + ".0\n"
      + "  },\n"
      + "  \"delay\": {\n"
      + "  \"data\": {\n"
      + "    \"64\": 0\n"
      + "  },\n"
      + "  \"valid\": {\n"
      + "    \"1\": 0\n"
      + "  },\n"
      + "  \"ready\": {\n"
      + "    \"1\": 0\n"
      + "  },\n"
      + "  \"VR\": 0.0,\n" // TODO should be 100.0 (open)
      + "  \"CV\": 0.0,\n" // TODO should be 100.0 (open)
      + "  \"CR\": 0.0,\n" // TODO should be 100.0 (open)
      + "  \"VC\": 0.0,\n" // TODO should be 100.0 (open)
      + "  \"VD\": 0.0\n" // TODO should be 100.0 (open)
      + "},\n"
      + "\"inport\": {\n"
      + "  \"transparentBuffer\": 0,\n"
      + "  \"opaqueBuffer\": 0,\n"
      + "  \"delay\": {\n"
      + "    \"data\": {\n"
      + "      \"64\": 0\n"
      + "    },\n"
      + "    \"valid\": {\n"
      + "      \"1\": 0\n"
      + "    },\n"
      + "    \"ready\": {\n"
      + "      \"1\": 0\n"
      + "    },\n"
      + "    \"VR\": 0,\n"
      + "    \"CV\": 0,\n"
      + "    \"CR\": 0,\n"
      + "    \"VC\": 0,\n"
      + "    \"VD\": 0\n"
      + "  }\n"
      + "},\n"
      + "\"outport\": {\n"
      + "  \"transparentBuffer\": 0,\n"
      + "  \"opaqueBuffer\": 0,\n"
      + "  \"delay\": {\n"
      + "    \"data\": {\n"
      + "      \"64\": 0\n"
      + "    },\n"
      + "    \"valid\": {\n"
      + "      \"1\": 0\n"
      + "      },\n"
      + "      \"ready\": {\n"
      + "        \"1\": 0\n"
      + "      },\n"
      + "      \"VR\": 0,\n"
      + "      \"CV\": 0,\n"
      + "      \"CR\": 0,\n"
      + "      \"VC\": 0,\n"
      + "      \"VD\": 0\n"
      + "    }\n"
      + "  }\n"
      + "}"
  }

  val isReg = "0.0" // TODO should be 100.0 (open)

  val zeroLatency: String = {
    "\"latency\": {\n"
      + "\"64\": 0.0\n"
      + "}"
  }

  def getMaxDelay(combTimings: List[CombTiming]): String = {
    combTimings.reduce {
      (c0, c1) =>
        {
          val c0Del = AddInterconnectDelay.sDelayToDouble(c0.maxDelay)
          val c1Del = AddInterconnectDelay.sDelayToDouble(c1.maxDelay)

          if (c0Del > c1Del) {
            c0
          } else {
            c1
          }
        }
    }.maxDelay
  }

  def printDelay(s: String): String = {
    s.replace("e-9", "")
  }

  def getDelay(pInfo: PhysicalInfo, hsType: Option[HSType]): String = {
    val regs = pInfo.timings
      .collect {
        case r: RegTiming => r
      }
      .map(_.loc)
      .toSet

    val combTimings = pInfo.timings
      .collect {
        case c: CombTiming => c
      }
      .filter {
        case CombTiming(source, dest, minDelay, maxDelay) => {
          (!regs.contains(source) && !regs.contains(dest))
          && (source.hsType == hsType) && (dest.hsType == hsType)
        }
      }

    if (combTimings.isEmpty) {
      isReg
    } else {
      printDelay(getMaxDelay(combTimings))
    }
  }

  def getDataDelay(pInfo: PhysicalInfo): String = {
    getDelay(pInfo, None)
  }

  def getValidDelay(pInfo: PhysicalInfo): String = {
    getDelay(pInfo, Some(HSValid))
  }

  def getReadyDelay(pInfo: PhysicalInfo): String = {
    getDelay(pInfo, Some(HSReady))
  }

  def getVR(pInfo: PhysicalInfo): String = {
    val regs = pInfo.timings
      .collect {
        case r: RegTiming => r
      }
      .map(_.loc)
      .toSet

    val combTimings = pInfo.timings
      .collect {
        case c: CombTiming => c
      }
      .filter {
        case CombTiming(source, dest, minDelay, maxDelay) => {
          (!regs.contains(source) && !regs.contains(dest))
          && (source.hsType == Some(HSValid)) && (dest.hsType == Some(HSReady))
        }
      }

    if (combTimings.isEmpty) {
      isReg
    } else {
      printDelay(getMaxDelay(combTimings))
    }
  }

  def getCV(pInfo: PhysicalInfo): String = {
    val regs = pInfo.timings
      .collect {
        case r: RegTiming => r
      }
      .map(_.loc)
      .toSet

    val combTimings = pInfo.timings
      .collect {
        case c: CombTiming => c
      }
      .filter {
        case CombTiming(source, dest, minDelay, maxDelay) => {
          (!regs.contains(source) && !regs.contains(dest))
          && (source.hsType == None) && (dest.hsType == Some(HSValid))
          && source.id.pmw.pm.isInstanceOf[PMCond]
        }
      }

    if (combTimings.isEmpty) {
      isReg
    } else {
      printDelay(getMaxDelay(combTimings))
    }
  }

  def getCR(pInfo: PhysicalInfo): String = {
    val regs = pInfo.timings
      .collect {
        case r: RegTiming => r
      }
      .map(_.loc)
      .toSet

    val combTimings = pInfo.timings
      .collect {
        case c: CombTiming => c
      }
      .filter {
        case CombTiming(source, dest, minDelay, maxDelay) => {
          (!regs.contains(source) && !regs.contains(dest))
          && (source.hsType == None) && (dest.hsType == Some(HSReady))
          && source.id.pmw.pm.isInstanceOf[PMCond]
        }
      }

    if (combTimings.isEmpty) {
      isReg
    } else {
      printDelay(getMaxDelay(combTimings))
    }
  }

  def getVC(pInfo: PhysicalInfo): String = {
    val regs = pInfo.timings
      .collect {
        case r: RegTiming => r
      }
      .map(_.loc)
      .toSet

    val combTimings = pInfo.timings
      .collect {
        case c: CombTiming => c
      }
      .filter {
        case CombTiming(source, dest, minDelay, maxDelay) => {
          (!regs.contains(source) && !regs.contains(dest))
          && (source.hsType == Some(HSValid)) && (dest.hsType == Some(None))
          && dest.id.pmw.pm.isInstanceOf[PMCond]
        }
      }

    if (combTimings.isEmpty) {
      isReg
    } else {
      printDelay(getMaxDelay(combTimings))
    }
  }

  def getVD(pInfo: PhysicalInfo): String = {
    val regs = pInfo.timings
      .collect {
        case r: RegTiming => r
      }
      .map(_.loc)
      .toSet

    val combTimings = pInfo.timings
      .collect {
        case c: CombTiming => c
      }
      .filter {
        case CombTiming(source, dest, minDelay, maxDelay) => {
          (!regs.contains(source) && !regs.contains(dest))
          && (source.hsType == Some(HSValid)) && (dest.hsType == Some(None))
          && dest.id.pmw.pm.isInstanceOf[PMData]
        }
      }

    if (combTimings.isEmpty) {
      isReg
    } else {
      printDelay(getMaxDelay(combTimings))
    }
  }

  def getWidth(cName: String): Int = {
    cName.split("W")(1).replace(GlobalParams.hwLib, "").replace(".cfg", "").split("_")(0).toInt
  }

  def getDataMap(width: Int, pInfo: PhysicalInfo): String = {
    val delay = getDataDelay(pInfo)

    if (width == 0) {
      "\"" + width + "\": 0.0"
    } else {
      "\"" + width + "\": " + delay
    }
  }

  def getCName(s: String): String = {
    if (s contains "CntrlMerge") {
      "CntrlMerge"
    } else {
      s.split("W")(0).replace(GlobalParams.hwLib, "")
    }
  }

  def apply(params: GlobalParamsInst): Unit = {
    val libDir = GlobalParams.hwLib

    val wantedPrims = compNameToDynCompName.map(_._2).toSet.filter(_.nonEmpty)
    val usedPrims = compNameToDynCompName.map(_._2).toSet

    val nameGroupped = new File(GlobalParams.hwLib).listFiles
      .map(_.toString())
      .filter {
        comp =>
          {
            wantedPrims contains getCName(comp)
          }
      }
      .map {
        hwModel =>
          {
            val pInfo = TimingReader(hwModel)
            assert(pInfo.timings.nonEmpty)

            (getCName(hwModel), (hwModel, pInfo))
          }
      }
      .groupBy(_._1)
      .map(
        (k, v) => (k, v.map(_._2))
      )
      .filter(usedPrims contains _._1)

    val jsons = compNameToDynCompName.map {
      (dynName, cName) =>
        {
          if (cName.isEmpty) {
            if (dynName == "handshake.lsq_load") { // Matches default dynamatic timing models, don't know why we need this
              emptyComp(dynName, 5)
            } else if (dynName == "handshake.mc_load") {
              emptyComp(dynName, 1)
            } else {
              emptyComp(dynName, 0)
            }
          } else {
            val pInfos = nameGroupped(cName)

            val maxWidth =
              if (cName contains "CntrlMerge") 0
              else
                pInfos
                  .map(
                    (name, pi) => getWidth(name)
                  )
                  .max
            val maxWidthPInfo = if (cName contains "CntrlMerge") {
              pInfos.head._2
            } else {
              pInfos
                .filter(
                  (name, pi) => getWidth(name) == maxWidth
                )
                .head
                ._2
            }

            val dataMaps = if (cName contains "Fork") {
              "\"64\": 0.0"
            } else {
              pInfos
                .map {
                  (name, pi) =>
                    {
                      if ((name contains "CntrlMerge")) {
                        "\"64\": 0.0"
                      } else {
                        getDataMap(getWidth(name), pi)
                      }
                    }
                }
                .mkString(",\n")
            }

            val valid = getValidDelay(maxWidthPInfo)
            val ready = getReadyDelay(maxWidthPInfo)
            val vr = getVR(maxWidthPInfo)
            val cv = getCV(maxWidthPInfo)
            val cr = getCR(maxWidthPInfo)
            val vc = getVC(maxWidthPInfo)
            val vd = getVD(maxWidthPInfo)

            val delayJson = "\"delay\": {\n"
              + "\"data\": {\n"
              + dataMaps + "\n"
              + "},\n"
              + "\"valid\": {\n"
              + "\"1\": " + valid + "\n"
              + "},\n"
              + "\"ready\": {\n"
              + "\"1\": " + ready + "\n"
              + "},\n"
              + "\"VR\": " + vr + ",\n"
              + "\"CV\": " + cv + ",\n"
              + "\"CR\": " + cr + ",\n"
              + "\"VC\": " + vc + ",\n"
              + "\"VD\": " + vd + "\n"
              + "}"

            val compJson = {
              "\"" + dynName + "\":{\n"
                + zeroLatency + ",\n"
                + delayJson + ",\n"
                + inport + ",\n"
                + outport + "\n"
                + "}"
            }

            compJson
          }
        }
    }

    val all = "{\n" + jsons.mkString(",\n") + "\n}\n"
    val f = Util.writeOpen(GlobalParams.root + "/build/hardware/dynamatic.json")
    f.write(all)
    f.close()
  }
}
