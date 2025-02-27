package frontend

import readers.MLIRParser
import crkt.MlirToCrktConverter
import crkt.RewriteIotoIo
import crkt.RewriteDangling
import crkt.RewriteUnsupportedToIO
import crkt.RewriteOutputToEnd
import core.PatternRewriterPass
import printers.DotPrinter
import crkt.CrktToMlirConverter
import crkt.Node
import printers.MLIRPrinter
import readers.FPGAReportReader

import sys.process._
import io.AnsiColor._
import archs.BlackBox
import archs.BlackBoxParams
import analysis.FPGAReports
import java.io.FileOutputStream
import java.io.File

object FPGABenchmark extends Tool {
  val name = "fpgabench"

  val discarded = Benchmark.discarded
  val benchList = Benchmark.benchList

  val targetCP = 4.0 // TODO pick the lowest one that doest not increase the II
  // val vivadoTargetCP = 4.0 // make sure to get the best performance given the circuit

  def rec(params: GlobalParamsInst): Unit = {
    assert(params.cp.size == 1)
    println("Synthesizing " + CYAN + params.bench + RESET + " on fpga.")

    val cp = params.cp.head

    val log = new FileOutputStream(new File("/dev/null"))
    Console.withOut(log) {
      Console.withErr(log) {
        ("mkdir -p " + params.fpgaBuild).!!

        val outDir = params.fpgaBuild
        val frontendOut = outDir + "frontend.mlir"
        val bufferOut = outDir + "buffer.mlir"
        val outmlir = outDir + "out.mlir"
        val hwmlir = outDir + "hw.mlir"

        val frontendScript = Seq(
          GlobalParams.frontendScript,
          params.benchSrcDir,
          outDir,
          params.bench,
          frontendOut,
          "fpga"
        )

        val bufferScript = Seq(
          GlobalParams.bufferScript,
          params.benchSrcDir,
          outDir,
          params.bench,
          frontendOut,
          bufferOut,
          cp.toString(),
          "fpga"
        )

        val canonScript = Seq(
          GlobalParams.canonScript,
          params.benchSrcDir,
          outDir,
          params.bench,
          bufferOut,
          outmlir
        )

        frontendScript.!!

        if (bufferScript.! == 0) {
          canonScript.!!

          println(CYAN + "Removing memories..." + RESET)

          val mlir = MLIRParser(outmlir)
          val g = MlirToCrktConverter(params, mlir)

          DotPrinter(outDir + "/parsed.dot")(g, Set())

          val nG = g.map {
            (nName, n) =>
              {
                n.nType match {
                  case BlackBox(p) => {
                    if (p.name.contains("end")) {
                      (
                        nName,
                        Node(n.name, BlackBox(BlackBoxParams(p.ins, p.outs, p.name, true)), n.mlirAttr, n.attr, n.annos, n.ports)
                      )
                    } else {
                      (nName, n)
                    }
                  }

                  case other => (nName, n)
                }
              }
          }

          val rewriters = List(
            RewriteUnsupportedToIO(0, 0),
            RewriteIotoIo,
            RewriteOutputToEnd,
            RewriteDangling
          )

          println(CYAN + "Printing back mlir..." + RESET)

          val withoutMemories = PatternRewriterPass(rewriters, nG)
          DotPrinter(outDir + "/nomem.dot")(withoutMemories, Set())

          val nMlir = CrktToMlirConverter(params, withoutMemories)
          MLIRPrinter(outmlir, nMlir)

          val writeHDLScript = Seq(
            GlobalParams.writeHDLScript,
            outDir,
            params.bench,
            outmlir,
            hwmlir,
            "vhdl"
          )

          val synScript = Seq(
            GlobalParams.synScript,
            outDir,
            params.bench,
            hwmlir,
            cp.toString(),
            (cp / 2).toString()
            // 2.toString(),
            // 1.toString()
          )

          def synRec(): Unit = { // Vivado sometimes crashes for no real reason...
            if (synScript.! != 0) {
              synRec()
            }
          }

          writeHDLScript.!!
          // synScript.!!
          synRec()

          FPGAReports(params, FPGAReportReader(params))
        }
      }
    }

    log.close()
  }

  def run(params: GlobalParamsInst): Unit = {
    params.cp.map {
      cp =>
        {
          val nParams = GlobalParamsInst(
            params.scArch,
            params.bench,
            params.buildDir,
            params.mlirDir,
            params.analysisDir,
            params.withBuffers,
            params.enableDisplay,
            params.timings,
            params.seed,
            cp :: Nil,
            params.vprSettings,
            params.forcePack,
            params.forceRun,
            params.parallel,
            params.fpga,
            params.fromDot,
            params.mode,
            params.config
          )

          rec(nParams)
        }
    }
  }

  def parseArguments(args: Seq[String]): Seq[GlobalParams] = {
    val dotName = getArgValOpt(args, "dot=")
    val cp = getCompositeArgValOpt(args, "cp=").fold(targetCP :: Nil)(_.map(_.toDouble))

    if (dotName.isEmpty) {
      GlobalParams(
        "",
        benchList,
        "",
        cp,
        fpga = true
      ) :: Nil
    } else {
      GlobalParams(
        "",
        dotName.get :: Nil,
        "",
        cp,
        fpga = true
      ) :: Nil
    }

  }

  def aggregate(params: Seq[GlobalParamsInst]): Unit = {}
}
