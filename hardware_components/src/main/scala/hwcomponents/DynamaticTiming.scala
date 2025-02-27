package components

import arch.{Mux => AMux, Pin => APin, _}

import java.io.File
import java.io.FileWriter

object DynamaticTiming {
  val unusedT = 100.0

  // should reflect contnet of ComponentTiming.h in dynamatic/elastic-pass/include
  val compNameToIndex = Map[String, Int](
    "Icmp" -> 0,
    "Add" -> 1,
    "Sub" -> 2,
    "Mult" -> 3,
    "Sext" -> 4,
    "Load" -> 5,
    "Store" -> 6,
    "LsqLoad" -> 7,
    "LsqStore" -> 8,
    "Merge" -> 9,
    "Getelementptr" -> 10,
    "Fadd" -> 11,
    "Fsub" -> 12,
    "FMul" -> 13,
    "Udiv" -> 14,
    "Sdiv" -> 15,
    "Fdiv" -> 16,
    "Fcmp" -> 17,
    "Phic" -> 18,
    "zdl" -> 19,
    "Fork" -> 20,
    "Ret" -> 21,
    "Branch" -> 22,
    "End" -> 23,
    "And" -> 24,
    "Or" -> 25,
    "Xor" -> 26,
    "Shl" -> 27,
    "Ashr" -> 28,
    "Lshr" -> 29,
    "Select" -> 30,
    "Mux" -> 31
  )

  def nonComposite(name: String): (String, List[String]) = {
    name -> (name :: Nil)
  }

  def unused(name: String): (String, List[String]) = {
    name -> Nil
  }

  val primNameToChiselName = Map[String, Option[String]](
    "Icmp" -> Some("ALU"),
    "Add" -> Some("ALU"),
    "Sub" -> Some("ALU"),
    "Mult" -> Some("Mult"),
    "Sext" -> None, // We never sign extend for now because we have only 1 data type.
    "Load" -> None,
    "Store" -> None,
    "LsqLoad" -> None,
    "LsqStore" -> None,
    "Merge" -> Some("Merge"),
    "Getelementptr" -> None,
    "Fadd" -> None,
    "Fsub" -> None,
    "FMul" -> None,
    "Udiv" -> None,
    "Sdiv" -> None,
    "Fdiv" -> None,
    "Fcmp" -> None,
    "Phic" -> Some("CntrlMerge"),
    "zdl" -> None, // Zero delay component?
    "Fork" -> Some("Fork"),
    "Ret" -> None, // Should be TEHB right?
    "Branch" -> Some("Branch"),
    "End" -> None, // Again, what to do with this, for now is IO but if in timing model wel... Maybe should add it?
    "And" -> Some("ALU"),
    "Or" -> Some("ALU"),
    "Xor" -> Some("ALU"),
    "Shl" -> Some("ALU"),
    "Ashr" -> Some("ALU"),
    "Lshr" -> Some("ALU"),
    "Select" -> Some("Mux"),
    "Mux" -> Some("Mux"),
    "CmpMax" -> Some("ALU") // What is this??
  )

  val compositeToNames = Map[String, List[String]](
    "ALU" -> ("Icmp" :: "Add" :: "Sub" :: "And" :: "Or" :: "Xor" :: "Shl" :: "Ashr" :: "Lshr" :: Nil),
    nonComposite("Mult"),
    nonComposite("Fork"),
    nonComposite("Merge"),
    nonComposite("Branch"),
    "CntrlMerge" -> ("phiC" :: Nil),
    "Mux" -> ("Select" :: "Mux" :: Nil),
    unused(
      "End"
    ), // TODO figure out how this component is exactly handled..............
    unused("getelementptr")
  )

  // assumes timing edges treated as bidirectional? TODO double check with Andrea or Carmine
  // TODO check that pin not linked to register!!
  def getWorseTiming(
      g: TimingGraph,
      hsSrc: Option[HSType],
      hsDst: Option[HSType],
      srcPm: Option[PortMeaning],
      dstPm: Option[PortMeaning]
  ): Option[TimingEdge] = {
    def checkPM(e: TimingEdge): Boolean = e match {
      case TimingEdge(Pin(_, p0), Pin(_, p1), _, _) => {
        ((srcPm.isEmpty || (p0.id.pmw.pm == srcPm.get)) && (dstPm.isEmpty || (p1.id.pmw.pm == dstPm.get))) ||
        ((srcPm.isEmpty || (p1.id.pmw.pm == srcPm.get)) && (dstPm.isEmpty || (p0.id.pmw.pm == dstPm.get)))
      }

      case other => false
    }

    val matchingEdges = g.edges
      .filter { case e @ TimingEdge(src, dst, _, _) =>
        src.isInstanceOf[Pin] && dst.isInstanceOf[Pin] && checkPM(e)
      }
      .filter {
        case TimingEdge(Pin(n0, p0), Pin(n1, p1), _, _) => {
          ((p0.hsType == hsSrc) && (p1.hsType == hsDst)) || ((p0.hsType == hsDst) && (p1.hsType == hsSrc))
        }

        case other => false
      }

    matchingEdges.reduceOption { (e0, e1) =>
      {
        if (e0.delay.slack < e1.delay.slack) {
          e0
        } else {
          e1
        }
      }
    }
  }

  def foldTiming(opt: Option[TimingEdge]): Double = {
    opt.fold(0.0)(_.delay.arrivalTime)
  }

  def getWidth(
      compSizes: List[(String, TimingGraph)],
      width: Int
  ): (Double, Double, Double) = {
    val compW = if (width == 0) {
      compSizes // TODO fixme, only used by CntrlMerge
    } else if (width == 1) {
      val t = compSizes.filter(_._1 contains width.toString())
      if (t.isEmpty) {
        compSizes.filter(_._1 contains 32.toString())
      } else {
        t
      }
    } else {
      compSizes.filter(_._1 contains width.toString())
    }

    if (compW.isEmpty) {
      (unusedT, unusedT, unusedT)
    } else {
      val (_, compG) = compW.head

      val delayData = foldTiming(getWorseTiming(compG, None, None, None, None))
      val delayValid = foldTiming(
        getWorseTiming(compG, Some(HSValid), Some(HSValid), None, None)
      )
      val delayReady = foldTiming(
        getWorseTiming(compG, Some(HSReady), Some(HSReady), None, None)
      )

      println("(" + delayData + ", " + delayValid + ", " + delayReady + ")")

      (delayData, delayValid, delayReady)
    }
  }

  // VR, CV, CR, VC, VD
  def getCrossings(
      compSizes: List[(String, TimingGraph)]
  ): (Double, Double, Double, Double, Double) = {
    val compG = compSizes
      .map {
        case (n: String, g: TimingGraph) => {
          val width =
            """W\d+""".r.findFirstIn(n).fold(32)(_.filter(_.isDigit).toInt)
          (width, g)
        }
      }
      .reduce[(Int, TimingGraph)] {
        case (g0: (Int, TimingGraph), g1: (Int, TimingGraph)) => {
          if (g0._1 > g1._1) {
            g0
          } else {
            g1
          }
        }
      }
      ._2

    // Is this exhaustive? How about rv, rd and other things?
    val vr = foldTiming(
      getWorseTiming(compG, Some(HSValid), Some(HSReady), None, None)
    )
    val cv = foldTiming(
      getWorseTiming(compG, None, Some(HSValid), Some(PMCond(None)), None)
    )
    val cr = foldTiming(
      getWorseTiming(compG, None, Some(HSReady), Some(PMCond(None)), None)
    )
    val vc = foldTiming(
      getWorseTiming(compG, Some(HSValid), None, None, Some(PMCond(None)))
    )
    val vd = foldTiming(getWorseTiming(compG, Some(HSValid), None, None, None))

    (vr, cv, cr, vc, vd)
  }

  def getComp(
      compName: String,
      comps: Map[String, List[(String, TimingGraph)]]
  ): String = {
    println("handling: " + compName)

    val (dd1, dv1, dr1) = if (compName == "CntrlMerge") {
      getWidth(comps(compName), 0)
    } else {
      getWidth(comps(compName), 1)
    }

    val (dd32, dv32, dr32) = if (compName == "CntrlMerge") {
      getWidth(comps(compName), 0)
    } else {
      getWidth(comps(compName), 32)
    }

    val (vr, cv, cr, vc, vd) = getCrossings(comps(compName))

    val ds = (dd1 :: List.fill(6)(dd32) ++:
      (dv1 :: List.fill(6)(dv32) ++:
        (dr1 :: List.fill(6)(dr32) ++:
          (vr :: cv :: cr :: vc :: vd :: Nil))))

    ds.mkString(",")
  }

  def getUnusedComp(): String = {
    List.fill(7 * 3 + 5)(unusedT).mkString(",")
  }

  def getZeroDelayComp(): String = {
    (List.fill(7 * 3)(0) ++: List.fill(5)(unusedT)).mkString(",")
  }

  def getZeroLatencyComp(): String = {
    List.fill(7)(0).mkString(", ")
  }

  def apply(
      comps: Map[String, List[(String, TimingGraph)]],
      dynDelay: String,
      dynLat: String
  ): Unit = {
    val withNames: Boolean = false // Debug only, to double check row ordering

    val dynTiming = compNameToIndex
      .map {
        case (compName: String, i: Int) => {
          val namePref = if (withNames) compName + " -> " else ""

          val equPrim = primNameToChiselName(compName)
          if (equPrim.nonEmpty) {
            (i, namePref + getComp(equPrim.get, comps))
          } else if (compName == "zdl") {
            (i, namePref + getZeroDelayComp())
          } else {
            (i, namePref + getZeroDelayComp())
          }
        }
      }
      .toList
      .sortBy(_._1)
      .map(_._2)
      .mkString("\n")

    val dynLatency = compNameToIndex
      .map {
        case (compName: String, i: Int) => {
          val namePref = if (withNames) compName + " -> " else ""

          namePref + getZeroLatencyComp()
        }
      }
      .mkString("\n")

    val del = new File(dynDelay)
    del.getParentFile().mkdirs()
    val delW = new FileWriter(del)

    delW.write(dynTiming)
    delW.write("\n")
    delW.close()

    val lat = new File(dynLat)
    lat.getParentFile().mkdirs()
    val latW = new FileWriter(lat)

    latW.write(dynLatency)
    latW.write("\n")
    latW.close()
  }
}
