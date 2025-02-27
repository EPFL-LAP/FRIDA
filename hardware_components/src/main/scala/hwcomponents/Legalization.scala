package components

import scala.collection.mutable.HashSet
import arch.{Mux => AMux, Pin => APin, _}

// Transforms the graph to a legal VPR one
// VPR's representation cannot bypass registers, and pin->pin where both pins have a register won't work
// If only one regsiter, tries to put it on the pin with least approximation error
// Otherwise pick combinational path with value of the worst weight
// If two regsiters, bypasses, either
//   - Take the bypass path and weight it with the value of the critical path (reg2reg)
//   - Assume the bypass is going to a register..
//   - Just advertise the bypass path as anyway the tool cannot optimize internal reg2reg primitive paths,
//     so the delay has to be lower than the objective (should we maybe just not advertise reg2reg paths?????)

// For now assumes a single clock for the whole system

// TODO remove code duplication

object Legalizer {
  var regId: Int = 0
  val configurationBits = "configBits"

  def pin2pin(e: TimingEdge): Boolean = {
    e.source.isInstanceOf[AbsPin] && e.dest.isInstanceOf[AbsPin]
  }

  def linkedToReg(n: AbsPin, g: TimingGraph): Boolean = {
    val sourceToReg = g.edges
      .filter(_.source == n)
      .filter(_.dest.isInstanceOf[Register])
      .nonEmpty
    val regToSink = g.edges
      .filter(_.dest == n)
      .filter(_.source.isInstanceOf[Register])
      .nonEmpty

    sourceToReg || regToSink
  }

  def linkedToReset(e: TimingEdge): Boolean = {
    (e.source == ResetPin) || (e.dest == ResetPin)
  }

  def isLegal(e: TimingEdge, g: TimingGraph): Boolean = {
    !(pin2pin(e) && (linkedToReg(
      e.source.asInstanceOf[AbsPin],
      g
    ) || linkedToReg(e.dest.asInstanceOf[AbsPin], g)))
  }

  def isReg2Reg(e: TimingEdge): Boolean = {
    e.source.isInstanceOf[Register] && e.dest.isInstanceOf[Register]
  }

  def partition(g: TimingGraph): (Set[TimingEdge], Set[TimingEdge]) = {
    g.edges.partition(e => isReg2Reg(e))
  }

  // Find the critical path of this component
  def reg2reg(g: TimingGraph): Option[TimingEdge] = {
    val criticalPath = g.edges.foldLeft[Option[TimingEdge]](None) {
      (tightestEdge: Option[TimingEdge], e: TimingEdge) =>
        {
          if (isReg2Reg(e)) {
            if (tightestEdge.isEmpty) {
              Some(e)
            } else if (tightestEdge.get.delay.slack > e.delay.slack) {
              Some(e)
            } else {
              tightestEdge
            }
          } else {
            tightestEdge
          }
        }
    }

    // If none means combinatorial component
    criticalPath
  }

  def setCriticalPath(
      g: TimingGraph,
      criticalPath: Option[TimingEdge]
  ): TimingGraph = criticalPath match {
    case Some(TimingEdge(_, _, delay, _)) => {
      val nEdges = g.edges.map {
        case e @ TimingEdge(source, dest, d, attrs) => {
          if (source.isInstanceOf[Register] && dest.isInstanceOf[Register]) {
            TimingEdge(source, dest, delay, attrs)
          } else {
            e
          }
        }
      }

      TimingGraph(g.nodes, nEdges, g.attrs)
    }

    case None => g
  }

  def setupEqualization(g: TimingGraph): TimingGraph = {
    val (reg2regs, fromToPins) = partition(g)

    val nFromToPins = g.nodes
      .collect { case p: Pin => p; case ResetPin => ResetPin }
      .map {
        case p @ Pin(name, port) => {
          val pt = port.id.pt

          val usingPin = pt match {
            case PTInput  => fromToPins.filter(e => e.source == p)
            case PTOutput => fromToPins.filter(e => e.dest == p)
            case PTUndef  => ???
          }

          val (targetRegs, targetPin) = pt match {
            case PTInput =>
              usingPin.partition(e => e.dest.isInstanceOf[Register])
            case PTOutput =>
              usingPin.partition(e => e.source.isInstanceOf[Register])
            case PTUndef => ???
          }

          val minSlack = targetRegs.reduceOption((e1, e2) =>
            if (e1.delay.slack > e2.delay.slack) e2 else e1
          )

          if (minSlack.isEmpty) {
            usingPin
          } else {
            targetRegs.map(e =>
              TimingEdge(e.source, e.dest, minSlack.get.delay, e.attrs)
            ) ++ targetPin
          }
        }

        case ResetPin => {
          // guaranteed that reset target registers
          val targetRegs = fromToPins.filter(e => e.source == ResetPin)
          val minSlack = targetRegs.reduceOption((e1, e2) =>
            if (e1.delay.slack > e2.delay.slack) e2 else e1
          )

          targetRegs.map(e =>
            TimingEdge(e.source, e.dest, minSlack.get.delay, e.attrs)
          )
        }
      }
      .flatten

    TimingGraph(g.nodes, nFromToPins ++ reg2regs, g.attrs)
  }

  // Needs to be run when both setup equalization and setCriticalPath have been run
  // for all pins, merge target registers
  //   - Pick one target reg
  //   - for each target node of all target register re-link
  def pinToSingleReg(
      g: TimingGraph,
      regToSide: Map[Register, PortType]
  ): TimingGraph = {
    val (reg2regs, fromToPins) = partition(g)

    val nFromToPins = g.nodes
      .collect { case p: Pin => p; case ResetPin => ResetPin }
      .map {
        case p @ Pin(name, port) => {
          val pt = port.id.pt

          val usingPin = pt match {
            case PTInput  => fromToPins.filter(e => e.source == p)
            case PTOutput => fromToPins.filter(e => e.dest == p)
            case PTUndef  => ???
          }

          val (targetRegs, targetPin) = pt match {
            case PTInput =>
              usingPin.partition(e => e.dest.isInstanceOf[Register])
            case PTOutput =>
              usingPin.partition(e => e.source.isInstanceOf[Register])
            case PTUndef => ???
          }

          val (mergeWith, otherSide) = pt match {
            case PTInput =>
              targetRegs.partition(e =>
                regToSide(e.dest.asInstanceOf[Register]) == PTInput
              )
            case PTOutput =>
              targetRegs.partition(e =>
                regToSide(e.source.asInstanceOf[Register]) == PTOutput
              )
            case PTUndef => ???
          }

          if (mergeWith.isEmpty || (mergeWith.size == 1)) {
            mergeWith ++ targetPin
          } else {
            // TODO make sure that all the same timing
            val keptReg = mergeWith.head

            val nEdges = mergeWith.tail.map { e =>
              pt match {
                case PTInput => {
                  g.edges
                    .filter(edge => edge.source == e.dest)
                    .map(edge =>
                      TimingEdge(
                        keptReg.dest,
                        edge.dest,
                        edge.delay,
                        edge.attrs
                      )
                    )
                }
                case PTOutput => {
                  g.edges
                    .filter(edge => edge.dest == e.source)
                    .map(edge =>
                      TimingEdge(
                        edge.source,
                        keptReg.source,
                        edge.delay,
                        edge.attrs
                      )
                    )
                }
                case PTUndef => ???
              }
            }.flatten

            nEdges + keptReg
          }
        }

        case ResetPin => {
          val targetRegs = fromToPins.filter(e => e.source == ResetPin)
          val (mergeWith, otherSide) = targetRegs.partition(e =>
            regToSide(e.dest.asInstanceOf[Register]) == PTInput
          )

          if (mergeWith.isEmpty || (mergeWith.size == 1)) {
            mergeWith
          } else {
            val keptReg = mergeWith.head

            val nEdges = mergeWith.tail.map { e =>
              {
                g.edges
                  .filter(edge => edge.source == e.dest)
                  .map(edge =>
                    TimingEdge(keptReg.dest, edge.dest, edge.delay, edge.attrs)
                  )
              }
            }.flatten

            nEdges + keptReg
          }
        }
      }
      .flatten

    TimingGraph(g.nodes, nFromToPins ++ reg2regs, g.attrs)
  }

  def assignRegsToPins(g: TimingGraph): Map[Register, PortType] = {
    val (reg2regs, fromToPins) = partition(g)

    // For all registers which can have two potential assigns (single bypassable registers)
    // Assign to the side without bypass (with only pin2reg), if both random
    // Else randomly assigned
    val rToLoc = g.nodes
      .collect { case r: Register => r }
      .map {
        case r @ Register(name) => {
          val rPinNeighbours = fromToPins.filter(e =>
            ((e.source == r) && e.dest.isInstanceOf[AbsPin])
              || ((e.dest == r) && e.source.isInstanceOf[AbsPin])
          )

          val res = if (rPinNeighbours.isEmpty) {
            (r, None)
          } else {
            val leftPins = rPinNeighbours.filter(e => e.dest == r)
            val rightPins = rPinNeighbours.filter(e => e.source == r)

            if (
              leftPins.nonEmpty && leftPins.forall(pinEdge =>
                g.edges
                  .filter(e => pinEdge.source == e.source)
                  .forall(e => e.dest.isInstanceOf[Register])
              )
            ) {
              (r, Some(PTInput))
            } else {
              (r, Some(PTOutput))
            }
          }

          res
        }
      }
      .filter(_._2.isDefined)
      .map(p => (p._1, p._2.get))
      .toMap

    rToLoc
  }

  def handleBypassableRegs(g: TimingGraph): TimingGraph = {
    val (reg2regs, fromToPins) = partition(g)

    // pin2pin links with registers targeting same pins
    val toRewire = HashSet[TimingEdge]()
    val dummyfiedPorts = HashSet[TimingEntity]()
    val seen = HashSet[TimingEntity]()

    val nFromToPins = g.nodes
      .collect { case p: Pin => p; case ResetPin => ResetPin }
      .map {
        case p @ Pin(name, port) => {
          val pt = port.id.pt

          val usingPin = pt match {
            case PTInput  => fromToPins.filter(e => e.source == p)
            case PTOutput => fromToPins.filter(e => e.dest == p)
            case PTUndef  => ???
          }

          val (targetRegs, targetPin) = pt match {
            case PTInput =>
              usingPin.partition(e => e.dest.isInstanceOf[Register])
            case PTOutput =>
              usingPin.partition(e => e.source.isInstanceOf[Register])
            case PTUndef => ???
          }

          val edges = if (targetPin.nonEmpty && targetRegs.nonEmpty) {
            toRewire ++= targetPin
            dummyfiedPorts += p

            targetRegs
          } else {
            usingPin
          }

          seen += p

          edges
        }

        case ResetPin => {
          val targetRegs = fromToPins.filter(e => e.source == ResetPin)

          targetRegs
        }
      }
      .flatten
      .filter {
        case TimingEdge(src, dst, _, _) => {
          val pin2pin = src.isInstanceOf[AbsPin] && dst.isInstanceOf[AbsPin]
          val oneEndDummy =
            dummyfiedPorts.contains(src) || dummyfiedPorts.contains(dst)

          !pin2pin || !oneEndDummy
        }
      }
      .toSet

    var extendedSources = false
    var extendedDests = false

    // new edges and pins
    val (nRewNodes, nRewEdges) = toRewire.map { (e: TimingEdge) =>
      {
        if (linkedToReset(e)) {
          (Nil, e)
        } else {
          val sourceHasBoth = fromToPins.filter { edge =>
            {
              (edge.source == e.source) && edge.dest.isInstanceOf[Register]
            }
          }.nonEmpty

          val destHasBoth = fromToPins.filter { edge =>
            {
              (edge.dest == e.dest) && edge.source.isInstanceOf[Register]
            }
          }.nonEmpty

          val src = e.source.asInstanceOf[Pin]
          val dst = e.dest.asInstanceOf[Pin]

          if (sourceHasBoth && destHasBoth) {
            val nSource = Pin(src.name + "_dummy", src.port.toDummy)
            val nDest = Pin(dst.name + "_dummy", dst.port.toDummy)

            val nEdge = TimingEdge(nSource, nDest, e.delay, e.attrs)

            extendedSources = true
            extendedDests = true

            (nSource :: nDest :: Nil, nEdge)
          } else if (sourceHasBoth && !destHasBoth) {
            val nSource = Pin(src.name + "_dummy", src.port.toDummy)
            val nEdge = TimingEdge(nSource, e.dest, e.delay, e.attrs)

            extendedSources = true

            (nSource :: Nil, nEdge)
          } else if (!sourceHasBoth && destHasBoth) {
            val nDest = Pin(dst.name + "_dummy", dst.port.toDummy)
            val nEdge = TimingEdge(e.source, nDest, e.delay, e.attrs)

            extendedDests = true

            (nDest :: Nil, nEdge)
          } else {
            scala.sys.error(
              "Edge should be handled in the regular case, what is it doing here?"
            )
          }
        }
      }
    }.unzip

    TimingGraph(
      g.nodes ++ nRewNodes.flatten,
      nFromToPins ++ nRewEdges ++ reg2regs,
      Map()
    )
  }

  def removeUselessRegisgterNodes(g: TimingGraph): TimingGraph = {
    val (reg2regs, fromToPins) = partition(g)

    val removing = g.nodes.filter {
      case p: Pin => false
      case r: Register =>
        g.edges.filter(e => (e.source == r)).isEmpty || g.edges
          .filter(e => (e.dest == r))
          .isEmpty
      case resetPin => false
    }.toSet

    val nNodes = g.nodes.filter {
      case p: Pin => true
      case r: Register =>
        g.edges.filter(e => (e.source == r)).nonEmpty && g.edges
          .filter(e => (e.dest == r))
          .nonEmpty
      case resetPin => true
    }

    val nEdges = g.edges.filter { (e: TimingEdge) =>
      {
        !((removing contains e.source) || (removing contains e.dest))
      }
    }

    TimingGraph(nNodes, nEdges, g.attrs)
  }

  def isRegLinkedToPin(r: Register, g: TimingGraph): Boolean = {
    g.edges.filter { e =>
      {
        ((e.source == r) && e.dest
          .isInstanceOf[AbsPin]) || ((e.dest == r) && e.source
          .isInstanceOf[AbsPin])
      }
    }.nonEmpty
  }

  // TODO this function does not seem correct, double check
  // TODO Do I apply this to all reg2reg? Doesn't it seem to be too much?
  // TODO Both regs linked to a pin. So need to express it with dummy regs.
  // TODO Probably just handle it better with setup analysis. If both regs map to same pin, pix max setup.
  // TODO or just run setup equalization again and that should do? Probably this should work no?
  def pinRegToPinReg(g: TimingGraph): (Map[Register, Register], TimingGraph) = {
    val (reg2regs, fromToPins) = partition(g)

    val (violating, meeting) = reg2regs.partition { (e: TimingEdge) =>
      {
        isRegLinkedToPin(
          e.source.asInstanceOf[Register],
          g
        ) && isRegLinkedToPin(e.dest.asInstanceOf[Register], g)
      }
    }

    val regToInt = violating
      .map(_.dest.asInstanceOf[Register])
      .toSet
      .map { (r: Register) =>
        {
          val p = (r, Register(regId.toString()))
          regId = regId + 1
          p
        }
      }
      .toMap

    val (toUpdate, untouched) = meeting.partition(e =>
      regToInt.keySet contains e.source.asInstanceOf[Register]
    )
    val updated = toUpdate.map { e =>
      TimingEdge(
        regToInt(e.source.asInstanceOf[Register]),
        e.dest,
        e.delay,
        e.attrs
      )
    }

    val nLinkingEdges = regToInt.map { case (k, v) =>
      TimingEdge(k, v, reg2reg(g).get.delay, Map())
    }
    val rewires = violating.map { e =>
      TimingEdge(
        e.source,
        regToInt(e.dest.asInstanceOf[Register]),
        e.delay,
        e.attrs
      )
    }

    val nFromToPins = fromToPins.map {
      case e @ TimingEdge(source, dest, delay, attrs)
          if source.isInstanceOf[Register] => {
        if (regToInt.keySet contains source.asInstanceOf[Register]) {
          TimingEdge(
            regToInt(source.asInstanceOf[Register]),
            dest,
            delay,
            attrs
          )
        } else {
          e
        }
      }

      case other => other
    }

    val nEdges = nFromToPins ++ untouched ++ updated ++ nLinkingEdges ++ rewires

    (
      regToInt,
      TimingGraph(g.nodes ++ regToInt.map(_._2).toSet, nEdges, g.attrs)
    )
  }

  def isNodeConfiguration(n: TimingEntity): Boolean = n match {
    case Register(name) => name.contains(configurationBits)
    case other          => false
  }

  def isEdgeConfiguration(e: TimingEdge): Boolean = e match {
    case TimingEdge(src, dst, _, _) => {
      isNodeConfiguration(src) || isNodeConfiguration(dst)
    }
  }

  // TODO make generic, or enforce naming in chisel in a way
  def removeConfigurationBits(g: TimingGraph): TimingGraph = {
    val nNodes = g.nodes.filter {
      case r @ Register(name) => !isNodeConfiguration(r)
      case other              => true
    }

    val nEdges = g.edges.filter { case e @ TimingEdge(src, dst, _, _) =>
      !isEdgeConfiguration(e)
    }

    TimingGraph(nNodes, nEdges, g.attrs)
  }

  def outputDanglingToZeroClockToQ(g: TimingGraph): TimingGraph = {
    var dang = 0

    val (nNodes, nEdges) = g.nodes.map { (n: TimingEntity) =>
      {
        if (g.edges.filter(e => (e.source == n) || (e.dest == n)).isEmpty) {
          val nReg = Register("dang_" + dang)
          val nEdge = TimingEdge(nReg, n, Delay.zero, Map())

          (Some(nReg), Some(nEdge))
        } else {
          (None, None)
        }
      }
    }.unzip

    val tNodes = g.nodes ++ nNodes.filter(_.isDefined).map(_.get)
    val tEdges = g.edges ++ nEdges.filter(_.isDefined).map(_.get)

    TimingGraph(tNodes, tEdges, g.attrs)
  }

  def apply(g: TimingGraph): TimingGraph = {
    val gWithoutConfig = removeConfigurationBits(g)
    val criticalPath = reg2reg(gWithoutConfig)
    val bp = handleBypassableRegs(gWithoutConfig)
    val (regToInt, normalized) = pinRegToPinReg(
      setupEqualization(setCriticalPath(bp, criticalPath))
    )
    val regLocs = assignRegsToPins(normalized)
    val nG = removeUselessRegisgterNodes(pinToSingleReg(normalized, regLocs))
    val nRegLocs = assignRegsToPins(nG)
    val fG = pinToSingleReg(setupEqualization(nG), nRegLocs)

    verifier(gWithoutConfig, fG, regToInt)

    outputDanglingToZeroClockToQ(fG)
  }

  def edgesFromPin(
      g: TimingGraph,
      n: Pin
  ): (Set[TimingEdge], Set[TimingEdge]) = {
    val fromPin = g.edges.filter(e => (e.source == n) || (e.dest == n))
    val (edgesWithReg, combEdges) = fromPin.partition { e =>
      e.source.isInstanceOf[Register] || e.dest.isInstanceOf[Register]
    }

    (edgesWithReg, combEdges)
  }

  def sameSetup(
      origSet: Set[TimingEdge],
      nSet: Set[TimingEdge],
      regToInt: Map[Register, Register]
  ): Unit = {
    val mappedOrig = origSet.map { e =>
      {
        if (
          e.source.isInstanceOf[Register]
          && e.dest.isInstanceOf[Pin]
          && (regToInt.keySet contains e.source.asInstanceOf[Register])
        ) {
          TimingEdge(
            regToInt(e.source.asInstanceOf[Register]),
            e.dest,
            e.delay,
            e.attrs
          )
        } else {
          e
        }
      }
    }

    if (!mappedOrig.isEmpty && !nSet.isEmpty) {
      val origSlack = mappedOrig.reduce((e1, e2) =>
        if (e1.delay.slack > e2.delay.slack) e2 else e1
      )
      val nSlack =
        nSet.reduce((e1, e2) => if (e1.delay.slack > e2.delay.slack) e2 else e1)

      if (origSlack.delay.slack != nSlack.delay.slack) {
        println(
          "Slack not matching in: origSlack= " + origSlack + ", nSlack= " + nSlack
        )
        assert(origSlack.delay.slack == nSlack.delay.slack)
      }
    } else {
      if (!(mappedOrig.isEmpty && nSet.isEmpty)) {
        println("mappedOrig: " + mappedOrig.isEmpty + " && " + nSet.isEmpty)
        println(mappedOrig)
        assert(mappedOrig.isEmpty && nSet.isEmpty)
      }
    }
  }

  def sameComb(origSet: Set[TimingEdge], nSet: Set[TimingEdge]): Unit = {
    if (!origSet.isEmpty && !nSet.isEmpty) {
      val origGrouped = origSet.groupBy(e => (e.source, e.dest))
      val nGrouped = nSet.groupBy(e => (e.source, e.dest))

      origGrouped.keySet.foreach { k =>
        {
          assert(nGrouped(k).size == 1)
          assert(origGrouped(k).size == 1)
          assert(nGrouped(k).head.delay == origGrouped(k).head.delay)
        }
      }
    } else {
      if (!(origSet.isEmpty && nSet.isEmpty)) {
        println("origSet: " + origSet.isEmpty + " && " + nSet.isEmpty)
        assert(origSet.isEmpty && nSet.isEmpty)
      }
    }
  }

  def allRegsCrit(g: TimingGraph): Unit = {
    val crit = reg2reg(g)

    assert(
      g.edges
        .filter(e =>
          e.source.isInstanceOf[Register] && e.dest.isInstanceOf[Register]
        )
        .forall(e => e.delay == crit.get.delay)
    )
  }

  def verifier(
      orig: TimingGraph,
      transformed: TimingGraph,
      regToInt: Map[Register, Register]
  ): Unit = {
    val critOrig = reg2reg(orig)
    val critTransformed = reg2reg(transformed)

    critOrig
      .fold[Option[(TimingEdge, TimingEdge)]](None)(t =>
        Some((t, critTransformed.get))
      )
      .foreach {
        case (a, b) => {
          if (a.delay != b.delay) {
            println("Crit Orig: " + a + " != critTransformed: " + b)
            assert(a.delay == b.delay)
          }
        }
      }

    orig.nodes.collect { case p: Pin => p }.foreach { (p: Pin) =>
      {
        val (origWithReg, origComb) = edgesFromPin(orig, p)
        val (nWithReg, nComb) = edgesFromPin(transformed, p)

        sameSetup(origWithReg, nWithReg, regToInt)

        val dumP = Pin(p.name + "_dummy", p.port.toDummy)
        val (dummyWithReg, dummyComb) = edgesFromPin(transformed, dumP)

        val nCombNorm = (nComb ++ dummyComb).map { (e: TimingEdge) =>
          {
            val src = e.source match {
              case p @ Pin(name, port) =>
                if (port.isDummy)
                  Pin(name.replace("_dummy", ""), port.toRegular)
                else p
              case other => other
            }

            val dst = e.dest match {
              case p @ Pin(name, port) =>
                if (port.isDummy)
                  Pin(name.replace("_dummy", ""), port.toRegular)
                else p
              case other => other
            }

            TimingEdge(src, dst, e.delay, e.attrs)
          }
        }

        // remove attributes as we might have added some annotations in this pass
        val origRaw =
          origComb.map(e => TimingEdge(e.source, e.dest, e.delay, Map()))
        val nCombRaw =
          nCombNorm.map(e => TimingEdge(e.source, e.dest, e.delay, Map()))

        sameComb(origRaw, nCombRaw)
      }
    }

    allRegsCrit(transformed)
  }
}
