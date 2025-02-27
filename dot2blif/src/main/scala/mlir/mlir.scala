package mlir

import crkt.MlirToCrktConverter
import archs._
import util.Util
import util.Util.log2ceil

case class Value(name: String, num: Option[Int], isArg: Boolean) {
  def str(): String = {
    val sep = if (isArg) "#" else ":"
    val suffix = if (num.isDefined) sep + num.get else ""

    name + suffix
  }

  def flipped = Value(name, num, !isArg)
}

case class Argument(v: Value, t: MLIRType) {
  def str(): String = "" + v.str() + " : " + t.str()
}

object PrefixedName {
  // We assume BlackBox components are moved behind the IO and have been removed
  def fromPrim(p: TPrim): PrefixedName = {
    p match {
      case Fork(p)       => {
        p.variant match {
          case EagerFork => PrefixedName("handshake", "fork")
          case LazyFork  => PrefixedName("handshake", "lazy_fork")
          case other => scala.sys.error("Expected previse fork implementation.")
        }
      }
      case Merge(_)      => PrefixedName("handshake", "merge")
      case Mux(_)        => PrefixedName("handshake", "mux")
      case Select(_)     => PrefixedName("handshake", "select")
      case Branch(_)     => PrefixedName("handshake", "cond_br")
      case Constant(_)   => PrefixedName("handshake", "constant")
      case TEHB(p)       => PrefixedName("handshake", "buffer")
      case OEHB(p)       => PrefixedName("handshake", "buffer")
      case CntrlMerge(_) => PrefixedName("handshake", "control_merge")
      case Sink(_)       => PrefixedName("handshake", "sink")
      case Source(_)     => PrefixedName("handshake", "source")
      case Extsi(_)      => PrefixedName("handshake", "extsi")
      case Extui(_)      => PrefixedName("handshake", "extui")
      case Trunc(_)      => PrefixedName("handshake", "trunci")

      case Mult(_) => PrefixedName("handshake", "muli")

      case Operator(op) => {
        op.op match {
          case ALUOperation.add   => PrefixedName("handshake", "addi")
          case ALUOperation.sub   => PrefixedName("handshake", "subi")
          case ALUOperation.and   => PrefixedName("handshake", "andi")
          case ALUOperation.or    => PrefixedName("handshake", "ori")
          case ALUOperation.shl   => PrefixedName("handshake", "shli")
          case ALUOperation.xor   => PrefixedName("handshake", "xori")
          case ALUOperation.ashr  => ??? // TODO What is this already?
          case ALUOperation.shrsi   => PrefixedName("handshake", "shrsi")
          case ALUOperation.shrui   => PrefixedName("handshake", "shrui")
          case ALUOperation.lshr => PrefixedName("handshake", "lshr")
          case ALUOperation.ult => PrefixedName("handshake", "ult")
          case ALUOperation.ule => PrefixedName("handshake", "ule")
          case ALUOperation.slt => PrefixedName("handshake", "slt")
          case ALUOperation.sle => PrefixedName("handshake", "sle")
          case ALUOperation.eqop => PrefixedName("handshake", "eq")
          case ALUOperation.neop => PrefixedName("handshake", "ne")
          case ALUOperation.ugt => PrefixedName("handshake", "ugt")
          case ALUOperation.uge => PrefixedName("handshake", "uge")
          case ALUOperation.sgt => PrefixedName("handshake", "sgt")
          case ALUOperation.sge => PrefixedName("handshake", "sge")
          case ALUOperation.anyop => scala.sys.error("Expected defined operation.")
        }
      }

      case Comparator(op) => PrefixedName("handshake", "cmpi")

      case BlackBox(p) => {
        val split = p.name.split('.')
        assert(split.size == 2, split.toList)

        PrefixedName(split(0), split(1))
      }

      case other => scala.sys.error("Unexpected primitive: " + p.typeString)
    }
  }
}

case class PrefixedName(dialect: String, name: String) {
  def str(): String = "\"" + dialect + "." + name + "\""
}

sealed trait MLIRType {
  def str(): String
}

sealed trait MLIRInteger extends MLIRType

case class MLIRInt(width: Int) extends MLIRInteger {
  def str(): String = "i" + width
}

case class MLIRUInt(width: Int) extends MLIRInteger {
  def str(): String = "ui" + width
}

case class MLIRFunc(ins: List[MLIRType], outs: List[MLIRType]) extends AttrValue with MLIRType {
  def str(): String = {
    val results = if (outs.size == 1) {
      outs.head.str()
    } else {
      "(" + outs.map(_.str()).mkString(", ") + ")"
    }

    "(" + ins.map(_.str()).mkString(", ") + ")" + " -> " + results
  }
}

case class MLIRMemref(shape: List[Int], t: MLIRInteger) extends MLIRType {
  def str(): String = "memref<" + shape.mkString("x") + "x" + t.str() + ">"
}

case object MLIRHs extends MLIRType {
  def str(): String = "none" // TODO update with the new type system
}

case object MLIRControl extends MLIRType {
  def str(): String = "!handshake.control<>"
}

case class MLIRChannel(integer: MLIRInteger) extends MLIRType {
  def str(): String = "!handshake.channel<" + integer.str() + ">"
}

sealed trait AttrValue {
  def str(): String
}

case class AttrInteger(value: Int, t: MLIRInteger) extends AttrValue {
  def str(): String = "" + value + " : " + t.str()
}

case class AttrBoolean(b: Boolean) extends AttrValue {
  def str(): String = b.toString()
}

case class AttrString(value: String) extends AttrValue {
  def str(): String = "\"" + value + "\""
}

case class ArrayAttr(value: List[AttrValue]) extends AttrValue {
  def str(): String = "[" + value.map(_.str()).mkString(", ") + "]"
}

case class BufConstrAttr(value: List[BufConstraint]) extends AttrValue {
  def str(): String = {
    "#handshake<bufProps{" + value.map(_.str()).mkString(", ") + "}>"
  }
}

case class BufConfigAttr(slots: Int, cutData: Boolean, cutVld: Boolean, cutRdy: Boolean) extends AttrValue {
  def str(): String = {
    val D = if (cutData) 1 else 0
    val V = if (cutVld) 1 else 0
    val R = if (cutRdy) 1 else 0

    "{NUM_SLOTS = " + slots + " : ui32, TIMING = #handshake<timing {D: " + D + ", V: " + V + ", R: " + R + "}>}"
  }
}

case class Dependency(opName: String, loopDepth: Int, components: List[(BigInt, BigInt)]) {
  def str(): String = {
    val comps = components
      .map(
        (k, v) => ("[" + k + ", " + v + "]")
      )
      .mkString(", ")
    val compsStr = if (components.isEmpty) "" else " [" + comps + "]"
    "<" + "\"" + opName + "\"" + " (" + loopDepth + ")" + compsStr + ">"
  }
}

case class DepsAttr(value: List[Dependency]) extends AttrValue {
  def str(): String = {
    "#handshake<deps[" + value.map(_.str()).mkString(", ") + "]>"
  }
}

sealed trait MemInterface
case object MC extends MemInterface
case object LSQ extends MemInterface

case class MemInterfaceAttr(interface: MemInterface) extends AttrValue {
  def str(): String = {
    "#handshake.mem_interface<" + interface + ">"
  }
}

case class Attr(name: String, value: AttrValue) {
  def str(indent: Int): String = name + " = " + value.str()
}

case class BufSlotConstraint(minT: Int, maxT: Int, minO: Int, maxO: Int) {
  override def toString(): String = {
    val maxTStr = if (maxT == Integer.MAX_VALUE) "inf" else maxT
    val maxOStr = if (maxO == Integer.MAX_VALUE) "inf" else maxO

    "T[" + minT + ", " + maxTStr + "] " + "O[" + minO + ", " + maxOStr + "]"
  }
}

case class BufDelayConstraint(inDelay: Double, outDelay: Double, delay: Double) {
  override def toString(): String = {
    f"$delay%1.3f"
  }

  def isEmpty = (inDelay == 0) && (outDelay == 0) && (delay == 0)
  def nonEmpty = (inDelay != 0) || (outDelay != 0) || (delay != 0)
}

// All delay values are in ns
case class BufConstraint(
    argNum: Int,
    slotConstr: BufSlotConstraint,
    delayConstr: BufDelayConstraint
) {
  val minT = slotConstr.minT
  val maxT = slotConstr.maxT
  val minO = slotConstr.minO
  val maxO = slotConstr.maxO

  val inDelay = delayConstr.inDelay
  val outDelay = delayConstr.outDelay
  val delay = delayConstr.delay

  // TODO a bit hacky but will do for now
  def formatNs(d: Double): String = {
    f"$d%1.6f" + "e+00"
  }

  def str(): String = {
    val maxTStr = if (maxT == Integer.MAX_VALUE) "inf" else maxT
    val maxOStr = if (maxO == Integer.MAX_VALUE) "inf" else maxO

    "\"" + argNum + "\"" + ": " + "[" + minT + "," + maxTStr + "]" + ", " + "[" + minO + "," + maxOStr + "]"
      + ", " + formatNs(inDelay) + ", " + formatNs(outDelay) + ", " + formatNs(delay)
  }
}

sealed trait ConfigValue extends AttrValue

object CmpCfg {
  def fromCmpOp(op: ComparatorOperation): CmpCfg = {
    op match {
      case CmpUlt     => Pult
      case CmpUle     => Pule
      case CmpSlt     => Pslt
      case CmpSle     => Psle
      case CmpEq    => Peq
      case CmpNe    => Pne
      case CmpUgt     => Pugt
      case CmpUge     => Puge
      case CmpSgt     => Psgt
      case CmpSge     => Psge
      case CmpAnyComp => ???
    }
  }
}

sealed trait CmpCfg extends ConfigValue
case object Peq extends CmpCfg { def str() = "0 : i64" }
case object Pne extends CmpCfg { def str() = "1 : i64" }
case object Pslt extends CmpCfg { def str() = "2 : i64" }
case object Psle extends CmpCfg { def str() = "3 : i64" }
case object Psgt extends CmpCfg { def str() = "4 : i64" }
case object Psge extends CmpCfg { def str() = "5 : i64" }
case object Pult extends CmpCfg { def str() = "6 : i64" }
case object Pule extends CmpCfg { def str() = "7 : i64" }
case object Pugt extends CmpCfg { def str() = "8 : i64" }
case object Puge extends CmpCfg { def str() = "9 : i64" }

object ConfigPredicate {
  def integerMap(i: Int): CmpCfg = {
    i match {
      case 0 => Peq
      case 1 => Pne
      case 2 => Pslt
      case 3 => Psle
      case 4 => Psgt
      case 5 => Psge
      case 6 => Pult
      case 7 => Pule
      case 8 => Pugt
      case 9 => Puge
    }
  }
}

type AttrMap = Map[String, AttrValue]
type ConfigMap = Map[String, ConfigValue]

case class GenericOp(
    name: PrefixedName,
    args: List[Value],
    attrs: AttrMap,
    regions: List[Region],
    config: ConfigMap,
    t: MLIRFunc
) {
  def str(indentLevel: Int): String = {
    val indent = " ".repeat(indentLevel)

    val argsStr = args.map(_.str()).mkString(", ")
    val regionsStr = regions.map(_.str(indentLevel + 1)).mkString(",\n" + indent)
    val attrsStr = attrs
      .map(
        (k, v) => "" + k + " = " + v.str()
      )
      .mkString(", ")
    val configStr = config
      .map(
        (k, v) => "" + k + " = " + v.str()
      )
      .mkString(", ")

    val suf = if (indentLevel == 0) "\n" else ""

    "" + name.str()
      + "(" + argsStr + ")"
      + (if (config.nonEmpty) " <{" + configStr + "}>" else "")
      + (if (regions.nonEmpty) " (" + regionsStr + ")" else "")
      + (if (attrs.nonEmpty) " {" + attrsStr + "}" else "")
      + " : " + t.str() + suf
  }
}

sealed trait MLIREntity

case class Region(ops: List[Operation], blocks: List[Block]) extends MLIREntity {
  def str(indentLevel: Int): String = {
    val indent = " ".repeat(indentLevel * 2)
    val pIndent = " ".repeat((indentLevel - 1) * 2)

    // TODO put back the indentation level increase here...
    val opsStr = ops.map(_.str(indentLevel)).mkString("\n" + indent)
    val blocksStr = blocks.map(_.str(indentLevel)).mkString("\n" + indent)

    "{" + "\n" + indent + opsStr + blocksStr + "\n" + pIndent + "}"
  }
}

case class Block(name: String, args: List[Argument], ops: List[Operation]) extends MLIREntity {
  def str(indentLevel: Int): String = {
    val indent = " ".repeat(indentLevel * 2)

    val argsStr = args.map(_.str()).mkString(", ")
    val opsStr = ops.map(_.str(indentLevel)).mkString("\n" + indent)

    "^" + name + "(" + argsStr + ")" + ":\n" + indent + opsStr
  }
}

case class Operation(values: List[Value], genOp: GenericOp) extends MLIREntity {
  def str(indentLevel: Int): String = {
    if (values.isEmpty) {
      genOp.str(indentLevel)
    } else {
      values.map(_.str()).mkString(", ") + " = " + genOp.str(indentLevel)
    }
  }

  def indexOf(value: Value): Int = {
    if(value.isArg) {
      genOp.args.indexOf(value)
    } else {
      values.indexOf(value)
    }
  }

  def getOutWidth: Int = {
    genOp.t.outs.head match {
      case MLIRInt(w)               => w
      case MLIRUInt(w)              => w
      case MLIRHs                   => 0
      case MLIRControl              => 0
      case MLIRChannel(MLIRInt(w))  => w
      case MLIRChannel(MLIRUInt(w)) => w
      case other                    => scala.sys.error("Unexpected type.")
    }
  }

  def getInWidth: Int = {
    genOp.t.ins.head match {
      case MLIRInt(w)               => w
      case MLIRUInt(w)              => w
      case MLIRHs                   => 0
      case MLIRControl              => 0
      case MLIRChannel(MLIRInt(w))  => w
      case MLIRChannel(MLIRUInt(w)) => w
      case other                    => scala.sys.error("Unexpected type.")
    }
  }

  def getSlots: Int = {
    if (!genOp.attrs.contains("slots")) {
      assert(genOp.name.name == "return")

      1
    } else {
      val slotsVal = genOp.attrs("slots")
      assert(slotsVal.isInstanceOf[AttrInteger])

      slotsVal.asInstanceOf[AttrInteger].value
    }
  }

  def toPrim: Primitive = {
    genOp.name.name match {
      case "fork" => Fork(ForkParams(getOutWidth, values.head.num.getOrElse(1), EagerFork))
      case "lazy_fork" => {
        Fork(ForkParams(getOutWidth, values.head.num.getOrElse(1), LazyFork))
      }
      case "constant"      => Constant(ConstantParams(getOutWidth))
      case "mux"           => Mux(MuxParams(getOutWidth, (genOp.args.size - 1)))
      case "select"        => Select(SelectParams(getOutWidth, (genOp.args.size - 1)))
      case "merge"         => Merge(MergeParams(getOutWidth, genOp.args.size))
      case "control_merge" => CntrlMerge(CntrlMergeParams(genOp.args.size, log2ceil(genOp.args.size).toInt))
      case "source"        => Source(SourceParams())
      case "sink"          => Sink(SinkParams(getInWidth))
      case "oehb"          => OEHB(OEHBParams(getOutWidth, getSlots))
      case "tehb"          => TEHB(TEHBParams(getOutWidth, getSlots))
      case "buffer" => {
        val bufAttrs = genOp.attrs.filter(_._1 == "hw.parameters")
        assert(bufAttrs.size == 1)

        val bufConfig = bufAttrs("hw.parameters").asInstanceOf[BufConfigAttr]

        if (bufConfig.cutData && bufConfig.cutVld && !bufConfig.cutRdy) {
          OEHB(OEHBParams(getOutWidth, bufConfig.slots))
        } else if (!bufConfig.cutData && !bufConfig.cutVld && bufConfig.cutRdy) {
          TEHB(TEHBParams(getOutWidth, bufConfig.slots))
        } else {
          ???
        }
      }
      case "cond_br" => Branch(BranchParams(getOutWidth))
      case "return" => {
        // Should be inserted by the buffer placement...
        // Ensures it will be removed...
        Fork(ForkParams(getOutWidth, 1, EagerFork))
      }
      case "subi"   => Operator(OperatorParams(getInWidth, ALUOperation.sub))
      case "addi"   => Operator(OperatorParams(getInWidth, ALUOperation.add))
      case "andi"   => Operator(OperatorParams(getInWidth, ALUOperation.and))
      case "ori"    => Operator(OperatorParams(getInWidth, ALUOperation.or))
      case "shli"   => Operator(OperatorParams(getInWidth, ALUOperation.shl))
      case "shrsi"   => Operator(OperatorParams(getInWidth, ALUOperation.shrsi))
      case "shrui"   => Operator(OperatorParams(getInWidth, ALUOperation.shrui))
      case "xori"   => Operator(OperatorParams(getInWidth, ALUOperation.xor))
      case "muli"   => Mult(MultParams(getOutWidth))
      case "extsi"  => Extsi(ExtsiParams(getInWidth, getOutWidth))
      case "extui"  => Extui(ExtuiParams(getInWidth, getOutWidth))
      case "trunci" => Trunc(TruncParams(getInWidth, getOutWidth))

      case "cmpi" => {
        val cmpAttrs = genOp.attrs.filter(_._1 == "predicate")
        assert(cmpAttrs.size == 1)

        val config = ConfigPredicate.integerMap(cmpAttrs("predicate").asInstanceOf[AttrInteger].value)

        config match {
          case Peq  => Comparator(ComparatorParams(getInWidth, CmpEq))
          case Pne  => Comparator(ComparatorParams(getInWidth, CmpNe))
          case Pslt => Comparator(ComparatorParams(getInWidth, CmpSlt))
          case Psle => Comparator(ComparatorParams(getInWidth, CmpSle))
          case Psgt => Comparator(ComparatorParams(getInWidth, CmpSgt))
          case Psge => Comparator(ComparatorParams(getInWidth, CmpSge))
          case Pult => Comparator(ComparatorParams(getInWidth, CmpUlt))
          case Pule => Comparator(ComparatorParams(getInWidth, CmpUle))
          case Pugt => Comparator(ComparatorParams(getInWidth, CmpUgt))
          case Puge => Comparator(ComparatorParams(getInWidth, CmpUge))
        }
      }

      case other => {
        def handleDirection(tList: List[MLIRType]): List[(Int, Int)] = {
          tList
            .map {
              case MLIRHs      => 0
              case MLIRInt(w)  => w
              case MLIRUInt(w) => w
              case MLIRMemref(shape, t) => {
                assert(shape.size == 1)
                Util.log2ceil(shape.head).toInt
              }
              case MLIRControl              => 0
              case MLIRChannel(MLIRInt(w))  => w
              case MLIRChannel(MLIRUInt(w)) => w
              case other                    => scala.sys.error("Unexpected type: " + other + "; " + this)
            }
            .toList.map(i => (i, 1))
        }

        println(other + " -> Operation not supported, treating as a blackbox.")

        val ins = handleDirection(genOp.t.ins)
        val outs = handleDirection(genOp.t.outs)

        BlackBox(BlackBoxParams(ins, outs, genOp.name.str().replace("\"", ""), false))
      }
    }
  }
}
