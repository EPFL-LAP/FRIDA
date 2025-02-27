package printers

import arch._
import crkt._
import util._
import core._
import archs.Branch
import archs.TEHB
import archs.OEHB
import archs.Exit
import archs.Entry
import archs.Fork
import archs.EagerFork
import archs.LazyFork

import scala.collection.mutable
import io.AnsiColor._

object DotPrinter {
  val nodeTypeToColor = Map[String, String](
    "Merge" -> "darkseagreen1",
    "Fork" -> "lightskyblue",
    "EFork" -> "lightskyblue",
    "LFork" -> "lightskyblue3",
    "Mux" -> "lightsalmon",
    "Select" -> "lightsalmon",
    "Branch" -> "tomato",
    "CntrlMerge" -> "slateblue1",
    "Operator" -> "burlywood4",
    "Comparator" -> "burlywood4",
    "Mult" -> "burlywood4",
    "Div" -> "burlywood4",
    "Entry" -> "hotpink2",
    "Exit" -> "hotpink2",
    "Input" -> "hotpink2",
    "Output" -> "hotpink2",
    "Constant" -> "azure3",
    "CConstant" -> "azure3",
    "Source" -> "azure3",
    "Sink" -> "azure3",
    "Buffer" -> "darkolivegreen3",
    "OEHB" -> "lightgreen",
    "TEHB" -> "forestgreen",
    "EB" -> "forestgreen",
    "Extsi" -> "darkslategray4",
    "Extui" -> "darkslategray4",
    "Trunc" -> "darkslategray4",
    "End" -> "crimson",
    "Dummy" -> "khaki3"
  )

  def printNodeDecl(indent: String, node: TNode): String = {
    val c = if (node.attr.contains("bland")) {
      "grey47"
    } else {
      node.nType match {
        case Fork(p) => {
          p.variant match {
            case EagerFork => nodeTypeToColor.getOrElse("EFork", "white")
            case LazyFork  => nodeTypeToColor.getOrElse("LFork", "white")
            case other => scala.sys.error("Expected previse fork implementation.")
          }
        }

        case other => nodeTypeToColor.getOrElse(node.typeStr, "white")
      }
    }

    val shape = if (node.nType.isBuf) {
      "box"
    } else {
      "ellipse"
    }

    val labelSuffix = node.nType match {
      case TEHB(p) => "[" + p.depth + "]"
      case OEHB(p) => "[" + p.depth + "]"
      case other   => ""
    }

    val defaultAttrs = Map(
      ("fillcolor" -> ("\"" + c + "\"")),
      ("style" -> ("\"" + "filled" + "\"")),
      ("label" -> ("\"" + node.name + labelSuffix + "\"")),
      ("shape" -> ("\"" + "box" + "\"")),
      ("type" -> ("\"" + node.nType.typeString + "\"")),
      ("config" -> ("\"" + node.nType.libName + "\""))
    )

    val molAttr = if (node.attr.contains("mol")) {
      Some(("mol" -> ("\"" + node.attr("mol") + "\"")))
    } else {
      None
    }

    val slotsAttr = if (node.attr.contains("slots")) {
      Some(("slots" -> ("\"" + node.attr("slots") + "\"")))
    } else {
      None
    }

    val attrs = defaultAttrs ++ (molAttr :: slotsAttr :: Nil).flatten

    indent + "\"" + node.name + "\"" + " [" + attrs
      .map(
        (k, v) => (k + "=" + v)
      )
      .mkString(",") + " ];\n"
  }

  def printId(id: BlockPortID): String = {
    "" + id.dummy.str + id.pmw.pm.str + id.pmw.pb.str + id.pt.str + id.width
  }

  def printEdge(
      indent: String,
      g: ElasticGraph,
      p: Port,
      edgeAnnos: Set[(PortNodeID, PortNodeID)]
  ): String = {
    if (p.distPorts.isEmpty) {
      return ""
    }

    val dps = p.distPorts.map {
      (dpID, distPort: Port) =>
        {
          val source = if (p.pt == PTInput) distPort else p
          val dest = if (p.pt == PTInput) p else distPort

          val hsEdge = (!p.pmw.pb.isImpl()) && p.pmw.pb.isHandshake()
          val dummy = p.id.isDummy

          val edge = "\"" + source.thisNode + "\"" + " -> " + "\"" + dest.thisNode + "\""

          val color = if (hsEdge) {
            if (dummy) {
              "color" + " = \"" + "aquamarine3" + "\""
            } else {
              "color" + " = \"" + "darkorchid2" + "\""
            }
          } else {
            if (dummy) {
              "color" + " = \"" + "burlywood4" + "\""
            } else {
              if (p.width == 0) {
                "color" + " = \"" + "darkorchid2" + "\""
              } else if (p.width == 1) {
                "color" + " = \"" + "chartreuse3" + "\""
              } else {
                "color" + " = \"" + "black" + "\""
              }
            }
          }

          val from = "from=" + "\"" + printId(p.id) + "[" + p.loc + "]" + "\""
          val to = "to=" + "\"" + printId(distPort.id) + "[" + distPort.loc + "]" + "\""

          val edgeAnnosStr = if (edgeAnnos.contains(source.nodeID(), dest.nodeID())) {
            "style=\"dotted\", penwidth=3"
          } else {
            ""
          }

          val bufAttr = if (g.properties.contains(dest.nodeID())) {
            val bufConstr = g.properties(dest.nodeID()).toString()
            "label=" + "\"" + bufConstr + "\""
          } else {
            ""
          }

          val attrs = (color :: from :: to :: edgeAnnosStr :: bufAttr :: Nil).filter(_.nonEmpty)
          indent + edge + " [" + attrs.mkString(", ") + "];\n"
        }
    }

    dps.mkString("")
  }

  def apply(fname: String) = {
    (graph: ElasticGraph, edgesAnnos: Set[(PortNodeID, PortNodeID)]) =>
      {
        val nodesBuilder = new StringBuilder()
        val edgesBuilder = new StringBuilder()

        val indent = "  "

        if (graph.nodes.nonEmpty && (graph.nodes.map(_._2).forall(_.attr.contains("mol")))) {
          var clusterNum = 0

          val noMol = graph.nodes.map(_._2).filter(!_.attr.contains("mol"))
          // println(noMol.size)

          graph.nodes.map(_._2).groupBy(_.attr("mol")).map {
            (molName, nodes) =>
              {
                nodesBuilder.append(indent + "subgraph cluster_" + clusterNum + " {\n")
                clusterNum += 1

                nodes.foreach {
                  n =>
                    {
                      nodesBuilder.append(printNodeDecl(indent + "    ", n))
                    }
                }

                nodesBuilder.append(indent + "    label = \"" + molName + "\";\n")
                nodesBuilder.append(indent + "    color=black\n")
                nodesBuilder.append(indent + "}\n\n")
              }
          }
        } else {
          graph.nodes.map(_._2).map {
            n =>
              {
                nodesBuilder.append(printNodeDecl(indent, n))
              }
          }
        }

        graph.nodes.map(_._2).foreach {
          n =>
            {
              n.outPortsList().foreach {
                p =>
                  {
                    edgesBuilder.append(printEdge(indent, graph, p, edgesAnnos))
                  }
              }
            }
        }

        val writer = Util.writeOpen(fname)
        println(YELLOW + "Printing: " + fname + RESET)

        writer.write("Digraph G {\n")
        writer.write(nodesBuilder.toString())

        writer.write("\n\n")

        writer.write(edgesBuilder.toString())
        writer.write("}")

        writer.close()

        graph
      }
  }
}
