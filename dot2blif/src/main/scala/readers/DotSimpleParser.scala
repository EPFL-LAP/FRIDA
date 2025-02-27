package readers

import crkt._
import arch._
import archs.Primitive
import archs.Params

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.collection.mutable.HashMap
import scala.util.Failure
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import archs.EB
import archs.TEHB
import archs.OEHB

import math.min
import collection.mutable.{Map => MMap}

case class DotEdge(src: PortNodeID, dst: PortNodeID)

object BlockPortIDParser extends RegexParsers {
  def portMeaning: Parser[PortMeaning] = (
    (PMData(None).str ^^^ PMData(None))
      | (PMCond(None).str ^^^ PMCond(None))
      | (PMAddr(None).str ^^^ PMAddr(None))
  )

  def portBundle: Parser[PortBundle] = {
    (Impl.str ^^^ Impl)
      | (D.str ^^^ D)
      | (Hs.str ^^^ Hs)
  }

  def portType: Parser[PortType] = (
    (PTInput.str ^^^ PTInput)
      | (PTOutput.str ^^^ PTOutput)
      | (PTUndef.str ^^^ PTUndef)
  )

  def hsType: Parser[HSType] = (
    (HSValid.str ^^^ HSValid)
      | (HSReady.str ^^^ HSReady)
  )

  def dummy: Parser[DummyType] = (
    (Dummy.str ^^^ Dummy)
      | (Regular.str ^^^ Regular)
  )

  def number: Parser[String] = """\d+""".r

  def portId: Parser[(BlockPortID, Int)] = {
    dummy ~ portMeaning ~ portBundle ~ portType ~ number ~ "[" ~ number ~ "]" ^^ {
      case dummy ~ pm ~ pb ~ pt ~ width ~ "[" ~ word ~ "]" => {
        val pmw = PortMeaningWrapper(pm, pb)
        val id = BlockPortID(width.toInt, pt, pmw, dummy)

        (id, word.toInt)
      }

      case other => scala.sys.error("Unexpected string")
    }
  }

  def apply(input: String): (BlockPortID, Int) = parseAll(portId, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

object DotSimpleParser extends JavaTokenParsers {
  def flatten(reader: Source): String = {
    val f = reader
      .getLines()
      .toList
      .filter {
        (line: String) =>
          {
            !line.contains("//")
            && !line.replaceAll("\\s", "").startsWith("color=")
            && !line.replaceAll("\\s", "").trim().startsWith("label=")
            && !line.contains("splines")
            && !line.replaceAll("\\s", "").startsWith("node")
            && !line.replaceAll("\\s", "").startsWith("compound=true")
          }
      }
      .foldLeft((0, ArrayBuffer[String]())) {
        (acc: (Int, ArrayBuffer[String]), line: String) =>
          {
            val (isInSub, accL) = (acc._1, acc._2)

            if (line.contains("subgraph")) {
              ((isInSub + 1), accL)
            } else if (line.contains("}")) {
              if (isInSub == 0) {
                (isInSub, accL.append(line))
              } else {
                (isInSub - 1, accL)
              }
            } else {
              (isInSub, accL.append(line))
            }
          }
      }
      ._2
      .mkString("\n")
    f
  }

  def graph: Parser[ElasticGraph] = {
    opt("strict") ~ ("Graph" | "Digraph") ~ opt(dot_id) ~ "{" ~ stmt_list ~ "}" ^^ {
      case str ~ grType ~ id ~ "{" ~ stmts ~ "}" => {
        // extract edges and Nodes
        val edges: List[DotEdge] = stmts.collect {
          case e: DotEdge => e
        }
        val nodes: List[TNode] = stmts.collect {
          case n: TNode => n
        }

        val portsGrouped = edges
          .map(
            dotEdge => (dotEdge.src :: dotEdge.dst :: Nil)
          )
          .flatten
          .groupBy(_.nodeName)

        val portMap = MMap[PortNodeID, Port]()

        val fullNodes = nodes.map {
          n =>
            {
              val ports = portsGrouped(n.name)
                .map {
                  pnId =>
                    {
                      val nP = Port(pnId.pId, pnId.pId.toString(), Map(), n.name, Map(), pnId.loc)
                      val nPId = nP.nodeID()

                      if (portMap.contains(nPId)) {
                        None
                      } else {
                        portMap += (nP.nodeID() -> nP)
                        Some(nP)
                      }
                    }
                }
                .flatten
                .groupBy(_.id)

              Node(n.name, ???, ???, n.attr, n.annos, ports)
            }
        }

        val nodesMap = fullNodes
          .map(
            n => (n.name, n)
          )
          .toMap

        val sourceLinks = edges.groupBy(_._1)
        val sinkLinks = edges.groupBy(_._2)

        sourceLinks.foreach {
          (pId, edges) =>
            {
              val targetPorts = edges.map(_._2).map(portMap(_))
              val p = nodesMap(pId.nodeName).ports(pId.pId).filter(_.loc == pId.loc).head

              p.distPorts = (p.distPorts ++ targetPorts.map(
                p => (p.nodeID(), p)
              )).toMap
            }
        }

        sinkLinks.foreach {
          (pId, edges) =>
            {
              val targetPorts = edges.map(_._1).map(portMap(_))
              val p = nodesMap(pId.nodeName).ports(pId.pId).filter(_.loc == pId.loc).head

              p.distPorts = (p.distPorts ++ targetPorts.map(
                p => (p.nodeID(), p)
              )).toMap
            }
        }

        fullNodes.foreach {
          n =>
            {
              n.ports.foreach {
                (pid, ps) =>
                  {
                    ps.foreach {
                      p =>
                        {
                          assert(p.distPorts.nonEmpty, p)
                        }
                    }
                  }
              }
            }
        }

        ElasticGraph(nodesMap)
      }

      case _ => throw new Exception("Ill formed expression")
    }
  }

  def stmt_list: Parser[List[(TNode | DotEdge)]] = opt(stmt ~ opt(";") ~ stmt_list) ^^ {
    case Some(graphElem1 ~ sep ~ graphElemList) => graphElem1 +: graphElemList
    case _                                      => List()
  }

  def stmt: Parser[(TNode | DotEdge)] = (
    edge_stmt
      | node_stmt
  )

  def attr_list: Parser[Map[String, String]] = "[" ~> repsep(attr_pair, ",") <~ opt(",") ~ "]" ^^ (Map() ++ _)

  def attr_pair: Parser[(String, String)] = dot_id ~ "=" ~ dot_id ^^ {
    case attrName ~ "=" ~ attrValue => (attrName, attrValue)
  }

  def node_stmt: Parser[TNode] = dot_id ~ opt(attr_list) ^^ {
    case nName ~ Some(l) => {
      // val nType = typeNameToType(l("type"))

      val molAttr = if (l.contains("mol")) {
        Some(("mol" -> l("mol")))
      } else {
        None
      }

      val slotsAttr = if (l.contains("slots")) {
        Some(("slots" -> l("slots")))
      } else {
        None
      }

      val attrs = (molAttr :: slotsAttr :: Nil).flatten.toMap

      Node(nName, ???, ???, attrs, Set(), Map())
    }

    case node1 ~ None => scala.sys.error(node1 + " does not have any attributes")
  }

  def edge_stmt: Parser[DotEdge] = {
    dot_id ~ "->" ~ dot_id ~ opt(attr_list) ^^ {
      case srcName ~ "->" ~ dstName ~ Some(l) => {
        val (srcId, srcWord) = BlockPortIDParser(l("from"))
        val (dstId, dstWord) = BlockPortIDParser(l("to"))

        val src = PortNodeID(srcName, srcId, srcWord)
        val dst = PortNodeID(dstName, dstId, dstWord)

        DotEdge(src, dst)
      }

      case other => scala.sys.error("Unexpected String")
    }
  }

  def dot_id: Parser[String] = ident | stringLit

  def stringLit: Parser[String] = stringLiteral ^^ {
    case s => s.drop(1).dropRight(1)
  }

  def apply(reader: Source): ElasticGraph = {
    parseAll(this.graph, flatten(reader)) match {
      case Success(result, _) => result
      case failure: NoSuccess => {
        println("Parsing failed at: " + failure.next.pos)
        scala.sys.error(failure.msg)
      }
    }
  }
}
