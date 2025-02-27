package readers

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.JavaTokenParsers

class GenericDotGraph(
    val nodes: Map[String, GenericDotNode],
    val edges: Set[GenericDotEdge]
) extends GenericGraph[GenericDotNode, String, GenericDotEdge] {
  override def toString(): String = {
    ("digraph G {" :: nodes.values.toSeq.sortBy(_.name).map(_.toString()).mkString("\n") :: edges.toSeq
      .sortBy(_.src.name)
      .map(
        _.toString()
      )
      .mkString("\n") :: "}" :: Nil).mkString("\n")
  }
}

case class GenericDotNode(name: String, attr: Map[String, String]) {

  def withAttr(k: String, v: String): GenericDotNode = {
    GenericDotNode(name, attr + (k -> v))
  }

  override def toString(): String = {
    val attrs = attr
      .map(
        (k, v) => (k + "=\"" + v + "\"")
      )
      .mkString(", ")
    name + " [ " + attrs + " ];"
  }
}

case class GenericStrDotEdge(
    val src: String,
    val dst: String,
    val attrs: Map[String, String]
) extends GenericEdge[String] {
  override def toString(): String = {
    val strAttrs = attrs
      .map(
        (k, v) => (k + "=\"" + v + "\"")
      )
      .mkString(", ")
    src + " -> " + dst + " [ " + strAttrs + " ];"
  }
}

case class GenericDotEdge(
    val src: GenericDotNode,
    val dst: GenericDotNode,
    val attrs: Map[String, String]
) extends GenericEdge[GenericDotNode] {
  override def toString(): String =
    GenericStrDotEdge(src.name, dst.name, attrs).toString()
  def withProcessedAttr(
      f: Map[String, String] => Map[String, String]
  ): GenericDotEdge = {
    GenericDotEdge(src, dst, f(attrs))
  }
}

object DotSimpleGenericParser extends JavaTokenParsers {
  def flatten(reader: Source): String = {
    val f = reader
      .getLines()
      .toList
      .mkString("\n")
    f
  }

  def graph: Parser[GenericDotGraph] = {
    opt("strict") ~ ("graph" | "digraph" | "Graph" | "Digraph") ~ opt(
      dot_id
    ) ~ "{" ~ stmt_list ~ "}" ^^ {
      case str ~ grType ~ id ~ "{" ~ stmts ~ "}" => {
        // extract edges and Nodes
        val edgesStr: List[GenericStrDotEdge] = stmts.collect {
          case e: GenericStrDotEdge => e
        }
        val nodes: List[GenericDotNode] = stmts.collect {
          case n: GenericDotNode =>
            n
        }

        // println(nodes)
        // println(edgesStr)

        val nodesMap = nodes
          .map(
            n => (n.name, n)
          )
          .toMap
        val edges = edgesStr.map(
          e => GenericDotEdge(nodesMap(e.src), nodesMap(e.dst), e.attrs)
        )

        // println(edges)

        GenericDotGraph(nodesMap, edges.toSet)
      }

      case _ => throw new Exception("Ill formed expression")
    }
  }

  def stmt_list: Parser[List[(GenericDotNode | GenericStrDotEdge)]] = {
    rep(stmt <~ opt(";"))
  }

  def stmt: Parser[(GenericDotNode | GenericStrDotEdge)] = (
    edge_stmt
      | node_stmt
  )

  def attr_list: Parser[Map[String, String]] =
    "[" ~> repsep(attr_pair, ",") <~ opt(",") ~ "]" ^^ (Map() ++ _)

  def attr_pair: Parser[(String, String)] = dot_id ~ "=" ~ dot_id ^^ {
    case attrName ~ "=" ~ attrValue => (attrName, attrValue)
  }

  def node_stmt: Parser[GenericDotNode] = dot_id ~ opt(attr_list) ^^ {
    case nName ~ Some(l) => {
      GenericDotNode(nName, l)
    }

    case node1 ~ None =>
      scala.sys.error(node1 + " does not have any attributes")
  }

  def edge_stmt: Parser[GenericStrDotEdge] = {
    dot_id ~ "->" ~ dot_id ~ opt(attr_list) ^^ {
      case srcName ~ "->" ~ dstName ~ Some(l) => {
        GenericStrDotEdge(srcName, dstName, l)
      }

      case other => scala.sys.error("Unexpected String")
    }
  }

  def dot_id: Parser[String] = ident | stringLit | floatingPointNumber

  def stringLit: Parser[String] = stringLiteral ^^ {
    case s =>
      s.drop(1).dropRight(1)
  }

  def apply(reader: Source): GenericDotGraph = {
    val inputStr = flatten(reader)
    parseAll(this.graph, inputStr) match {
      case Success(result, _) => result
      case failure: NoSuccess => {
        println("Parsing failed at: " + failure.next.pos)
        scala.sys.error(failure.msg)
      }
    }
  }
}

trait toDotNodeConvertible {
  def toDotNode(): GenericDotNode
}

trait fromDotNodeConvertible[RegularType] {
  def apply(n: GenericDotNode): RegularType
}

trait toDotEdgeConvertible {
  def toDotEdge(): GenericDotEdge
}

trait fromDotEdgeConvertible[RegularType] {
  def apply(e: GenericDotEdge): RegularType
}
