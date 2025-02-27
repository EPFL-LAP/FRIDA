package readers

trait GenericEdge[NodeType] {
  def src: NodeType
  def dst: NodeType
}

trait GenericGraph[NodeType, NodeKeyType, EdgeType <: GenericEdge[NodeType]] {
  def nodes: Map[NodeKeyType, NodeType]
  def edges: Set[EdgeType]

  def getOutgoingEdgesOf(srcNode: NodeType): Set[EdgeType] = {
    edges.filter(_.src.equals(srcNode))
  }

  def getIncomingEdgesOf(dstNode: NodeType): Set[EdgeType] = {
    edges.filter(_.dst.equals(dstNode))
  }
}
