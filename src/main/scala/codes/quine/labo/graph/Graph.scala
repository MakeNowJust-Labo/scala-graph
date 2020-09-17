package codes.quine.labo.graph

trait Graph[G] {
  type Vertex
  type Label

  def vertices(g: G): Set[Vertex]

  def edges(g: G): Seq[Edge[Vertex, Label]]

  def label(g: G, source: Vertex, target: Vertex): Option[Label]

  def in(g: G, target: Vertex): Seq[(Vertex, Label)] =
    vertices(g).iterator.flatMap { source => label(g, source, target).map((source, _)) }.toSeq

  def out(g: G, source: Vertex): Seq[(Vertex, Label)] =
    vertices(g).iterator.flatMap { target => label(g, source, target).map((target, _)) }.toSeq
}

object Graph {
  type Aux[G, V, L] = Graph[G] {
    type Vertex = V
    type Label = L
  }
}

trait DirectedGraph[G] extends Graph[G] {
  def edges(g: G): Seq[DirectedEdge[Vertex, Label]]
}

object DirectedGraph {
  type Aux[G, V, L] = DirectedGraph[G] {
    type Vertex = V
    type Label = L
  }

  @inline def apply[G](implicit G: DirectedGraph[G]): DirectedGraph[G] = G
}

trait UndirectedGraph[G] extends Graph[G] {
  def edges(g: G): Seq[UndirectedEdge[Vertex, Label]]
}

object UndirectedGraph {
  type Aux[G, V, L] = UndirectedGraph[G] {
    type Vertex = V
    type Label = L
  }

  @inline def apply[G](implicit G: UndirectedGraph[G]): UndirectedGraph[G] = G
}
