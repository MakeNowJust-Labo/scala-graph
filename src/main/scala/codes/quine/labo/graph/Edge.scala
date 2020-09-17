package codes.quine.labo.graph

/** Edge represents labelled edge. */
sealed abstract class Edge[+V, +L] extends Product with Serializable

/** Param is base class of any parameter. */
sealed trait Param[+V, +L]

/** DirectedParam is parameter of undirected graph constructor. */
sealed trait DirectedParam[+V, +L] extends Param[V, L]

/** UndirectecParam is parameter of directed graph constructor. */
sealed trait UndirectedParam[+V, +L] extends Param[V, L]

/** VertexParam is an orphan vertex on graph constructor. */
final case class VertexParam[+V](vertex: V) extends DirectedParam[V, Nothing] with UndirectedParam[V, Nothing]

/** DirectedEdge represents directed labelled edge. */
final case class DirectedEdge[+V, +L](source: V, target: V, label: L) extends Edge[V, L] with DirectedParam[V, L] {
  override def toString(): String =
    s"$source ~| $label |> $target"
}

object DirectedEdge {
  implicit def OrderingInstance[V: Ordering, L]: Ordering[DirectedEdge[V, L]] =
    Ordering.by(edge => (edge.source, edge.target))
}

/** UndirectedEdge represents undirected labelled edge. */
final case class UndirectedEdge[+V, +L](vertex1: V, vertex2: V, label: L)
    extends Edge[V, L]
    with DirectedParam[V, L]
    with UndirectedParam[V, L] {
  override def toString(): String =
    s"$vertex1 ~| $label |~ $vertex2"
}

object UndirectedEdge {
  implicit def OrderingInstance[V: Ordering, L]: Ordering[UndirectedEdge[V, L]] =
    Ordering.by(edge => (edge.vertex1, edge.vertex2))
}
