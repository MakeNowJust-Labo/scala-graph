package codes.quine.labo.graph
package util

import syntax._

/** ReachableGraph is wrapper of a graph with a new vertex which is reachable to any other vertices.
  *
  * This wrapper is used by some algorithms (e.g. Johnson's algorithm).
  *
  * @param g a graph is wrapped
  * @param G a [[Graph]] instance
  * @param L a [[Numeric]] instance
  */
final case class ReachableGraph[G, V, L](g: G, G: Graph.Aux[G, V, L], L: Numeric[L]) {
  def vertices: Set[Option[V]] =
    Set(None) ++ G.vertices(g).map(Some(_))

  def edges: Seq[Edge[Option[V], L]] =
    G.vertices(g).toSeq.map(v => None ~| L.zero |> Some(v)) ++
      G.edges(g).map {
        case v1 ~| l |~ v2 => Some(v1) ~| l |~ Some(v2)
        case v1 ~| l |> v2 => Some(v1) ~| l |> Some(v2)
      }

  def label(v1: Option[V], v2: Option[V]): Option[L] =
    (v1, v2) match {
      case (Some(v1), Some(v2)) => G.label(g, v1, v2)
      case (None, Some(v2))     => Some(L.zero)
      case _                    => None
    }

  def out(source: Option[V]): Seq[(Option[V], L)] =
    source match {
      case Some(s) => G.out(g, s).map { case (t, l) => (Some(t), l) }
      case None    => G.vertices(g).toSeq.map(t => (Some(t), L.zero))
    }
}

object ReachableGraph {
  implicit def GraphInstance[G, V, L]: Graph.Aux[ReachableGraph[G, V, L], Option[V], L] =
    new Graph[ReachableGraph[G, V, L]] {
      type Vertex = Option[V]
      type Label = L
      def vertices(g: ReachableGraph[G, V, L]): Set[Option[V]] = g.vertices
      def edges(g: ReachableGraph[G, V, L]): Seq[Edge[Option[V], L]] = g.edges
      def label(g: ReachableGraph[G, V, L], source: Option[V], target: Option[V]): Option[L] = g.label(source, target)
      override def out(g: ReachableGraph[G, V, L], source: Option[V]): Seq[(Option[V], L)] = g.out(source)
    }
}
