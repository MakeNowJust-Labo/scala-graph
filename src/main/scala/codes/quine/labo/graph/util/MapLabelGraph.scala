package codes.quine.labo.graph
package util

import syntax._

/** MapLabelGraph is wrapper of graph that labels of its edges are mapped by the given function.
  *
  * This wrapper is used by some algorithms (e.g. Johnson's algorithm).
  *
  * @param g a graph is wrapped
  * @param f label mapping function
  * @param G a [[Graph]] instance
  */
final case class MapLabelGraph[G, V, L](g: G, f: (V, V, L) => L, G: Graph.Aux[G, V, L]) {
  def vertices: Set[V] = G.vertices(g)

  def edges: Seq[Edge[V, L]] =
    G.edges(g).flatMap {
      case v1 ~| l |~ v2 =>
        Seq(v1 ~| f(v1, v2, l) |> v2, v2 ~| f(v2, v1, l) |> v1)
      case v1 ~| l |> v2 =>
        Seq(v1 ~| f(v1, v2, l) |> v2)
    }

  def label(v1: V, v2: V): Option[L] =
    G.label(g, v1, v2).map(f(v1, v2, _))

  def in(v2: V): Seq[(V, L)] =
    G.in(g, v2).map { case (v1, l) => (v2, f(v1, v2, l)) }

  def out(v1: V): Seq[(V, L)] =
    G.out(g, v1).map { case (v2, l) => (v2, f(v1, v2, l)) }
}

object MapLabelGraph {
  implicit def GraphInstance[G, V, L]: Graph.Aux[MapLabelGraph[G, V, L], V, L] =
    new Graph[MapLabelGraph[G, V, L]] {
      type Vertex = V
      type Label = L
      def vertices(g: MapLabelGraph[G, V, L]): Set[V] = g.vertices
      def edges(g: MapLabelGraph[G, V, L]): Seq[Edge[V, L]] = g.edges
      def label(g: MapLabelGraph[G, V, L], source: V, target: V): Option[L] = g.label(source, target)
      override def in(g: MapLabelGraph[G, V, L], target: V): Seq[(V, L)] = g.in(target)
      override def out(g: MapLabelGraph[G, V, L], source: V): Seq[(V, L)] = g.out(source)
    }
}
