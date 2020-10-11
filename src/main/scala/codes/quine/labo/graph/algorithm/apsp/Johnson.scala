package codes.quine.labo.graph
package algorithm
package apsp

import scala.collection.mutable

import util.{MapLabelGraph, ReachableGraph}

/** Implementation of Johnson's algorithm.
  *
  * @see [[https://en.wikipedia.org/wiki/Johnson's_algorithm]]
  */
object Johnson {
  def apply[G, V, L](
      g: G
  )(implicit G: Graph.Aux[G, V, L], L: Numeric[L]): (Map[(V, V), L], Map[(V, V), Option[V]]) = {
    val g1 = ReachableGraph(g, G, L)
    val (h, _) = algorithm.sssp.BellmanFord(g1, None: Option[V])

    val dist = mutable.Map.empty[(V, V), L]
    val prev = mutable.Map.empty[(V, V), Option[V]]

    val g2 = MapLabelGraph(g, (v1: V, v2: V, l: L) => L.minus(L.plus(h(Some(v1)), l), h(Some(v2))), G)

    for (v1 <- G.vertices(g)) {
      val (d, p) = algorithm.sssp.Dijkstra(g2, v1)
      for ((v2, l) <- d) {
        dist((v1, v2)) = L.plus(L.minus(l, h(Some(v1))), h(Some(v2)))
      }
      for ((v2, q) <- p) {
        prev((v1, v2)) = q
      }
    }

    (dist.toMap, prev.toMap)
  }
}
