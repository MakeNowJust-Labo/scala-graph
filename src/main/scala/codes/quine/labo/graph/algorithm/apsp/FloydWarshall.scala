package codes.quine.labo.graph
package algorithm
package apsp

import scala.collection.mutable

import syntax._

/** Implementation of Floyd-Warshall algorithm.
  *
  * Note that this algorithm may return invalid result when a graph has negative weight cycle.
  *
  * @see [[https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm]]
  */
object FloydWarshall {
  def apply[G, V, L](
      g: G
  )(implicit G: Graph.Aux[G, V, L], L: Numeric[L]): (Map[(V, V), L], Map[(V, V), Option[V]]) = {
    val vs = G.vertices(g)

    val dist = mutable.Map.empty[(V, V), L]
    val prev = mutable.Map.empty[(V, V), Option[V]]

    for (v <- vs) {
      dist((v, v)) = L.zero
      prev((v, v)) = None
    }

    G.edges(g).foreach {
      case v1 ~| l |> v2 =>
        if (dist.get((v1, v2)).forall(L.lt(l, _))) {
          dist((v1, v2)) = l
          prev((v1, v2)) = Some(v1)
        }
      case v1 ~| l |~ v2 =>
        if (dist.get((v1, v2)).forall(L.lt(l, _))) {
          dist((v1, v2)) = l
          prev((v1, v2)) = Some(v1)
        }
        if (dist.get((v2, v1)).forall(L.lt(l, _))) {
          dist((v2, v1)) = l
          prev((v2, v1)) = Some(v2)
        }
    }

    for (v2 <- vs) {
      for (v1 <- vs) {
        for (v3 <- vs) {
          val w13 = dist.get((v1, v3))
          val w123 = for {
            w12 <- dist.get((v1, v2))
            w23 <- dist.get((v2, v3))
          } yield L.plus(w12, w23)
          if (w123.isDefined && w13.forall(L.lt(w123.get, _))) {
            dist((v1, v3)) = w123.get
            prev((v1, v3)) = prev((v2, v3))
          }
        }
      }
    }

    (dist.toMap, prev.toMap)
  }
}
