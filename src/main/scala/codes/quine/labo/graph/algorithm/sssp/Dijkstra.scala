package codes.quine.labo.graph
package algorithm
package sssp

import scala.collection.mutable

/**
  * Implementation of Dijkstra's algorithm.
  *
  * @see [[https://en.wikipedia.org/wiki/Dijkstra's_algorithm]]
  */
object Dijkstra {
  def apply[G, V, L](g: G, start: V)(implicit
      G: Graph.Aux[G, V, L],
      L: Numeric[L]
  ): (Map[V, L], Map[V, Option[V]]) = {
    implicit val OrderingInstanceForQueueState: Ordering[(V, L)] = Ordering.by(_._2)

    val dist = mutable.Map.empty[V, L]
    val prev = mutable.Map.empty[V, Option[V]]
    dist(start) = L.zero
    prev(start) = None

    val queue = mutable.PriorityQueue.empty[(V, L)](OrderingInstanceForQueueState.reverse)
    queue.enqueue((start, L.zero))

    while (queue.nonEmpty) {
      val (v1, w1) = queue.dequeue()
      if (L.equiv(dist(v1), w1)) {
        for ((v2, l) <- G.out(g, v1)) {
          if (L.lt(l, L.zero)) {
            throw new IllegalArgumentException("graph has negative weight edge")
          }

          val w2 = L.plus(w1, l)
          if (dist.get(v2).forall(L.lt(w2, _))) {
            dist(v2) = w2
            prev(v2) = Some(v1)
            queue.addOne((v2, w2))
          }
        }
      }
    }

    (dist.toMap, prev.toMap)
  }
}
