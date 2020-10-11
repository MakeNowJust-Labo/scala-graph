package codes.quine.labo.graph
package algorithm
package sssp

import scala.collection.mutable

/** Implementation of Bellman-Ford algorithm.
  *
  * @see [[https://en.wikipedia.org/wiki/Bellman–Ford_algorithm]]
  */
object BellmanFord {
  def apply[G, V, L](g: G, start: V)(implicit
      G: Graph.Aux[G, V, L],
      L: Numeric[L]
  ): (Map[V, L], Map[V, Option[V]]) = {
    val vs = G.vertices(g)

    val dist = mutable.Map.empty[V, L]
    val prev = mutable.Map.empty[V, Option[V]]
    dist(start) = L.zero
    prev(start) = None

    val queue = mutable.Queue.empty[Option[V]]
    val inQueue = mutable.Set.empty[V]

    queue.enqueue(Some(start))
    inQueue.add(start)

    var loopIndex = 0
    queue.enqueue(None)

    while (inQueue.nonEmpty) {
      queue.dequeue() match {
        case Some(v1) =>
          inQueue.remove(v1)
          val w1 = dist(v1)
          for ((v2, l) <- G.out(g, v1)) {
            val w2 = L.plus(w1, l)
            if (dist.get(v2).forall(L.lt(w2, _))) {
              dist(v2) = w2
              prev(v2) = Some(v1)
              if (!inQueue.contains(v2)) {
                queue.enqueue(Some(v2))
                inQueue.add(v2)
              }
            }
          }
        case None =>
          loopIndex += 1
          if (loopIndex == vs.size - 1 && queue.nonEmpty) {
            throw new IllegalArgumentException("graph has negative weight cycle")
          }
          queue.enqueue(None)
      }
    }

    (dist.toMap, prev.toMap)
  }
}
