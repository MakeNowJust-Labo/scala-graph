package codes.quine.labo.graph
package algorithm
package mst

import scala.collection.mutable

import syntax._

/** Implementation of Prim's algorithm.
  *
  * @see [[https://en.wikipedia.org/wiki/Prim's_algorithm]]
  */
object Prim {
  def apply[G, V, L](g: G)(implicit G: UndirectedGraph.Aux[G, V, L], L: Ordering[L]): Seq[UndirectedEdge[V, L]] = {
    implicit val OrderingInstanceForUndirectedEdge = Ordering.by[UndirectedEdge[V, L], L](_.label)

    val vs = G.vertices(g)
    val tree = Seq.newBuilder[UndirectedEdge[V, L]]
    val inTree = mutable.Set.empty[V]
    val queue = mutable.PriorityQueue.empty[UndirectedEdge[V, L]](OrderingInstanceForUndirectedEdge.reverse)

    for (v1 <- vs.headOption) {
      inTree.add(v1)
      for ((v2, l) <- G.out(g, v1)) {
        queue.enqueue(v1 ~| l |~ v2)
      }
    }

    while (inTree.size < vs.size) {
      if (queue.isEmpty) {
        throw new IllegalArgumentException("graph is disconnected")
      }

      val v1 ~| l1 |~ v2 = queue.dequeue()
      if (!inTree.contains(v2)) {
        inTree.add(v2)
        tree.addOne(v1 ~| l1 |~ v2)
        for ((v3, l2) <- G.out(g, v2)) {
          if (v3 != v1) {
            queue.enqueue(v2 ~| l2 |~ v3)
          }
        }
      }
    }

    tree.result()
  }
}
