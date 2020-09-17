package codes.quine.labo.graph
package algorithm
package mst

import scala.collection.mutable

import syntax._
import util.UnionFind

/**
  * Implementation of Kruskal's algorithm.
  *
  * @see [[https://en.wikipedia.org/wiki/Kruskal's_algorithm]]
  */
object Kruskal {
  def apply[G, V, L](g: G)(implicit G: UndirectedGraph.Aux[G, V, L], L: Ordering[L]): Seq[UndirectedEdge[V, L]] = {
    implicit val OrderingInstanceForUndirectedEdge = Ordering.by[UndirectedEdge[V, L], L](_.label)

    val vs = G.vertices(g)
    val queue = mutable.Queue.from(G.edges(g).sorted)
    val tree = Seq.newBuilder[UndirectedEdge[V, L]]
    var treeSize = 0
    val uf = UnionFind[V](mutable.Map.empty)

    while (treeSize + 1 < vs.size) {
      if (queue.isEmpty) {
        throw new IllegalArgumentException("graph is disconnected")
      }

      val v1 ~| l |~ v2 = queue.dequeue()
      if (uf.find(v1) != uf.find(v2)) {
        treeSize += 1
        tree.addOne(v1 ~| l |~ v2)
        uf.union(v1, v2)
      }
    }

    tree.result()
  }
}
