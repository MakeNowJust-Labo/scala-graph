package codes.quine.labo.graph
package algorithm
package scc

import scala.collection.mutable

/** Implementation of Kosaraju's algorithm.
  *
  * @see [[https://en.wikipedia.org/wiki/Kosaraju's_algorithm]]
  */
object Kosaraju {
  def apply[G, V, L](g: G)(implicit G: DirectedGraph.Aux[G, V, L]): Seq[Set[V]] = {
    val stack = mutable.Stack.empty[V]
    val visited = mutable.Set.empty[V]

    for (v <- G.vertices(g)) {
      reverseDFS(g, v, stack, visited)
    }

    var components = Seq.newBuilder[Set[V]]

    visited.clear()
    while (stack.nonEmpty) {
      val v = stack.pop()
      if (!visited.contains(v)) {
        val component = Set.newBuilder[V]
        ordinalDFS(g, v, component, visited)
        components.addOne(component.result())
      }
    }

    components.result()
  }

  private[this] def reverseDFS[G, V, L](g: G, v1: V, stack: mutable.Stack[V], visited: mutable.Set[V])(implicit
      G: Graph.Aux[G, V, L]
  ): Unit = {
    if (visited.contains(v1)) {
      return
    }
    visited.add(v1)

    for ((v0, _) <- G.in(g, v1)) {
      reverseDFS(g, v0, stack, visited)
    }
    stack.push(v1)
  }

  private[this] def ordinalDFS[G, V, L](g: G, v1: V, component: mutable.Builder[V, Set[V]], visited: mutable.Set[V])(
      implicit G: Graph.Aux[G, V, L]
  ): Unit = {
    if (visited.contains(v1)) {
      return
    }
    visited.add(v1)
    component.addOne(v1)

    for ((v2, _) <- G.out(g, v1)) {
      ordinalDFS(g, v2, component, visited)
    }
  }
}
