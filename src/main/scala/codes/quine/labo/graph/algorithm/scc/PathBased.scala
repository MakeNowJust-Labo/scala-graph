package codes.quine.labo.graph
package algorithm
package scc

import scala.collection.mutable

/**
  * Implementation of path-based SCC algorithm.
  *
  * @see [[https://en.wikipedia.org/wiki/Path-based_strong_component_algorithm]]
  */
object PathBased {
  def apply[G, V, L](g: G)(implicit G: DirectedGraph.Aux[G, V, L]): Seq[Set[V]] = {
    var clock = 0
    val visited = mutable.Map.empty[V, Int]

    val stack = mutable.Stack.empty[V]
    val inStack = mutable.Set.empty[V]
    val path = mutable.Stack.empty[V]

    val components = Seq.newBuilder[Set[V]]

    def dfs(v1: V): Unit = {
      visited(v1) = clock
      clock += 1

      stack.push(v1)
      inStack.add(v1)
      path.push(v1)

      for ((v2, _) <- G.out(g, v1)) {
        if (!visited.contains(v2)) {
          dfs(v2)
        } else if (inStack.contains(v2)) {
          while (visited(path.top) > visited(v2)) {
            path.pop()
          }
        }
      }

      if (path.top == v1) {
        val component = Set.newBuilder[V]
        var v2 = stack.pop()
        inStack.remove(v2)
        while (v1 != v2) {
          component.addOne(v2)
          v2 = stack.pop()
          inStack.remove(v2)
        }
        component.addOne(v2)
        components.addOne(component.result())
        path.pop()
      }
    }

    for (v <- G.vertices(g)) {
      if (!visited.contains(v)) {
        dfs(v)
      }
    }

    components.result()
  }
}
