package codes.quine.labo.graph

import scala.collection.mutable

import syntax._

/**
  * AdjacencyMap is base class of graph represented by adjacency map.
  *
  * An adjacency map is data structure that represents a graph with edge labels.
  * If it has no label (or all labels share the same value), it is called adjacency list simply.
  *
  * @see [[https://en.wikipedia.org/wiki/Adjacency_list]]
  */
sealed abstract class AdjacencyMap[V, L] {
  def map: Map[V, Map[V, L]]

  def vertices: Set[V] = map.keySet

  def edges: Seq[Edge[V, L]]

  def label(source: V, target: V): Option[L] =
    map.get(source).flatMap(_.get(target))

  def out(source: V): Seq[(V, L)] = map.get(source).toSeq.flatten

  override def toString(): String = {
    val name = this match {
      case _: DirectedAdjacencyMap[_, _]   => "DirectedAdjacencyMap"
      case _: UndirectedAdjacencyMap[_, _] => "UndirectedAdjacencyMap"
    }
    val orphans = map.filter(_._2.isEmpty).map(x => s"Vertex(${x._1})")
    s"$name(${(orphans ++ edges.map(_.toString)).mkString(", ")})"
  }

  override def hashCode(): Int = map.##
}

final class DirectedAdjacencyMap[V, L] private[graph] (val map: Map[V, Map[V, L]]) extends AdjacencyMap[V, L] {
  def edges: Seq[DirectedEdge[V, L]] =
    map.flatMap { case (s, ts) =>
      ts.map { case (t, l) => s ~| l |> t }
    }.toSeq

  override def equals(obj: Any): Boolean =
    obj match {
      case obj: DirectedAdjacencyMap[_, _] =>
        map == obj.map
      case _ => false
    }
}

object DirectedAdjacencyMap {
  def apply[V, L](params: DirectedParam[V, L]*): DirectedAdjacencyMap[V, L] =
    from(params)

  def from[V, L](params: Seq[DirectedParam[V, L]]): DirectedAdjacencyMap[V, L] = {
    val map = mutable.Map.empty[V, mutable.Map[V, L]]
    params.foreach {
      case Vertex(v) =>
        map.getOrElseUpdate(v, mutable.Map.empty)
      case v1 ~| l |> v2 =>
        map.getOrElseUpdate(v1, mutable.Map.empty).update(v2, l)
        map.getOrElseUpdate(v2, mutable.Map.empty) // prevent missing v2 in vertices method result.
      case v1 ~| l |~ v2 =>
        map.getOrElseUpdate(v1, mutable.Map.empty).update(v2, l)
        map.getOrElseUpdate(v2, mutable.Map.empty).update(v1, l)
    }
    new DirectedAdjacencyMap(map.map { case (v, ts) => (v, ts.toMap) }.toMap)
  }

  implicit def DirectedGraphInstance[V, L]: DirectedGraph.Aux[DirectedAdjacencyMap[V, L], V, L] =
    new DirectedGraph[DirectedAdjacencyMap[V, L]] {
      type Vertex = V
      type Label = L
      def vertices(g: DirectedAdjacencyMap[V, L]): Set[V] = g.vertices
      def edges(g: DirectedAdjacencyMap[V, L]): Seq[DirectedEdge[V, L]] = g.edges
      def label(g: DirectedAdjacencyMap[V, L], source: V, target: V): Option[L] = g.label(source, target)
      override def out(g: DirectedAdjacencyMap[V, L], source: V): Seq[(V, L)] = g.out(source)
    }
}

final class UndirectedAdjacencyMap[V, L] private[graph] (val map: Map[V, Map[V, L]]) extends AdjacencyMap[V, L] {
  def edges: Seq[UndirectedEdge[V, L]] = {
    val edges = Seq.newBuilder[UndirectedEdge[V, L]]
    val processed = mutable.Set.empty[V]
    for (v1 <- vertices) {
      for ((v2, l) <- map(v1)) {
        if (!processed.contains(v2)) {
          edges.addOne(v1 ~| l |~ v2)
        }
      }
      processed.add(v1)
    }
    edges.result()
  }

  def in(target: V): Seq[(V, L)] = out(target)

  override def equals(obj: Any): Boolean =
    obj match {
      case obj: UndirectedAdjacencyMap[_, _] =>
        map == obj.map
      case _ => false
    }
}

object UndirectedAdjacencyMap {
  def apply[V, L](params: UndirectedParam[V, L]*): UndirectedAdjacencyMap[V, L] =
    from(params)

  def from[V, L](params: Seq[UndirectedParam[V, L]]): UndirectedAdjacencyMap[V, L] = {
    val map = mutable.Map.empty[V, mutable.Map[V, L]]
    params.foreach {
      case Vertex(v) =>
        map.getOrElseUpdate(v, mutable.Map.empty)
      case v1 ~| l |~ v2 =>
        map.getOrElseUpdate(v1, mutable.Map.empty).update(v2, l)
        map.getOrElseUpdate(v2, mutable.Map.empty).update(v1, l)
    }
    new UndirectedAdjacencyMap(map.map { case (v, ts) => (v, ts.toMap) }.toMap)
  }

  implicit def UndirectedGraphInstance[V, L]: UndirectedGraph.Aux[UndirectedAdjacencyMap[V, L], V, L] =
    new UndirectedGraph[UndirectedAdjacencyMap[V, L]] {
      type Vertex = V
      type Label = L
      def vertices(g: UndirectedAdjacencyMap[V, L]): Set[V] = g.vertices
      def edges(g: UndirectedAdjacencyMap[V, L]): Seq[UndirectedEdge[V, L]] = g.edges
      def label(g: UndirectedAdjacencyMap[V, L], source: V, target: V): Option[L] = g.label(source, target)
      override def in(g: UndirectedAdjacencyMap[V, L], target: V): Seq[(V, L)] = g.in(target)
      override def out(g: UndirectedAdjacencyMap[V, L], source: V): Seq[(V, L)] = g.out(source)
    }
}
