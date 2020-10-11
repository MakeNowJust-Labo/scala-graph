package codes.quine.labo.graph

import scala.collection.mutable

import syntax._

/** MutualAdjacencyMap is bi-directional traversible variant of DirectedAdjacencyMap.
  *
  * It holds not only out-going adjacency vertices, but also in-coming adjacency vertices.
  * So, it can execute [[Graph#in]] method efficiently.
  *
  * @see [[https://en.wikipedia.org/wiki/Adjacency_list]]
  */
final class MutualAdjacencyMap[V, L] private[graph] (val map: Map[V, Map[Either[V, V], L]]) {
  def vertices: Set[V] = map.keySet

  def edges: Seq[DirectedEdge[V, L]] =
    (for {
      (s, ts) <- map
      (Right(t), l) <- ts
    } yield s ~| l |> t).toSeq

  def label(source: V, target: V): Option[L] =
    map.get(source).flatMap(_.get(Right(target)))

  def in(target: V): Seq[(V, L)] =
    map
      .get(target)
      .iterator
      .flatten
      .flatMap {
        case (Left(source), l) => Some((source, l))
        case _                 => None
      }
      .toSeq

  def out(source: V): Seq[(V, L)] =
    map
      .get(source)
      .iterator
      .flatten
      .flatMap {
        case (Right(target), l) => Some((target, l))
        case _                  => None
      }
      .toSeq

  override def toString(): String = {
    val orphans = map.filter(_._2.isEmpty).map(x => s"Vertex(${x._1})")
    s"MutualAdjacencyMap(${(orphans ++ edges.map(_.toString)).mkString(", ")})"
  }
  override def equals(obj: Any): Boolean =
    obj match {
      case obj: MutualAdjacencyMap[_, _] => map == obj.map
      case _                             => false
    }

  override def hashCode(): Int = map.##
}

object MutualAdjacencyMap {
  def apply[V, L](params: DirectedParam[V, L]*): MutualAdjacencyMap[V, L] =
    from(params)

  def from[V, L](params: Seq[DirectedParam[V, L]]): MutualAdjacencyMap[V, L] = {
    val map = mutable.Map.empty[V, mutable.Map[Either[V, V], L]]
    params.foreach {
      case Vertex(v) =>
        map.getOrElseUpdate(v, mutable.Map.empty)
      case v1 ~| l |> v2 =>
        map.getOrElseUpdate(v1, mutable.Map.empty).update(Right(v2), l)
        map.getOrElseUpdate(v2, mutable.Map.empty).update(Left(v1), l)
      case v1 ~| l |~ v2 =>
        map.getOrElseUpdate(v1, mutable.Map.empty).update(Right(v2), l)
        map.getOrElseUpdate(v1, mutable.Map.empty).update(Left(v2), l)
        map.getOrElseUpdate(v2, mutable.Map.empty).update(Right(v1), l)
        map.getOrElseUpdate(v2, mutable.Map.empty).update(Left(v1), l)
    }
    new MutualAdjacencyMap(map.map { case (v, ts) => (v, ts.toMap) }.toMap)
  }

  implicit def DirectedGraphInstance[V, L]: DirectedGraph.Aux[MutualAdjacencyMap[V, L], V, L] =
    new DirectedGraph[MutualAdjacencyMap[V, L]] {
      type Vertex = V
      type Label = L
      def vertices(g: MutualAdjacencyMap[V, L]): Set[V] = g.vertices
      def edges(g: MutualAdjacencyMap[V, L]): Seq[DirectedEdge[V, L]] = g.edges
      def label(g: MutualAdjacencyMap[V, L], source: V, target: V): Option[L] = g.label(source, target)
      override def in(g: MutualAdjacencyMap[V, L], target: V): Seq[(V, L)] = g.in(target)
      override def out(g: MutualAdjacencyMap[V, L], source: V): Seq[(V, L)] = g.out(source)
    }
}
