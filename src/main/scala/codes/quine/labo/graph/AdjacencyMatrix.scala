package codes.quine.labo.graph

import scala.collection.mutable

import syntax._

/**
  * AdjacencyMatrix is base class of graph represented by adjacency matrix.
  *
  * An adjacency matrix is data structure that represents a graph.
  *
  * @see [[https://en.wikipedia.org/wiki/Adjacency_matrix]]
  */
sealed abstract class AdjacencyMatrix[V, L] {
  def vertices: Set[V]

  def edges: Seq[Edge[V, L]]

  def label(source: V, target: V): Option[L]
}

object AdjacencyMatrix {
  private[graph] def buildMapping[V, L](params: Seq[Param[V, L]]): Map[V, Int] = {
    val mapping = mutable.Map.empty[V, Int]
    params.foreach {
      case Vertex(v) =>
        mapping.getOrElseUpdate(v, mapping.size)
      case v1 ~| _ |~ v2 =>
        mapping.getOrElseUpdate(v1, mapping.size)
        mapping.getOrElseUpdate(v2, mapping.size)
      case v1 ~| _ |> v2 =>
        mapping.getOrElseUpdate(v1, mapping.size)
        mapping.getOrElseUpdate(v2, mapping.size)
    }
    mapping.toMap
  }
}

final class DirectedAdjacencyMatrix[V, L] private[graph] (val mapping: Map[V, Int], val labels: Vector[Option[L]])
    extends AdjacencyMatrix[V, L] {
  def vertices: Set[V] = mapping.keySet

  def edges: Seq[DirectedEdge[V, L]] = {
    val edges = Seq.newBuilder[DirectedEdge[V, L]]
    for (v1 <- vertices) {
      val i1 = mapping(v1)
      for (v2 <- vertices) {
        val i2 = mapping(v2)
        for (l <- labels(i1 * mapping.size + i2)) {
          edges.addOne(v1 ~| l |> v2)
        }
      }
    }
    edges.result()
  }

  def label(source: V, target: V): Option[L] =
    (mapping.get(source), mapping.get(target)) match {
      case (Some(s), Some(t)) => labels(s * mapping.size + t)
      case _                  => None
    }

  override def toString(): String = {
    if (mapping.isEmpty) {
      return "DirectedAdjacencyMatrix(Seq(), Vector())"
    }

    val builder = new mutable.StringBuilder

    builder ++= "DirectedAdjacencyMatrix(\n"
    builder ++= "  " + mapping.toSeq.sortBy(_._2).map(_._1).mkString("Seq(", ", ", ")") + ",\n"

    val n = mapping.size
    builder ++= "  Vector(\n"
    for (i1 <- 0 until n) {
      builder ++= "    "
      for (i2 <- 0 until n) {
        builder ++= labels(i1 * n + i2).toString + ", "
      }
      builder ++= "\n"
    }
    builder ++= "  )\n"
    builder ++= ")"

    builder.result()
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case obj: DirectedAdjacencyMatrix[_, _] =>
        mapping == obj.mapping && labels == obj.labels
      case _ => false
    }

  override def hashCode(): Int = (mapping, labels).##
}

object DirectedAdjacencyMatrix {
  def apply[V, L](mapping: Seq[V], labels: Vector[Option[L]]): DirectedAdjacencyMatrix[V, L] = {
    if (mapping.size * mapping.size != labels.size) {
      throw new IllegalArgumentException("invalid matrix shape")
    }
    new DirectedAdjacencyMatrix(mapping.zipWithIndex.toMap, labels)
  }

  def apply[V, L](params: DirectedParam[V, L]*): DirectedAdjacencyMatrix[V, L] =
    from(params)

  def from[V, L](params: Seq[DirectedParam[V, L]]): DirectedAdjacencyMatrix[V, L] = {
    val mapping = AdjacencyMatrix.buildMapping(params)
    val n = mapping.size
    val labels = mutable.ArraySeq.fill[Option[L]](n * n)(None)
    params.foreach {
      case Vertex(_) =>
      // nothing to do
      case v1 ~| l |> v2 =>
        val i1 = mapping(v1)
        val i2 = mapping(v2)
        labels(i1 * n + i2) = Some(l)
      case v1 ~| l |~ v2 =>
        val i1 = mapping(v1)
        val i2 = mapping(v2)
        labels(i1 * n + i2) = Some(l)
        labels(i2 * n + i1) = Some(l)
    }
    new DirectedAdjacencyMatrix(mapping, labels.toVector)
  }

  implicit def DirectedGraphInstance[V, L]: DirectedGraph.Aux[DirectedAdjacencyMatrix[V, L], V, L] =
    new DirectedGraph[DirectedAdjacencyMatrix[V, L]] {
      type Vertex = V
      type Label = L
      def vertices(g: DirectedAdjacencyMatrix[V, L]): Set[V] = g.vertices
      def edges(g: DirectedAdjacencyMatrix[V, L]): Seq[DirectedEdge[V, L]] = g.edges
      def label(g: DirectedAdjacencyMatrix[V, L], source: V, target: V): Option[L] = g.label(source, target)
    }
}

final class UndirectedAdjacencyMatrix[V, L] private[graph] (val mapping: Map[V, Int], val labels: Vector[Option[L]])
    extends AdjacencyMatrix[V, L] {
  def vertices: Set[V] = mapping.keySet

  def edges: Seq[UndirectedEdge[V, L]] = {
    val edges = Seq.newBuilder[UndirectedEdge[V, L]]
    for (v1 <- vertices) {
      val i1 = mapping(v1)
      for (v2 <- vertices) {
        val i2 = mapping(v2)
        if (i1 >= i2) {
          for (l <- labels((i1 + 1) * i1 / 2 + i2)) {
            edges.addOne(v1 ~| l |~ v2)
          }
        }
      }
    }
    edges.result()
  }

  def label(v1: V, v2: V): Option[L] =
    (mapping.get(v1), mapping.get(v2)) match {
      case (Some(i1), Some(i2)) if i1 >= i2 => labels((i1 + 1) * i1 / 2 + i2)
      case (Some(i2), Some(i1))             => labels((i2 + 1) * i2 / 2 + i1)
      case _                                => None
    }

  override def toString(): String = {
    if (mapping.isEmpty) {
      return "UndrectedAdjacencyMatrix(Seq(), Vector())"
    }

    val builder = new mutable.StringBuilder

    builder ++= "UndirectedAdjacencyMatrix(\n"
    builder ++= "  " + mapping.toSeq.sortBy(_._2).map(_._1).mkString("Seq(", ", ", ")") + ",\n"

    val n = mapping.size
    builder ++= "  Vector(\n"
    for (i1 <- 0 until n) {
      builder ++= "    "
      for (i2 <- 0 to i1) {
        builder ++= labels((i1 + 1) * i1 / 2 + i2).toString + ", "
      }
      builder ++= "\n"
    }
    builder ++= "  )\n"
    builder ++= ")"

    builder.result()
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case obj: UndirectedAdjacencyMatrix[_, _] =>
        mapping == obj.mapping && labels == obj.labels
      case _ => false
    }

  override def hashCode(): Int = (mapping, labels).##
}

object UndirectedAdjacencyMatrix {
  def apply[V, L](mapping: Seq[V], labels: Vector[Option[L]]): UndirectedAdjacencyMatrix[V, L] = {
    if ((mapping.size + 1) * mapping.size / 2 != labels.size) {
      throw new IllegalArgumentException("invalid matrix shape")
    }
    new UndirectedAdjacencyMatrix(mapping.zipWithIndex.toMap, labels)
  }

  def apply[V, L](params: UndirectedParam[V, L]*): UndirectedAdjacencyMatrix[V, L] =
    from(params)

  def from[V, L](params: Seq[UndirectedParam[V, L]]): UndirectedAdjacencyMatrix[V, L] = {
    val mapping = AdjacencyMatrix.buildMapping(params)
    val n = mapping.size
    val labels = mutable.ArraySeq.fill[Option[L]]((n + 1) * n / 2)(None)
    params.foreach {
      case Vertex(_) =>
      // nothing to do
      case v1 ~| l |~ v2 =>
        val i1 = mapping(v1)
        val i2 = mapping(v2)
        if (i1 >= i2) {
          labels((i1 + 1) * i1 / 2 + i2) = Some(l)
        } else {
          labels((i2 + 1) * i2 / 2 + i1) = Some(l)
        }
    }
    new UndirectedAdjacencyMatrix(mapping, labels.toVector)
  }

  implicit def UndirectedGraphInstance[V, L]: UndirectedGraph.Aux[UndirectedAdjacencyMatrix[V, L], V, L] =
    new UndirectedGraph[UndirectedAdjacencyMatrix[V, L]] {
      type Vertex = V
      type Label = L
      def vertices(g: UndirectedAdjacencyMatrix[V, L]): Set[V] = g.vertices
      def edges(g: UndirectedAdjacencyMatrix[V, L]): Seq[UndirectedEdge[V, L]] = g.edges
      def label(g: UndirectedAdjacencyMatrix[V, L], source: V, target: V): Option[L] = g.label(source, target)
    }
}
