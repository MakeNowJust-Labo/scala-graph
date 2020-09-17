package codes.quine.labo.graph

object syntax {
  val Vertex = VertexParam

  implicit final class EdgeOps[V](private val source: V) extends AnyVal {
    def ~|[L](label: L): EdgeBuilder[V, L] = EdgeBuilder(source, label)
    def ~>[W >: V](target: W): DirectedEdge[W, Unit] = DirectedEdge(source, target, ())
    def ~~[W >: V](target: W): UndirectedEdge[W, Unit] = UndirectedEdge(source, target, ())
  }

  final case class EdgeBuilder[V, L](source: V, label: L) {
    def |>[W >: V](target: W): DirectedEdge[W, L] = DirectedEdge(source, target, label)
    def |~[W >: V](target: W): UndirectedEdge[W, L] = UndirectedEdge(source, target, label)
  }

  object ~| {
    def unapply[V, L](tmp: (V, L)): Option[(V, L)] = Some(tmp)
  }

  object ~> {
    def unapply[V, L](edge: DirectedEdge[V, L]): Option[(V, V)] = Some((edge.source, edge.target))
  }

  object ~~ {
    def unapply[V, L](edge: UndirectedEdge[V, L]): Option[(V, V)] = Some((edge.vertex1, edge.vertex2))
  }

  object |> {
    def unapply[V, L](edge: DirectedEdge[V, L]): Option[((V, L), V)] = Some(((edge.source, edge.label), edge.target))
  }

  object |~ {
    def unapply[V, L](edge: UndirectedEdge[V, L]): Option[((V, L), V)] =
      Some(((edge.vertex1, edge.label), edge.vertex2))
  }
}
