package codes.quine.labo.graph

import minitest.SimpleTestSuite

import syntax._

object MutualAdjacencyMapSuite extends SimpleTestSuite {
  test("MutualAdjacencyMap") {
    val g = MutualAdjacencyMap(
      1 ~| 0 |> 2,
      2 ~| 1 |> 1,
      2 ~| 2 |~ 3,
      Vertex(4)
    )

    assertEquals(g.vertices, Set(1, 2, 3, 4))

    assertEquals(g.edges.sorted, Seq(1 ~| 0 |> 2, 2 ~| 1 |> 1, 2 ~| 2 |> 3, 3 ~| 2 |> 2))

    assertEquals(g.label(1, 2), Some(0))
    assertEquals(g.label(2, 1), Some(1))
    assertEquals(g.label(2, 3), Some(2))
    assertEquals(g.label(3, 2), Some(2))
    assertEquals(g.label(1, 4), None)

    assertEquals(g.in(1).sorted, Seq((2, 1)))
    assertEquals(g.in(2).sorted, Seq((1, 0), (3, 2)))
    assertEquals(g.in(3).sorted, Seq((2, 2)))
    assertEquals(g.in(4).sorted, Seq.empty)

    assertEquals(g.out(1).sorted, Seq((2, 0)))
    assertEquals(g.out(2).sorted, Seq((1, 1), (3, 2)))
    assertEquals(g.out(3).sorted, Seq((2, 2)))
    assertEquals(g.out(4).sorted, Seq.empty)

    assert(MutualAdjacencyMap.from(g.edges) == MutualAdjacencyMap.from(g.edges))
    assert(MutualAdjacencyMap.from(g.edges).## == MutualAdjacencyMap.from(g.edges).##)
    assert(MutualAdjacencyMap.from(g.edges) != MutualAdjacencyMap.from(g.edges.tail))
    assert(MutualAdjacencyMap.from(g.edges).## != MutualAdjacencyMap.from(g.edges.tail).##)
    assert(MutualAdjacencyMap.from(g.edges) != 1)

    val pattern = """MutualAdjacencyMap\((.*)\)""".r
    assertEquals(
      g.toString match { case pattern(params) => params.split(", ").toSeq.sorted },
      Seq("1 ~| 0 |> 2", "2 ~| 1 |> 1", "2 ~| 2 |> 3", "3 ~| 2 |> 2", "Vertex(4)")
    )
  }
}
