package codes.quine.labo.graph

import minitest.SimpleTestSuite

object EdgeSuite extends SimpleTestSuite {
  test("syntax") {
    import syntax._

    assertEquals(1 ~> 2, DirectedEdge(1, 2, ()))
    assertEquals(1 ~~ 2, UndirectedEdge(1, 2, ()))
    assertEquals(1 ~| 2 |> 3, DirectedEdge(1, 3, 2))
    assertEquals(1 ~| 2 |~ 3, UndirectedEdge(1, 3, 2))

    assertEquals((1 ~> 2) match { case v1 ~> v2 => (v1, v2) }, (1, 2))
    assertEquals((1 ~~ 2) match { case v1 ~~ v2 => (v1, v2) }, (1, 2))
    assertEquals((1 ~| 2 |> 3) match { case v1 ~| l |> v2 => (v1, l, v2) }, (1, 2, 3))
    assertEquals((1 ~| 2 |~ 3) match { case v1 ~| l |~ v2 => (v1, l, v2) }, (1, 2, 3))
  }

  test("DirectedEdge") {
    assertEquals(DirectedEdge(1, 3, 2).toString, "1 ~| 2 |> 3")
  }

  test("UndirectedEdge") {
    assertEquals(UndirectedEdge(1, 3, 2).toString, "1 ~| 2 |~ 3")
  }
}
