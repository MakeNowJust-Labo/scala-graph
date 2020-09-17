package codes.quine.labo.graph
package util

import scala.collection.mutable

final case class UnionFind[T](private val data: mutable.Map[T, Either[Int, T]]) {
  def union(x: T, y: T): Unit = {
    val rootX = find(x)
    val rootY = find(y)
    if (rootX != rootY) {
      val Left(rankX) = data(rootX)
      val Left(rankY) = data(rootY)
      if (rankX < rankY) {
        data(rootY) = Left(rankX + rankY)
        data(rootX) = Right(rootY)
      } else {
        data(rootX) = Left(rankX + rankY)
        data(rootY) = Right(rootX)
      }
    }
  }

  def find(x: T): T =
    data.get(x) match {
      case None =>
        data(x) = Left(1)
        x
      case Some(Left(_)) =>
        x
      case Some(Right(y)) =>
        val rootY = find(y)
        data(x) = Right(rootY)
        rootY
    }
}
