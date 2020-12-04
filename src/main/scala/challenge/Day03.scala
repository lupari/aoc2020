package challenge

import lib.GridImplicits.{Grid, GridInput}
import lib.Points.Point

import scala.annotation.tailrec
import scala.io.Source

object Day03 {

  def countTrees(grid: Grid[Char], dir: Point): Int = {

    val width  = grid.keys.maxBy(_.x).x
    val height = grid.keys.maxBy(_.y).y

    @tailrec
    def acc(p: Point, n: Int): Int = p match {
      case p1 if p1.y > height => n
      case _ =>
        val curr = grid(Point(p.x % (width + 1), p.y))
        acc(p + dir, if (curr == '#') n + 1 else n)
    }

    acc(Point(0, 0), 0)
  }

  val grid: Grid[Char] = Source.fromResource("day03.txt").mkString.toList.toGrid

  def partOne(): Int = countTrees(grid, Point(3, 1))
  def partTwo(): Long = {
    val slopes = List(Point(1, 1), Point(3, 1), Point(5, 1), Point(7, 1), Point(1, 2))
    slopes.map(countTrees(grid, _).toLong).product
  }

}
