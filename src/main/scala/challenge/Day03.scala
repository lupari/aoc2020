package challenge

import lib.GridImplicits.{Grid, GridInput}
import lib.Points.{Point, Position}

import scala.annotation.tailrec
import scala.io.Source

object Day03 {

  def countTrees(dir: Point): Int = {

    val width  = grid.keys.maxBy(_.x).x
    val height = grid.keys.maxBy(_.y).y

    @tailrec
    def helper(p: Point, acc: Int): Int = p match {
      case point if point.y > height => acc
      case _ =>
        val curr = grid(Point(p.x % (width + 1), p.y))
        helper(p + dir, if (curr == '#') acc + 1 else acc)
    }

    helper(Position.zero, 0)
  }

  val grid: Grid[Char] = Source.fromResource("day03.txt").mkString.toList.toGrid

  def partOne(): Int = countTrees(Point(3, 1))
  def partTwo(): Long = {
    val slopes = List(Point(1, 1), Point(3, 1), Point(5, 1), Point(7, 1), Point(1, 2))
    slopes.map(countTrees(_).toLong).product
  }

}
