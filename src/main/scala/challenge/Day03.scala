package challenge

import base.Challenge
import lib.GridImplicits.{Grid, GridInput}
import lib.Points.Point

import scala.annotation.tailrec
import scala.io.Source

object Day03 extends Challenge {

  def countTrees(grid: Grid[Char]): Int = {

    val width  = grid.keys.maxBy(_.x).x
    val height = grid.keys.maxBy(_.y).y

    @tailrec
    def acc(p: Point, n: Int): Int = p match {
      case p1 if p1.y > height => n
      case _ =>
        val curr = grid(Point(p.x % (width + 1), p.y))
        acc(p + Point(3, 1), if (curr == '#') n + 1 else n)
    }

    acc(Point(0, 0), 0)
  }

  override def run(): Any = {
    val grid: Grid[Char] = Source.fromResource("day03.txt").mkString.toList.toGrid
    countTrees(grid)
  }

}
