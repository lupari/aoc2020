package challenge

import lib.GridImplicits.{Grid, GridInput}
import lib.Points.{Point, Position}

import scala.annotation.tailrec
import scala.io.Source

object Day11 {

  case class Seating(changed: Boolean, placement: Grid[Char])

  def behavior1(seat: Point, g: Grid[Char]): Char = g(seat) match {
    case 'L' => if (seat.surroundings.count(p => g(p) == '#') == 0) '#' else 'L'
    case '#' => if (seat.surroundings.count(p => g(p) == '#') > 3) 'L' else '#'
  }

  def behavior2(seat: Point, g: Grid[Char]): Char = {

    @tailrec
    def nextVisible(dir: Point, p: Point): Option[Char] = {
      val p2 = p + dir
      if ((0 to bounds._1).contains(p2.x) && (0 to bounds._2).contains(p2.y))
        g(p2) match {
          case '.' => nextVisible(dir, p2)
          case c   => Some(c)
        } else None
    }

    val visible = Position.surroundings.flatMap(nextVisible(_, seat))
    g(seat) match {
      case 'L' => if (!visible.contains('#')) '#' else 'L'
      case '#' => if (visible.count(_ == '#') > 4) 'L' else '#'
    }
  }

  def occupiedSeats()(bf: (Point, Grid[Char]) => Char): Int = {

    def nextSeating(g: Grid[Char]): Seating = {
      val g2   = Map.empty[Point, Char].withDefaultValue('.')
      val next = g.keys.foldLeft(g2)((acc, seat) => acc + (seat -> bf(seat, g)))
      Seating(changed = next != g, next)
    }

    Iterator
      .iterate(Seating(changed = true, grid.filterNot(_._2 == '.')))(s => nextSeating(s.placement))
      .find(!_.changed)
      .get
      .placement
      .values
      .count(_ == '#')
  }

  val grid: Grid[Char] =
    Source.fromResource("day11.txt").mkString.toList.toGrid.withDefaultValue('.')
  val bounds: (Int, Int) = (grid.keys.maxBy(_.x).x, grid.keys.maxBy(_.y).y)

  def partOne(): Int = occupiedSeats()(behavior1)
  def partTwo(): Int = occupiedSeats()(behavior2)

}
