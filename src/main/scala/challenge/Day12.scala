package challenge

import lib.Points.{Dir, Point, Position}

import scala.io.Source

object Day12 {

  trait Vessel {
    def fwd(n: Int): Vessel
    def right(d: Int): Vessel
    def left(d: Int): Vessel
    def other(d: Char, n: Int): Vessel
    def position(): Point
  }
  case class Ship1(loc: Dir) extends Vessel {
    override def fwd(n: Int): Vessel             = Ship1(loc.forward(n))
    override def right(d: Int): Vessel           = Ship1(loc.turn(d))
    override def left(d: Int): Vessel            = Ship1(loc.turn(-d))
    override def other(d: Char, n: Int): Vessel  = Ship1(loc.turn(d).forward(n).turn(loc.dir))
    override def position(): Point               = loc.p
  }
  case class Ship2(loc: Point, wp: Point) extends Vessel {
    override def fwd(n: Int): Vessel             = Ship2(loc + wp * n, wp)
    override def right(d: Int): Vessel           = Ship2(loc, wp.rotate(d))
    override def left(d: Int): Vessel            = Ship2(loc, wp.rotate(-d))
    override def other(d: Char, n: Int): Vessel  = Ship2(loc, wp + Position.directions(d) * n)
    override def position(): Point               = loc
  }

  def navigate(vessel: Vessel): Point = {
    def next(vessel: Vessel, cmd: (Char, Int)): Vessel = cmd match {
      case ('F', n) => vessel.fwd(n)
      case ('R', n) => vessel.right(n)
      case ('L', n) => vessel.left(n)
      case (c, n)   => vessel.other(c, n)
    }

    directions.foldLeft(vessel)((a, b) => next(a, b)).position()
  }

  def parse(s: String): (Char, Int) = (s.head, s.tail.toInt)
  val directions: List[(Char, Int)] = Source.fromResource("day12.txt").getLines().map(parse).toList

  def partOne(): Int = navigate(Ship1(Dir(Position.zero, 'R'))).manhattan()
  def partTwo(): Int = navigate(Ship2(Position.zero, Point(10, 1))).manhattan()

}
