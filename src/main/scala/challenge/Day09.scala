package challenge

import scala.annotation.tailrec
import scala.io.Source

object Day09 {

  def findTermSum(ns: List[Long], n: Long): Option[Long] = {

    @tailrec
    def lookup(xs: List[Long], terms: List[Long]): Option[List[Long]] = xs match {
      case Nil => None
      case h :: t =>
        terms.sum + h match {
          case x if x > n  => None
          case x if x == n => Some(terms :+ h)
          case _           => lookup(t, terms :+ h)
        }
    }

    @tailrec
    def _findTermSum(xs: List[Long]): Option[Long] = xs match {
      case Nil => None
      case h :: t =>
        lookup(h :: t, Nil) match {
          case None        => _findTermSum(t)
          case Some(terms) => Some(terms.min + terms.max)
        }
    }

    _findTermSum(ns)
  }

  val input: List[Long] = Source.fromResource("day09.txt").getLines().map(_.toLong).toList

  def partOne(): Long =
    input
      .sliding(26)
      .find(xs => !xs.dropRight(1).combinations(2).exists(c => c.sum == xs.last))
      .get
      .last

  def partTwo(): Long = findTermSum(input, 138879426).get

}
