package challenge

import scala.annotation.tailrec
import scala.io.Source

object Day09 {

  def findWeakness(numbers: List[Long], n: Long): Option[Long] = {

    @tailrec
    def helper(xs: List[Long]): Option[Long] = xs match {
      case Nil => None
      case h :: t =>
        val sum = t.scan(h)(_ + _).takeWhile(_ <= n)
        if (sum.last == n) {
          val terms = (h :: t).take(sum.length)
          Some(terms.min + terms.max)
        } else helper(t)
    }

    helper(numbers)
  }

  val input: List[Long] = Source.fromResource("day09.txt").getLines().map(_.toLong).toList

  def partOne(): Long =
    input
      .sliding(25 + 1)
      .find(xs => !xs.init.combinations(2).exists(_.sum == xs.last))
      .get
      .last

  def partTwo(): Long = findWeakness(input, 138879426).get

}
