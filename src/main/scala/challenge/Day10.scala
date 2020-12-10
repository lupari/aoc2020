package challenge

import scala.annotation.tailrec
import scala.io.Source

object Day10 {

  def diff(adapters: List[Int]): Int = {
    val diffs = (0 +: adapters).sorted.sliding(2).map(g => g.last - g.head).toSeq
    diffs.count(_ == 1) * diffs.count(_ == 3)
  }

  def connections(adapters: List[Int]): Long = {

    @tailrec
    def _connections(xs: List[Int], seen: Map[Int, Long]): Long = xs match {
      case Nil => seen(builtin)
      case h :: t =>
        val sum = seen(h - 3) + seen(h - 2) + seen(h - 1)
        _connections(t, seen + (h -> sum))
    }

    _connections(adapters.sorted, Map(0 -> 1L).withDefaultValue(0))
  }

  val adapters: List[Int] = Source.fromResource("day10.txt").getLines().map(_.toInt).toList
  val builtin: Int        = adapters.max + 3

  def partOne(): Int  = diff(adapters :+ builtin)
  def partTwo(): Long = connections(adapters :+ builtin)

}
