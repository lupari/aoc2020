package challenge

import scala.io.Source

object Day10 {

  def diff(adapters: List[Int]): Int = {
    val diffs = (0 +: adapters).sorted.sliding(2).map(g => g.last - g.head).toSeq
    diffs.count(_ == 1) * diffs.count(_ == 3)
  }

  def connections(adapters: List[Int]): Long = {
    val init: Map[Int, Long] = Map(0 -> 1L).withDefaultValue(0)
    adapters.sorted.foldLeft(init)((a, b) => a + (b -> (a(b - 3) + a(b - 2) + a(b - 1))))(builtin)
  }

  val adapters: List[Int] = Source.fromResource("day10.txt").getLines().map(_.toInt).toList
  val builtin: Int        = adapters.max + 3

  def partOne(): Int  = diff(adapters :+ builtin)
  def partTwo(): Long = connections(adapters :+ builtin)

}
