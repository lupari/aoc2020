package challenge

import scala.annotation.tailrec
import scala.collection.mutable

object Day15 {

  def play(rounds: Int): Int = {

    // Mutable map outperforms the immutable one here
    val numbers = mutable.Map() ++ input.zipWithIndex.map(i => i._1 -> (i._2 + 1))

    @tailrec
    def _play(round: Int, n: Int): Int = round match {
      case r if r == rounds => n
      case _ =>
        val n2 = if (numbers.contains(n)) round - numbers(n) else 0
        numbers += (n -> round)
        _play(round + 1, n2)
    }

    _play(input.length + 1, 0)
  }

  val input: List[Int] = List(0, 14, 6, 20, 1, 4)
  def partOne(): Int   = play(2020)
  def partTwo(): Int   = play(30000000)

}
