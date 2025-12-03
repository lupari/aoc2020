package challenge

import scala.annotation.tailrec

object Day15 {

  def play(rounds: Int): Int = {

    // Maps don't perform well here, lets brutally use an array for the job
    val ns = Array.fill[Int](rounds)(-1)
    input.zipWithIndex.foreach(i => ns(i._1) = i._2 + 1)

    @tailrec
    def helper(round: Int, n: Int): Int = round match {
      case r if r == rounds => n
      case _ =>
        val n2 = if (ns(n) != -1) round - ns(n) else 0
        ns(n) = round
        helper(round + 1, n2)
    }

    helper(input.length + 1, 0)
  }

  val input: List[Int] = List(0, 14, 6, 20, 1, 4)
  def partOne(): Int   = play(2020)
  def partTwo(): Int   = play(30000000)

}
