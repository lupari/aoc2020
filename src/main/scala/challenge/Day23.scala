package challenge

object Day23 {

  implicit class CycledInt(i: Int) {
    def at(p: Int): Int = (i % p + p) % p
  }

  case class Round(order: Map[Int, Int], current: Int) {
    def iterator(from: Int): Iterator[(Int, Seq[Int])] =
      Iterator.iterate((from, Seq[Int]()))(a => {
        val next = order(a._1)
        (next, a._2 :+ next)
      })
  }
  object Round {
    // Order is basically a linked list implemented as a map
    def from(xs: List[Int]): Round = {
      val order = xs.sliding(2).map(pair => (pair.head, pair.last)).toMap + (xs.last -> xs.head)
      Round(order, xs.head)
    }
  }

  def play(cups: List[Int], count: Int): Round = {
    val max = cups.max

    def move(round: Round): Round = {
      val Round(order, current) = round
      val pickup1               = order(current)
      val pickup2               = order(pickup1)
      val pickup3               = order(pickup2)
      val destination = Iterator
        .iterate(current)(i => ((i - 2) at max) + 1)
        .find(i => i != current && i != pickup1 && i != pickup2 && i != pickup3) // do not use a set lookup because of perf
        .get
      val order2 = order + (current -> order(pickup3)) + (destination -> pickup1) +
        (pickup3 -> order(destination))
      Round(order2, order2(current))
    }

    Iterator.iterate(Round.from(cups))(move).drop(count).next()
  }

  val cups: List[Int] = "364289715".map(_.asDigit).toList

  def partOne(): Int = {
    val lastRound = play(cups, 100)
    val cups2to9  = lastRound.iterator(1).drop(1).dropWhile(_._1 != 1).next()._2.init
    cups2to9.mkString.toInt
  }

  def partTwo(): Long = {
    val cups2     = cups ++ ((cups.max + 1) to 1000000)
    val lastRound = play(cups2, 10000000)
    val cups2to3  = lastRound.iterator(1).drop(2).next()._2
    cups2to3.map(_.toLong).product
  }

}
