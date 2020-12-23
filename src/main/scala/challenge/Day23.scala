package challenge

object Day23 {

  implicit class CycledInt(i: Int) {
    def at(p: Int): Int = (i % p + p) % p
  }

  def cupsIterator(links: Map[Int, Int], from: Int): Iterator[(Int, Vector[Int])] =
    Iterator.iterate((from, Vector[Int]()))(a => {
      val next = links(a._1)
      (next, a._2 :+ next)
    })

  def linkCups(xs: List[Int]): (Int, Map[Int, Int]) = {
    val link = xs.sliding(2).map(pair => (pair.head, pair.last)).toMap + (xs.last -> xs.head)
    (xs.head, link)
  }

  def play(cups: List[Int], count: Int): Map[Int, Int] = {
    val max        = cups.max
    val (i, order) = linkCups(cups)
    val arr        = Array.fill[Int](max + 1)(0)
    for { kv <- order } yield arr(kv._1) = kv._2

    def move(current: Int): Int = {
      val pickup1 = arr(current)
      val pickup2 = arr(pickup1)
      val pickup3 = arr(pickup2)
      val destination = Iterator
        .iterate(current)(i => ((i - 2) at max) + 1)
        .find(i => i != current && i != pickup1 && i != pickup2 && i != pickup3) // do not use a set lookup because of perf
        .get
      arr(current) = arr(pickup3)
      val tmp = arr(destination)
      arr(destination) = pickup1
      arr(pickup3) = tmp
      arr(current)
    }

    val _ = Iterator.iterate(i)(move).drop(count).next()
    arr.indices.map(i => (i, arr(i))).toMap
  }

  val cups: List[Int] = "364289715".map(_.asDigit).toList

  def partOne(): Int = {
    val lastRound = play(cups, 100)
    val cups2to9  = cupsIterator(lastRound, 1).drop(1).dropWhile(_._1 != 1).next()._2.init
    cups2to9.mkString.toInt
  }

  def partTwo(): Long = {
    val cups2     = cups ++ ((cups.max + 1) to 1000000)
    val lastRound = play(cups2, 10000000)
    val cups2to3  = cupsIterator(lastRound, 1).drop(2).next()._2
    cups2to3.map(_.toLong).product
  }

}
