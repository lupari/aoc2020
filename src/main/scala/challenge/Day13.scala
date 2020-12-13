package challenge

import scala.annotation.tailrec
import scala.io.Source

object Day13 {

  case class Bus(id: Int, departure: Int)

  def schedule1(data: List[String]): Int = {
    val (t1, buses) = (data.head.toInt, data.last.split(',').flatMap(_.toIntOption).toList)
    val (t2, bus) = Iterator
      .iterate((t1, 0))(tb => (tb._1 + 1, buses.find(tb._1 % _ == 0).getOrElse(0)))
      .dropWhile(_._2 == 0)
      .next()

    (t2 - t1 - 1) * bus
  }

  def schedule2(data: List[String]): Long = {
    val schedule = data.last.split(',').toList
    val buses    = schedule.zipWithIndex.filterNot(_._1.head == 'x').map(b => Bus(b._1.toInt, b._2))

    def ok(xs: List[Bus], t: Long): Boolean = xs.forall(b => (t + b.departure) % b.id == 0)

    @tailrec
    def search(ptr: Int, t: Long): Long = ptr match {
      case p if p == buses.length => t
      case _ =>
        val (seen, h) = (buses.take(ptr), buses.drop(ptr).head)
        val inc       = seen.map(_.id.toLong).product
        val t2 = Iterator
          .iterate((t, false))(t => (t._1 + inc, ok(seen :+ h, t._1 + inc)))
          .dropWhile(!_._2)
          .next()
          ._1
        search(ptr + 1, t2)
    }

    search(1, buses.head.id)
  }

  val input: List[String] = Source.fromResource("day13.txt").getLines().toList
  def partOne(): Int      = schedule1(input)
  def partTwo(): Long     = schedule2(input)

}
