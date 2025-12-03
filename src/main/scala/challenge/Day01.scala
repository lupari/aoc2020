package challenge

import scala.io.Source

object Day01 {

  val entries: List[Int] = Source.fromResource("day01.txt").getLines().map(_.toInt).toList

  def partOne(): Int = entries.combinations(2).find(_.sum == 2020).get.product
  def partTwo(): Int = entries.combinations(3).find(_.sum == 2020).get.product

}
