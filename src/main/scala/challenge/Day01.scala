package challenge

import base.Challenge

import scala.io.Source

object Day01 extends Challenge {

  override def run(): Any = {
    val entries = Source.fromResource("day01.txt").getLines().map(_.toInt).toList
    entries.combinations(2).find(_.sum == 2020).get.product
  }

}
