package challenge

import scala.io.Source

object Day06 {

  val answers: List[String] = Source.fromResource("day06.txt").mkString.trim.split("\n\n").toList

  def partOne(): Int = answers.map(_.filter(_.isLetter).toSet.mkString).mkString.length
  def partTwo(): Int =
    answers
      .map(_.split("\n"))
      .flatMap(l => ('a' to 'z').filter(c => l.forall(_.contains(c))))
      .length

}
