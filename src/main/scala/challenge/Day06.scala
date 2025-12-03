package challenge

import scala.io.Source

object Day06 {

  val answers: List[String] = Source.fromResource("day06.txt").mkString.split("\n\n").toList

  def partOne(): Int = answers.flatMap(_.filter(_.isLetter).toSet).length
  def partTwo(): Int =
    answers
      .map(_.linesIterator.toSeq)
      .flatMap(g => g.mkString.toSet.filter(c => g.forall(_.contains(c))))
      .length

}
