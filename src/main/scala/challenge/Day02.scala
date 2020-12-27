package challenge

import scala.io.Source
import scala.util.matching.Regex

object Day02 {

  case class Setting(min: Int, max: Int, letter: Char, pwd: String) {
    def isValid: Boolean  = (min to max).contains(pwd.count(_ == letter))
    def isValid2: Boolean = pwd.charAt(min - 1) == letter ^ pwd.charAt(max - 1) == letter
  }

  val pattern: Regex = """(\d+)-(\d+) (\w): (\w+)""".r
  def parse(s: String): Setting = s match {
    case pattern(min, max, letter, pwd) => Setting(min.toInt, max.toInt, letter.head, pwd)
  }

  val input: List[Setting] = Source.fromResource("day02.txt").getLines().map(parse).toList

  def partOne(): Int = input.count(_.isValid)
  def partTwo(): Int = input.count(_.isValid2)

}
