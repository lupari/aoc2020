package challenge

import base.Challenge

import scala.io.Source
import scala.util.matching.Regex

object Day02 extends Challenge {

  case class Setting(min: Int, max: Int, letter: Char, pwd: String) {
    def isValid: Boolean = (min to max).contains(pwd.count(_ == letter))
  }

  val pattern: Regex = "(\\d+)-(\\d+) (\\w): (\\w+)".r
  def parse(s: String): Setting = {
    val pattern(min, max, letter, pwd) = s
    Setting(min.toInt, max.toInt, letter.head, pwd)
  }

  override def run(): Any =
    Source.fromResource("day02.txt").getLines().map(parse).count(_.isValid)

}
