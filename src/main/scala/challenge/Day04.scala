package challenge

import scala.io.Source

object Day04 {

  val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val validators: Map[String, String => Boolean] = Map(
    "byr" -> ((s: String) => s.toIntOption.exists(i => 1920 <= i && i <= 2002)),
    "iyr" -> ((s: String) => s.toIntOption.exists(i => 2010 <= i && i <= 2020)),
    "eyr" -> ((s: String) => s.toIntOption.exists(i => 2020 <= i && i <= 2030)),
    "hgt" -> (
        (s: String) =>
          s.dropRight(2)
            .toIntOption
            .exists(i => {
              s.takeRight(2) match {
                case "cm" => 150 <= i && i <= 193
                case "in" => 59 <= i && i <= 76
                case _    => false
              }
            })),
    "hcl" -> ((s: String) =>
      s.length == 7 && s.head == '#' && s.tail.forall(c => c.isDigit || ('a' to 'f').contains(c))),
    "ecl" -> ((s: String) => List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(s)),
    "pid" -> ((s: String) => s.length == 9 && s.forall(_.isDigit))
  )

  def isValid(s: String): Boolean = {
    val config = s.split("\\s+").map(c => (c.take(3), c.drop(4))).toMap
    requiredFields.forall(f => config.contains(f) && validators(f)(config(f)))
  }

  val input: List[String] = Source.fromResource("day04.txt").mkString.split("\n\n").toList

  def partOne(): Int = input.count(p => requiredFields.forall(f => p.contains(f + ':')))
  def partTwo(): Int = input.count(isValid)

}
