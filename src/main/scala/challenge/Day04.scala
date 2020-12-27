package challenge

import scala.io.Source

object Day04 {

  val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val validators: Map[String, String => Boolean] = Map(
    "byr" -> (_.toIntOption.exists(i => 1920 <= i && i <= 2002)),
    "iyr" -> (_.toIntOption.exists(i => 2010 <= i && i <= 2020)),
    "eyr" -> (_.toIntOption.exists(i => 2020 <= i && i <= 2030)),
    "hgt" -> (
        s =>
          s.dropRight(2)
            .toIntOption
            .exists(i => {
              s.takeRight(2) match {
                case "cm" => 150 <= i && i <= 193
                case "in" => 59 <= i && i <= 76
                case _    => false
              }
            })),
    "hcl" -> (s =>
      s.length == 7 && s.head == '#' && s.tail.forall(c => c.isDigit || ('a' to 'f').contains(c))),
    "ecl" -> (Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(_)),
    "pid" -> (s => s.length == 9 && s.forall(_.isDigit))
  )

  def isValid(s: String): Boolean = {
    val data = s.split("\\s+").map(c => (c.take(3), c.drop(4))).toMap
    requiredFields.forall(f => data.contains(f) && validators(f)(data(f)))
  }

  val input: List[String] = Source.fromResource("day04.txt").mkString.split("\n\n").toList

  def partOne(): Int = input.count(p => requiredFields.forall(p.contains(_)))
  def partTwo(): Int = input.count(isValid)

}
