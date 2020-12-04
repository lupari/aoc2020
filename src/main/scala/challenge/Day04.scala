package challenge

import scala.io.Source

object Day04 {

  case class Passport(byr: Int,
                      iyr: Int,
                      eyr: Int,
                      hgt: String,
                      hcl: String,
                      ecl: String,
                      pid: String) {
    def checkByr: Boolean = 1920 <= byr && byr <= 2002
    def checkIyr: Boolean = 2010 <= iyr && iyr <= 2020
    def checkEyr: Boolean = 2020 <= eyr && eyr <= 2030
    def checkHgt: Boolean = hgt.dropRight(2).toIntOption match {
      case None => false
      case Some(h) =>
        hgt.takeRight(2) match {
          case "in" => 59 <= h && h <= 76
          case "cm" => 150 <= h && h <= 193
          case _    => false
        }
    }
    def checkHcl: Boolean =
      hcl.length == 7 && hcl.head == '#' && hcl.tail.forall(c => c.isDigit || "abcdef".contains(c))
    def checkEcl: Boolean = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(ecl)
    def checkPid: Boolean = pid.length == 9 && pid.forall(_.isDigit)
    def isValid: Boolean =
      checkByr && checkIyr && checkEyr && checkHgt && checkHcl && checkEcl && checkPid
  }
  object Passport {
    def parse(s: String): Option[Passport] = {
      val m = s
        .replace('\n', ' ')
        .split(' ')
        .map(f => (f.take(3), Some(f.drop(4))))
        .toMap
        .withDefaultValue(None)
      val (byr, iyr, eyr, hgt, hcl, ecl, pid) =
        (m("byr").flatMap(_.toIntOption),
         m("iyr").flatMap(_.toIntOption),
         m("eyr").flatMap(_.toIntOption),
         m("hgt"),
         m("hcl"),
         m("ecl"),
         m("pid"))
      if (byr.isDefined && iyr.isDefined && eyr.isDefined && hgt.isDefined && hcl.isDefined && ecl.isDefined && pid.isDefined)
        Some(Passport(byr.get, iyr.get, eyr.get, hgt.get, hcl.get, ecl.get, pid.get))
      else None
    }
  }

  val input: List[String] = Source.fromResource("day04.txt").mkString.split("\n\n").toList

  def partOne(): Int = {
    val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    input.count(p => requiredFields.forall(f => p.contains(f + ':')))
  }

  def partTwo(): Int = input.flatMap(Passport.parse).count(_.isValid)

}
