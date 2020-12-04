package challenge

import base.Challenge

import scala.io.Source

object Day04b extends Challenge {

  case class Passport(byr: Int,
                      iyr: Int,
                      eyr: Int,
                      hgt: String,
                      hcl: String,
                      ecl: String,
                      pid: String) {
    def isValidByr: Boolean = 1920 <= byr && byr <= 2002
    def isValidIyr: Boolean = 2010 <= iyr && iyr <= 2020
    def isValidEyr: Boolean = 2020 <= eyr && eyr <= 2030
    def isValidHgt: Boolean = hgt.dropRight(2).toIntOption match {
      case None => false
      case Some(h) =>
        hgt.takeRight(2) match {
          case "in" => 59 <= h && h <= 76
          case "cm" => 150 <= h && h <= 193
          case _    => false
        }
    }
    def isValidHcl: Boolean = hcl.toList match {
      case h :: t => h == '#' && t.length == 6 && t.forall(c => c.isDigit || "abcdef".contains(c))
      case _      => false
    }
    def isValidEcl: Boolean = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(ecl)
    def isValidPid: Boolean = pid.length == 9 && pid.forall(_.isDigit)

    def isValid: Boolean =
      isValidByr && isValidIyr && isValidEyr && isValidHgt && isValidHcl && isValidEcl && isValidPid
  }

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

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day04.txt").mkString.split("\n\n").toList
    val passports           = input.flatMap(parse)
    passports.count(_.isValid)
  }

}
