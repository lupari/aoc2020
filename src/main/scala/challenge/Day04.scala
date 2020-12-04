package challenge

import base.Challenge

import scala.io.Source

object Day04 extends Challenge {

  override def run(): Any = {
    val passports: List[String] = Source.fromResource("day04.txt").mkString.split("\n\n").toList
    val requiredFields          = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    passports.count(p => requiredFields.forall(f => p.contains(f + ':')))
  }

}
