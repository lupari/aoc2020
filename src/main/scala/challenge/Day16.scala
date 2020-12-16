package challenge

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day16 {

  type Ticket = List[Int]
  case class Rule(name: String, valid1: Range.Inclusive, valid2: Range.Inclusive) {
    def validate(n: Int): Boolean    = valid1.contains(n) || valid2.contains(n)
    def validate(t: Ticket): Boolean = t.forall(validate)
  }

  val rulePattern: Regex = """(.*): (\d+)-(\d+) or (\d+)-(\d+)""".r
  def parseRule(s: String): Rule = s match {
    case rulePattern(name, r11, r12, r21, r22) =>
      Rule(name, r11.toInt to r12.toInt, r21.toInt to r22.toInt)
  }
  def parse(s: String): (List[Rule], Ticket, List[Ticket]) = {
    val Seq(rules, mine, nearby) = s.split("\n\n").toSeq
    (rules.linesIterator.map(parseRule).toList,
     mine.linesIterator.drop(1).flatMap(_.split(",").flatMap(_.toIntOption)).toList,
     nearby.linesIterator.drop(1).map(_.split(",").flatMap(_.toIntOption).toList).toList)
  }

  def findRuleMapping(): Map[Int, String] = {
    val positions = nearby.filter(c => rules.exists(_.validate(c))).transpose

    @tailrec
    def map(xs: Set[List[Int]], rules: Set[Rule], acc: Map[Int, String]): Map[Int, String] =
      if (xs.isEmpty) acc
      else {
        val matches = xs
          .map(x => (x, rules.filter(_.validate(x))))
          .collect { case (x, rs) if rs.size == 1 => (x, rs.head) }
        val newMatches = matches.map(m => (positions.indexOf(m._1), m._2.name))
        map(xs -- matches.map(_._1), rules -- matches.map(_._2), acc ++ newMatches)
      }

    map(positions.toSet, rules.toSet, Map.empty)
  }

  val (rules, mine, nearby) = parse(Source.fromResource("day16.txt").mkString)

  def partOne(): Int = nearby.flatten.filter(n => !rules.exists(r => r.validate(n))).sum
  def partTwo(): Long = {
    val departures = findRuleMapping().filter(_._2.startsWith("departure")).keys.toList
    mine.zipWithIndex.collect { case (x, i) if departures.contains(i) => x.toLong }.product
  }

}
