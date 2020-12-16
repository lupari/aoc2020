package challenge

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day16 {

  case class Rule(name: String, valid1: Range, valid2: Range) {
    def validate(n: Int): Boolean = valid1.contains(n) || valid2.contains(n)
  }
  case class Ticket(rules: List[Rule], mine: List[Int], nearby: List[List[Int]]) {
    def errorRate(): Int = nearby.flatten.filter(n => !rules.exists(r => r.validate(n))).sum
    def discardInvalid(): Ticket =
      Ticket(rules, mine, nearby.filter(n => n.forall(i => rules.exists(r => r.validate(i)))))
  }

  val rulePattern: Regex = """(.*): (\d+)-(\d+) or (\d+)-(\d+)""".r
  def parseRule(s: String): Rule = s match {
    case rulePattern(name, r11, r12, r21, r22) =>
      Rule(name, Range(r11.toInt, r12.toInt).inclusive, Range(r21.toInt, r22.toInt).inclusive)
  }
  def parseRules(rules: List[String]): List[Rule] = rules.map(parseRule)

  def parseTicket(input: List[String]): Ticket = {
    val rulePart   = input.takeWhile(!_.isBlank)
    val minePart   = input.dropWhile(!_.contains("your ticket:")).drop(1).takeWhile(!_.isBlank)
    val nearbyPart = input.dropWhile(!_.contains("nearby tickets:")).drop(1)
    Ticket(parseRules(rulePart),
           minePart.flatMap(_.split(",").flatMap(_.toIntOption)),
           nearbyPart.map(_.split(",").flatMap(_.toIntOption).toList))
  }

  def findRuleMapping(): Map[Int, Rule] = {
    val values = ticket.discardInvalid().nearby.transpose

    def findMatchingRule(vs: List[Int], rules: List[Rule]): List[Rule] =
      rules.filter(r => vs.forall(v => r.validate(v)))

    @tailrec
    def _find(xs: List[List[Int]], rules: Map[String, Rule], acc: Map[Int, Rule]): Map[Int, Rule] =
      xs match {
        case Nil => acc
        case _ =>
          val matches =
            xs.map(x => (x, findMatchingRule(x, rules.values.toList))).filter(_._2.length == 1)
          val nextValues = xs.diff(matches.map(_._1))
          val newMatches = matches.map(m => (values.indexOf(m._1), m._2.head))
          val newRules   = rules -- matches.map(_._2.head.name)
          _find(nextValues, newRules, acc ++ newMatches)
      }

    _find(values, ticket.rules.map(r => r.name -> r).toMap, Map.empty)
  }

  val ticket: Ticket = parseTicket(Source.fromResource("day16.txt").getLines().toList)

  def partOne(): Int = ticket.errorRate()
  def partTwo(): Long = {
    val map     = findRuleMapping()
    val indices = map.filter(_._2.name.startsWith("departure")).keys.toList
    ticket.mine.zipWithIndex.filter(i => indices.contains(i._2)).map(_._1.toLong).product
  }

}
