package challenge

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Day19 {

  trait Rule
  case class Value(v: Char)             extends Rule
  case class Single(i: Int)             extends Rule
  case class Both(r1: Rule, r2: Rule)   extends Rule
  case class Either(r1: Rule, r2: Rule) extends Rule

  object RulesParser extends RegexParsers {
    def validateFn(rules: Map[Int, Rule]): String => Boolean = {

      def step(rule: Rule): Parser[Unit] = rule match {
        case Value(v)     => literal(v.toString) ^^^ ()
        case Single(i)    => step(rules(i))
        case Both(a, b)   => (step(a) ~ step(b)) ^^^ ()
        case Either(a, b) => step(a) | step(b)
      }

      val parser = step(rules(0))
      parseAll(parser, _).successful
    }
  }

  val ruleRegex: Regex  = """(\d+): (.*)""".r
  val valueRegex: Regex = """"(.*)"""".r
  def parseRule(s: String): (Int, Rule) = s match {
    case ruleRegex(id, content) =>
      val rule = content match {
        case valueRegex(v) => Value(v.head)
        case _ =>
          val options = content.split("\\|")
          if (options.length > 1) {
            val children = options.map(o => {
              val singles: List[Single] = o.trim.split(" ").map(p => Single(p.toInt)).toList
              if (singles.length == 1) singles.head else Both(singles.head, singles.last)
            })
            Either(children.head, children.last)
          } else {
            val singles: List[Single] = content.split(" ").map(p => Single(p.toInt)).toList
            if (singles.length > 1) Both(singles.head, singles.last) else singles.head
          }
      }
      (id.toInt, rule)
  }

  def outcomes(key: Int)(implicit rules: Map[Int, String]): Seq[String] = rules(key) match {
    case valueRegex(v) => List(v)
    case rule =>
      rule
        .split(" \\| ")
        .map(_.split(" "))
        .flatMap(part => {
          val subs = part.map(s => outcomes(s.toInt)).toList
          subs match {
            case h :: i :: _ => (for { x <- h.indices; y <- i.indices } yield h(x) + i(y)).toList
            case _           => subs.flatten
          }
        })
  }

  val input: List[String]  = Source.fromResource("day19.txt").getLines().toList
  val values: List[String] = input.reverse.takeWhile(_.nonEmpty)

  def partOne(): Int = {
    val rules     = input.takeWhile(_.nonEmpty).map(parseRule).toMap
    val validator = RulesParser.validateFn(rules)
    values.count(validator)
  }
  def partTwo(): Int = {
    // OK this is ugly, for some reason (that I am incompetent to understand)
    // Scala combinator parser gives wrong answer to this
    // Therefore this deduction based on permutations of problematic rule outcomes
    def parse(s: String) = (s.takeWhile(_ != ':').toInt, s.dropWhile(_ != ' ').drop(1))
    val rules: Map[Int, String] = input.takeWhile(_.nonEmpty).map(parse).toMap ++ Seq(
      8  -> "42 | 42 8",
      11 -> "42 31 | 42 11 31")
    val c42 = outcomes(42)(rules)
    val c31 = outcomes(31)(rules)
    def ok(s: String) =
      c42.contains(s.take(8)) && c42.contains(s.slice(8, 16)) && c31.contains(s.takeRight(8))
    values
      .collect { case v if ok(v) => v.drop(16).dropRight(8) }
      .filterNot(v => {
        val c0 = v.grouped(8).takeWhile(c42.contains).length
        val c1 = v.grouped(8).drop(c0).takeWhile(c31.contains).length
        v.drop(8 * (c0 + c1)).nonEmpty || c1 > c0
      })
      .length
  }

}
