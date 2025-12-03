package challenge

import scala.io.Source
import scala.util.matching.Regex

object Day19 {

  trait Rule
  case class Value(v: Char)             extends Rule
  case class Single(i: Int)             extends Rule
  case class Both(r1: Rule, r2: Rule)   extends Rule
  case class Either(r1: Rule, r2: Rule) extends Rule

  object RulesParser {
    def validateFn(rules: Map[Int, Rule]): String => Boolean = {

      def step(rule: Rule, chars: List[Char]): Iterator[List[Char]] = rule match {
        case Value(v) =>
          chars match {
            case h :: t if h == v => Iterator(t)
            case _                => Iterator.empty
          }
        case Single(i)    => step(rules(i), chars)
        case Both(a, b)   => step(a, chars).flatMap(step(b, _))
        case Either(a, b) => step(a, chars) ++ step(b, chars)
      }

      s =>
        step(rules(0), s.toList).contains(Nil)
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

  val input: List[String]  = Source.fromResource("day19.txt").getLines().toList
  val values: List[String] = input.reverse.takeWhile(_.nonEmpty)

  def partOne(): Int = {
    val rules     = input.takeWhile(_.nonEmpty).map(parseRule).toMap
    val validator = RulesParser.validateFn(rules)
    values.count(validator)
  }
  def partTwo(): Int = {
    val rules = input.takeWhile(_.nonEmpty).map(parseRule).toMap ++ Map(
      8  -> Either(Single(42), Both(Single(42), Single(8))),
      11 -> Either(Both(Single(42), Single(31)), Both(Both(Single(42), Single(11)), Single(31)))
    )
    val validator = RulesParser.validateFn(rules)
    values.count(validator)
  }

}
