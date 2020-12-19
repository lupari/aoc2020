package challenge

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

object Day19 {

  trait Rule
  case class Value(v: Char)             extends Rule
  case class Single(i: Int)             extends Rule
  case class Combo(r1: Rule, r2: Rule)  extends Rule
  case class Either(r1: Rule, r2: Rule) extends Rule

  object RulesParser extends RegexParsers {
    def validateFn(rules: Map[Int, Rule]): String => Boolean = {

      def step(rule: Rule): Parser[Unit] = rule match {
        case Value(v)     => literal(v.toString) ^^^ ()
        case Single(i)    => step(rules(i))
        case Combo(a, b)  => (step(a) ~ step(b)) ^^^ ()
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
        case valueRegex(v) => Value(v.head) // value
        case _ =>
          val options = content.split("\\|")
          if (options.length > 1) {
            val children = options.map(o => {
              val singles: List[Single] = o.trim.split(" ").map(p => Single(p.toInt)).toList
              if (singles.length == 1) singles.head else Combo(singles.head, singles.last)
            })
            Either(children.head, children.last)
          } else {
            val singles: List[Single] = content.split(" ").map(p => Single(p.toInt)).toList
            if (singles.length > 1) Combo(singles.head, singles.last) else singles.head
          }
      }
      (id.toInt, rule)
  }

  val input: List[String]   = Source.fromResource("day19.txt").getLines().toList
  val rules: Map[Int, Rule] = input.takeWhile(_.nonEmpty).map(parseRule).toMap
  val values: List[String]  = input.reverse.takeWhile(_.nonEmpty)

  def partOne(): Int = {
    val checker = RulesParser.validateFn(rules)
    values.count(checker(_))
  }
  def partTwo(): Int = {
    val r8 = Either(Single(42), Combo(Single(42), Single(8)))
    val r11 =
      Either(Combo(Single(42), Single(31)), Combo(Single(42), Combo(Single(11), Single(31))))
    val rules2  = rules ++ Seq(8 -> r8, 11 -> r11)
    val checker = RulesParser.validateFn(rules2)
    val k2      = checker("aaaaabbaabaaaaababaa")
    val matches = values.filter(checker)
    values.count(checker)
  }

}
// aaaaabbaabaaaaababaa
