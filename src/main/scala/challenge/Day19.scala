package challenge

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

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

  def permutations(key: Int, rules: Map[Int, String]): List[String] = {
    val rule: String = rules(key).dropWhile(_ != ' ').tail
    rule match {
      case valueRegex(v) => List(v)
      case _ =>
        rule
          .split(" \\| ")
          .toList
          .flatMap(p => {
            val subs = p.split(" ").map(ps => permutations(ps.toInt, rules)).toList
            subs match {
              case h :: i :: _ =>
                (for {
                  x <- h.indices
                  y <- i.indices
                } yield h(x) + i(y)).toList
              case _ => subs.flatten
            }
          })
    }
  }

  val input: List[String]  = Source.fromResource("day19.txt").getLines().toList
  val values: List[String] = input.reverse.takeWhile(_.nonEmpty)

  def partOne(): Int = {
    val rules     = input.takeWhile(_.nonEmpty).map(parseRule).toMap
    val predicate = RulesParser.validateFn(rules)
    values.count(predicate(_))
  }
  def partTwo2(): Int = {
    // this is ugly, for some reason (that I am incompetent to understand)
    // Scala combinator parser gives wrong answer to this
    // Therefore this deduction based on permutations of problematic rule outcomes
    def parse(s: String)         = (s.takeWhile(_ != ':').toInt, s.dropWhile(_ != ' ').drop(1))
    val rules: Map[Int, String]  = input.takeWhile(_.nonEmpty).map(parse).toMap
    val rules2: Map[Int, String] = rules ++ Seq(8 -> "42 | 42 8", 11 -> "42 31 | 42 11 31")
    val p42                      = permutations(42, rules2)
    val p31                      = permutations(31, rules2)
    def ok(v: String) = {
      p42.contains(v.take(8)) && p42.contains(v.slice(8, 16)) && p31.contains(v.takeRight(8))
    }
    val v2 = values.filter(ok)
    v2.filterNot(v => {
        val q  = v.drop(16).dropRight(8)
        val c0 = q.grouped(8).takeWhile(p42.contains).length
        val c1 = q.grouped(8).drop(c0).takeWhile(p31.contains).length
        q.drop(8 * (c0 + c1)).nonEmpty || c1 > c0
      })
      .length
  }

}
