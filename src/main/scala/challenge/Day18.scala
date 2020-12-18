package challenge

import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

object Day18 {

  // Inspiration from https://gist.github.com/kiritsuku/5529436

  class Parser extends JavaTokenParsers {
    def numeric: Parser[Long] = wholeNumber ^^ (number => number.toLong)
  }

  class Parser1 extends Parser {
    def expr: Parser[Long] = term ~ rep("+" ~ term | "*" ~ term) ^^ {
      case n ~ ns => ns.foldLeft(n)((a, b) => if (b._1 == "+") a + b._2 else a * b._2)
    }
    def term: Parser[Long] = numeric | "(" ~> expr <~ ")"
  }
  object Parser1 extends Parser1 {
    def calculate(s: String): Long = parse(expr, s).get
  }

  class Parser2 extends Parser {
    def expr: Parser[Long] = term ~ rep("*" ~ term) ^^ {
      case n ~ ns => ns.foldLeft(n)((a, b) => a * b._2)
    }
    def term: Parser[Long] = factor ~ rep("+" ~ factor) ^^ {
      case n ~ ns => ns.foldLeft(n)((a, b) => a + b._2)
    }
    def factor: Parser[Long] = numeric | "(" ~> expr <~ ")"
  }
  object Parser2 extends Parser2 {
    def calculate(s: String): Long = parse(expr, s).get
  }

  val sentences: List[String] = Source.fromResource("day18.txt").getLines().toList

  def partOne(): Long = sentences.map(Parser1.calculate).sum
  def partTwo(): Long = sentences.map(Parser2.calculate).sum

}
