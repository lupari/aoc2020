package challenge

import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

object Day18 {

  // Inspiration from https://gist.github.com/kiritsuku/5529436

  trait Op
  case class Add(op1: Op, op2: Op) extends Op
  case class Mul(op1: Op, op2: Op) extends Op
  case class Num(op1: Long)        extends Op

  class ArithmeticParser extends JavaTokenParsers {
    def numeric: Parser[Op] = wholeNumber ^^ (number => Num(number.toLong))
    def eval(op: Op): Long = op match {
      case Add(a, b) => eval(a) + eval(b)
      case Mul(a, b) => eval(a) * eval(b)
      case Num(n)    => n
    }
  }

  class Parser1 extends ArithmeticParser { // sum and mul have equal precedence
    def expr: Parser[Op] = term ~ rep("+" ~ term | "*" ~ term) ^^ {
      case n ~ ns => ns.foldLeft(n)((a, b) => if (b._1 == "+") Add(a, b._2) else Mul(a, b._2))
    }
    def term: Parser[Op] = numeric | "(" ~> expr <~ ")"
  }
  object Parser1 extends Parser1 {
    def calculate(s: String): Long = parse(expr, s).map(eval).get
  }

  class Parser2 extends ArithmeticParser { // sum has higher precedence over mul
    def expr: Parser[Op] = term ~ rep("*" ~ term) ^^ {
      case n ~ ns => ns.foldLeft(n)((a, b) => Mul(a, b._2))
    }
    def term: Parser[Op] = factor ~ rep("+" ~ factor) ^^ {
      case n ~ ns => ns.foldLeft(n)((a, b) => Add(a, b._2))
    }
    def factor: Parser[Op] = numeric | "(" ~> expr <~ ")"
  }
  object Parser2 extends Parser2 {
    def calculate(s: String): Long = parse(expr, s).map(eval).get
  }

  val sentences: List[String] = Source.fromResource("day18.txt").getLines().toList

  def partOne(): Long = sentences.map(Parser1.calculate).sum
  def partTwo(): Long = sentences.map(Parser2.calculate).sum
  //AST.eval(AST.parse(s)((a, b) => a == '+' || b == '*'))

}
