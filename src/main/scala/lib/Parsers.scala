package lib

import scala.annotation.tailrec

object Parsers {

  object AST {
    trait Node
    case class Value(v: Int)                                           extends Node
    case class Tree(op: Char, left: Option[Node], right: Option[Node]) extends Node

    def parse(expr: String)(precedenceFn: (Char, Char) => Boolean): Node = {

      def node(stack: List[Node], op: Char): Node =
        Tree(op, stack.tail.headOption, stack.headOption)

      @tailrec
      def helper(xs: List[Char], stack: List[Char], acc: List[Node]): Node = xs match {
        case Nil => node(acc, stack.head)
        case h :: t =>
          h match {
            case n if n.isDigit => helper(t, stack, Value(h.asDigit) +: acc)
            case '+' | '*' =>
              val out       = stack.takeWhile(s => s != '(' && precedenceFn(s, h))
              val nextStack = h +: stack.drop(out.length)
              val nextAcc   = out.map(node(acc, _))
              helper(t, nextStack, nextAcc ++ acc.drop(nextAcc.length * 2))
            case '(' => helper(t, h +: stack, acc)
            case ')' =>
              val out       = stack.takeWhile(_ != '(')
              val nextStack = stack.drop(out.length).drop(1)
              val nextAcc   = out.map(node(acc, _))
              helper(t, nextStack, nextAcc ++ acc.drop(nextAcc.length * 2))
          }
      }

      helper(expr.filterNot(_ == ' ').toList, Nil, Nil)
    }

    def eval(node: Node): Long = node match {
      case Tree(op, Some(l), Some(r)) =>
        op match {
          case '+' => eval(l) + eval(r)
          case '*' => eval(l) * eval(r)
        }
      case Value(op) => op.toLong
    }
  }

  object Postfix {

    def parse(expr: String)(precedenceFn: (Char, Char) => Boolean): List[Char] = {

      @tailrec
      def helper(xs: List[Char], stack: List[Char], acc: List[Char]): List[Char] = xs match {
        case Nil => acc ++ stack
        case h :: t =>
          h match {
            case n if n.isDigit => helper(t, stack, acc :+ n)
            case '+' | '*' =>
              val out       = stack.takeWhile(s => s != '(' && precedenceFn(s, h))
              val nextStack = h +: stack.drop(out.length)
              helper(t, nextStack, acc ++ out)
            case '(' => helper(t, h +: stack, acc)
            case ')' =>
              val out       = stack.takeWhile(_ != '(')
              val nextStack = stack.drop(out.length).drop(1)
              helper(t, nextStack, acc ++ out)
          }
      }

      helper(expr.filterNot(_ == ' ').toList, Nil, Nil)
    }

    def eval(expr: List[Char]): Long = {

      @tailrec
      def helper(xs: List[Char], stack: List[Long]): Long = xs match {
        case Nil => stack.head
        case h :: t =>
          h match {
            case n if n.isDigit => helper(t, n.asDigit.toLong +: stack)
            case _ =>
              val (right, left) = (stack.head, stack.tail.head)
              val op            = if (h == '+') left + right else left * right
              helper(t, op +: stack.drop(2))
          }
      }

      helper(expr, Nil)
    }
  }
}
