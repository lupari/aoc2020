package lib

import scala.annotation.tailrec

object Parsers {

  object AST {
    case class ASTNode(op: Char, left: Option[ASTNode], right: Option[ASTNode])

    def parse(expr: String)(precedenceFn: (Char, Char) => Boolean): ASTNode = {

      def addNode(stack: List[ASTNode], op: Char): ASTNode =
        ASTNode(op, stack.drop(1).headOption, stack.headOption)

      @tailrec
      def proc(xs: List[Char], stack: List[Char], acc: List[ASTNode]): ASTNode = xs match {
        case Nil => addNode(acc, stack.head)
        case h :: t =>
          h match {
            case n if n.isDigit => proc(t, stack, ASTNode(h, None, None) +: acc)
            case '+' | '*' =>
              val nextOut   = stack.takeWhile(s => s != '(' && precedenceFn(s, h))
              val nextStack = h +: stack.drop(nextOut.length)
              val nextAcc   = nextOut.map(addNode(acc, _)).reverse
              proc(t, nextStack, nextAcc ++ acc.drop(nextAcc.length * 2))
            case '(' => proc(t, h +: stack, acc)
            case ')' =>
              val nextOut   = stack.takeWhile(_ != '(')
              val nextStack = stack.drop(nextOut.length).drop(1)
              val nextAcc   = nextOut.map(addNode(acc, _))
              proc(t, nextStack, nextAcc.reverse ++ acc.drop(nextAcc.length * 2))
          }
      }

      proc(expr.filterNot(_ == ' ').toList, Nil, Nil)
    }

    def eval(node: ASTNode): Long = {

      node match {
        case ASTNode(op, Some(l), Some(r)) =>
          op match {
            case '+' => eval(l) + eval(r)
            case '*' => eval(l) * eval(r)
          }
        case ASTNode(op, _, _) => op.asDigit.toLong
      }
    }

  }

  object Postfix {

    def postfix(expr: String)(precedenceFn: (Char, Char) => Boolean): List[Char] = {

      @tailrec
      def proc(xs: List[Char], stack: List[Char], acc: List[Char]): List[Char] = xs match {
        case Nil => acc ++ stack
        case h :: t =>
          h match {
            case n if n.isDigit => proc(t, stack, acc :+ n)
            case '+' | '*' =>
              val nextOut   = stack.takeWhile(s => s != '(' && precedenceFn(s, h))
              val nextStack = h +: stack.drop(nextOut.length)
              proc(t, nextStack, acc ++ nextOut)
            case '(' => proc(t, h +: stack, acc)
            case ')' =>
              val nextOut   = stack.takeWhile(_ != '(')
              val nextStack = stack.drop(nextOut.length).drop(1)
              proc(t, nextStack, acc ++ nextOut)
          }
      }

      proc(expr.filterNot(_ == ' ').toList, Nil, Nil)
    }

    def evalPostfix(expr: List[Char]): Long = {

      @tailrec
      def proc(xs: List[Char], stack: List[Long]): Long = xs match {
        case Nil => stack.head
        case h :: t =>
          h match {
            case n if n.isDigit => proc(t, n.asDigit.toLong +: stack)
            case _ =>
              val rhs = stack.head
              val lhs = stack.tail.head
              val op  = if (h == '+') (lhs + rhs) else (lhs * rhs)
              proc(t, op +: stack.drop(2))
          }
      }

      proc(expr, Nil)

    }

    def evalInfix(expr: String)(precedenceFn: (Char, Char) => Boolean): Long =
      evalPostfix(postfix(expr)(precedenceFn))
  }

}
