package challenge

import scala.annotation.tailrec
import scala.io.Source

object Day08 {

  type Instruction = (String, Int)
  type Program     = List[Instruction]
  case class Output(success: Boolean, value: Int)

  def exec(program: Program): Output = {

    @tailrec
    def helper(ptr: Int, acc: Int, seen: Set[Int]): Output = ptr match {
      case p if p == program.length => Output(success = true, acc)
      case p if seen.contains(p)    => Output(success = false, acc) // Will not halt
      case _ =>
        program(ptr) match {
          case ("nop", _) => helper(ptr + 1, acc, seen + ptr)
          case ("acc", x) => helper(ptr + 1, acc + x, seen + ptr)
          case ("jmp", x) => helper(ptr + x, acc, seen + ptr)
        }
    }

    helper(0, 0, Set.empty)
  }

  def fixAndExec(program: Program): Output =
    program.zipWithIndex
      .filter(i => Seq("jmp", "nop").contains(i._1._1))
      .map {
        case (("jmp", x), i) => exec(program.updated(i, ("nop", x)))
        case (("nop", x), i) => exec(program.updated(i, ("jmp", x)))
      }
      .find(_.success)
      .get

  def parse(s: String): Instruction = (s.take(3), s.drop(4).toInt)

  val program: Program = Source.fromResource("day08.txt").getLines().map(parse).toList

  def partOne(): Int = exec(program).value
  def partTwo(): Int = fixAndExec(program).value

}
