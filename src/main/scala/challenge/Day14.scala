package challenge

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day14 {

  trait Cmd
  case class Mem(addr: Int, value: Int) extends Cmd
  case class Mask(mask: String)         extends Cmd

  def mem1(cmd: Mem, mask: String): Seq[(Long, Long)] = {

    def bitmask(x: Long): Long = {
      val and = BigInt(mask.replace('X', '1'), 2)
      val or  = BigInt(mask.replace('X', '0'), 2)
      (x & and | or).toLong
    }

    Seq(cmd.addr.toLong -> bitmask(cmd.value))
  }

  def mem2(cmd: Mem, mask: String): Seq[(Long, Long)] = {

    def leftPad(l: Int, s: String, c: Char = '0'): String = List.fill(l - s.length)(c).mkString + s

    def bitmask(x: Long): Seq[Long] = {
      val result = leftPad(36, x.toBinaryString)
        .zip(mask)
        .map {
          case (_, '1') => '1'
          case (c, '0') => c
          case _        => 'X'
        }
      val floats = result.count(_ == 'X')
      val combinations = (0 until math.pow(2, floats).toInt)
        .map(i => leftPad(floats, i.toBinaryString))
      val addresses = combinations.map(c => {
        val indices = c.zip(result.zipWithIndex.filter(_._1 == 'X').map(_._2))
        indices.foldLeft(result)((a, b) => a.updated(b._2, b._1)).mkString
      })
      addresses.map(BigInt(_, 2).toLong)
    }

    bitmask(cmd.addr).map(_ -> cmd.value)
  }

  def exec()(memFn: (Mem, String) => Seq[(Long, Long)]): Long = {

    @tailrec
    def _exec(xs: List[Cmd], mask: String, reg: Map[Long, Long]): Long = xs match {
      case Mask(m) :: t    => _exec(t, m, reg)
      case (mem: Mem) :: t => _exec(t, mask, reg ++ memFn(mem, mask).toMap)
      case _               => reg.values.sum
    }

    _exec(input, "", Map.empty)
  }

  val maskPattern: Regex = """mask = ([01X]*)""".r
  val memPattern: Regex  = """mem\[(\d+)] = (\d+)""".r
  def parse(s: String): Cmd = s match {
    case maskPattern(mask)       => Mask(mask)
    case memPattern(addr, value) => Mem(addr.toInt, value.toInt)
  }
  val input: List[Cmd] = Source.fromResource("day14.txt").getLines().map(parse).toList
  def partOne(): Long  = exec()(mem1)
  def partTwo(): Long  = exec()(mem2)

}
