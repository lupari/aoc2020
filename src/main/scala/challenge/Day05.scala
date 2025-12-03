package challenge

import scala.io.Source

object Day05 {

  case class Seat(row: Int, col: Int) {
    def id: Int = row * 8 + col
  }

  def parse(s: String): Seat = {
    def _parse(c: Char, lo: Int, hi: Int): (Int, Int) = c match {
      case 'F' | 'L' => (lo, lo + (hi - lo) / 2)
      case 'B' | 'R' => (lo + (hi - lo) / 2 + 1, hi)
    }
    val row = s.takeWhile("FB".contains(_)).foldLeft(0, 127)((r, c) => _parse(c, r._1, r._2))
    val col = s.dropWhile("FB".contains(_)).foldLeft(0, 7)((r, c) => _parse(c, r._1, r._2))
    Seat(row._1, col._1)
  }

  val boardingPasses: List[String] = Source.fromResource("day05.txt").getLines().toList

  def partOne(): Int = boardingPasses.map(parse).map(_.id).max

  def partTwo(): Int = {
    val occupied    = boardingPasses.map(parse)
    val vacantIds   = (for (r <- 0 to 127; c <- 0 to 7) yield Seat(r, c)).diff(occupied).map(_.id)
    val occupiedIds = occupied.map(_.id)
    vacantIds.find(vid => Seq(vid - 1, vid + 1).forall(occupiedIds.contains)).get
  }

}
