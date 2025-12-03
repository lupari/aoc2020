package challenge

import scala.annotation.tailrec
import scala.io.Source

object Day24 {

  type Floor = Map[Tile, Boolean]
  case class Tile(x: Int, y: Int, z: Int) {
    def +(other: Tile): Tile = Tile(other.x + x, other.y + y, other.z + z)
    def neighbors: Seq[Tile] = Tile.adj.map(this + _)
  }
  object Tile {
    def parse(s: String): Option[Tile] =
      Seq("nw", "w", "sw", "se", "e", "ne").zip(adj).find(_._1 == s).map(_._2)
    def adj: Seq[Tile] = Seq(nw, w, sw, se, e, ne)
    def zero: Tile     = Tile(0, 0, 0)
    def nw: Tile       = Tile(0, 1, -1)
    def w: Tile        = Tile(-1, 1, 0)
    def sw: Tile       = Tile(-1, 0, 1)
    def se: Tile       = Tile(0, -1, 1)
    def e: Tile        = Tile(1, -1, 0)
    def ne: Tile       = Tile(1, 0, -1)
  }

  def init(tiles: List[Tile]): Floor = tiles.map(t => (t, tiles.count(_ == t) % 2 == 1)).toMap

  def evolve(floor: Floor): Floor = {

    def isBlack(tile: Tile, f: Floor): Boolean = {
      val blackNeighbors = tile.neighbors.count(f(_))
      blackNeighbors == 2 || f(tile) && blackNeighbors == 1
    }

    val expansion = floor.keys.flatMap(_.neighbors).filterNot(floor.contains).map(_ -> false)
    (floor ++ expansion).map(kv => (kv._1, isBlack(kv._1, floor))).withDefaultValue(false)
  }

  def parse(s: String): Tile = {

    @tailrec
    def helper(xs: List[Char], curr: Tile): Tile = xs match {
      case Nil      => curr
      case h :: Nil => curr + Tile.parse(h.toString).get
      case h :: i :: t =>
        Tile.parse(List(h, i).mkString) match {
          case None       => helper(i +: t, curr + Tile.parse(h.toString).get)
          case Some(tile) => helper(t, curr + tile)
        }
    }
    helper(s.toList, Tile.zero)
  }

  val input: List[Tile] = Source.fromResource("day24.txt").getLines().map(parse).toList
  val floor: Floor      = init(input).withDefaultValue(false)

  def partOne(): Int = floor.values.count(_ == true)
  def partTwo(): Int = Iterator.iterate(floor)(evolve).drop(100).next().values.count(_ == true)

}
