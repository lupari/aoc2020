package challenge

import scala.collection.mutable
import scala.io.Source

object Day20 {

  case class Grid(v: Vector[String]) {

    def flips(grid: Grid): GridList = Vector(Grid(grid.v.reverse), Grid(grid.v.map(_.reverse)))

    def rotations(): GridList = {
      def rotate90(l: Grid): Grid = Grid(l.v.transpose.map(_.reverse).map(_.mkString))

      (1 to 3).foldLeft(Vector(rotate90(this)))((a, _) => a :+ rotate90(a.last))
    }

    def variations(): Set[Grid] = {
      val rotated: GridList = this +: rotations()
      val flipped: GridList = rotated.flatMap(flips)
      (rotated ++ flipped).toSet
    }

    def divide(): GridList = {
      val n: Int = if (v.size % 2 == 0) 2 else 3
      v.grouped(n).map(g => g.map(s => s.grouped(n).toList).transpose).toVector.flatten.map(Grid)
    }

    def count(): Int = v.map(_.count(_ == '#')).sum

  }

  type GridList = Vector[Grid]

  def join(grids: GridList): Grid = {
    val gs: Int = math.sqrt(grids.length.toDouble).toInt
    gs match {
      case 1 => grids.head
      case _ =>
        val ts: Int = grids.head.v.length
        val v = grids
          .grouped(gs)
          .flatMap(g => (0 until ts).map(i => g.indices.map(j => g(j).v(i)).mkString))
          .toVector
        Grid(v)
    }
  }

  // memoization grid -> rule
  val memo: mutable.Map[Grid, Grid] = mutable.Map()

  def transform(grid: Grid, rules: Map[Grid, Grid]): Grid = {
    if (memo.contains(grid)) memo(grid)
    else {
      val rule: Grid     = rules.keys.find(k => grid.variations().contains(k)).get
      val transformation = rules(rule)
      memo(grid) = transformation
      transformation
    }
  }

  def parse(line: String): (Grid, Grid) = {
    val parts = line.split(" => ")
    val rule  = List(parts.head, parts.last).map(_.split("/").toVector.map(_.mkString))
    (Grid(rule.head), Grid(rule.last))
  }

  def execute(limit: Int): Int = {
    val input: List[String]              = Source.fromResource("day21.txt").getLines.toList
    val transformations: Map[Grid, Grid] = input.map(parse).toMap
    val result: Grid = (1 to limit).foldLeft(Grid(Vector(".#.", "..#", "###")))((a, _) => {
      val replacements: GridList = a.divide().map(transform(_, transformations))
      join(replacements)
    })
    result.count()
  }

  def runOne(): Int = execute(5)

}
