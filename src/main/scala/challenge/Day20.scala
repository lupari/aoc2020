package challenge

import lib.Points.{Box, Point, Position}

import scala.collection.immutable
import scala.io.Source

object Day20 {

  type Grid[A] = Vector[Vector[A]]
  type Border  = Vector[Boolean]

  implicit class SymmetricMatrix[A](grid: Grid[A]) {
    def rotate: Grid[A] = grid.transpose.reverse
    def flip: Grid[A]   = grid.reverse
    def variations: Seq[Grid[A]] = Seq(
      grid,
      rotate,
      rotate.rotate,
      rotate.rotate.rotate,
      flip,
      flip.rotate,
      flip.rotate.rotate,
      flip.rotate.rotate.rotate,
    )
  }

  case class Tile(id: Int, img: Grid[Boolean]) {
    val borders: Map[Char, Border] =
      Map('L' -> img.map(_.head), 'R' -> img.map(_.last), 'T' -> img.head, 'B' -> img.last)
    def borderVariations(): Seq[Border] = borders.values.flatMap(b => Seq(b, b.reverse)).toSeq
    def variations: Seq[Tile]           = img.variations.map(img => copy(img = img))
    def inside: Grid[Boolean]           = img.tail.init.map(_.tail.init)
  }

  def tilesOfBorder(tiles: Seq[Tile]): Map[Border, Seq[Tile]] =
    tiles.flatMap(t => t.borderVariations().map(_ -> t)).groupMap(_._1)(_._2)

  def edgeTiles(borderTiles: Map[Border, Seq[Tile]]): Map[Tile, Set[Border]] = {
    val uniqueBorders =
      borderTiles
        .collect({
          case (border, Seq(tile)) => border -> tile
        }) // borders that can be fitted to a single tile. This will eliminate the inner tiles

    uniqueBorders.groupMapReduce(_._2)(b => Set(b._1))(_ | _) // map of tile to unique borders
  }

  def corners(borders: Map[Border, Seq[Tile]]): Seq[Tile] = {
    val edges = edgeTiles(borders)
    // corners have 2 unique borders (4 with those borders reversed)
    edges.filter(_._2.size == 2 * 2).keys.toSeq
  }

  def corners(tiles: Seq[Tile]): Seq[Tile] = corners(tilesOfBorder(tiles))

  def compose(tiles: Seq[Tile]): Grid[Tile] = {
    val borderTiles = tilesOfBorder(tiles)

    def composeRow(left: Tile): Vector[Tile] = {
      val border   = left.borders('R')
      val newTiles = borderTiles(border).toSet.filterNot(_.id == left.id)
      if (newTiles.size == 1) {
        val newTile         = newTiles.head
        val newTileOriented = newTile.variations.find(_.borders('L') == border).get
        composeRow1(newTileOriented)
      } else Vector.empty
    }

    def composeRow1(left: Tile): Vector[Tile] = left +: composeRow(left)

    def composeCol(top: Tile): Grid[Tile] = {
      val border   = top.borders('B')
      val newTiles = borderTiles(border).toSet.filterNot(_.id == top.id)
      if (newTiles.size == 1) {
        val newTile          = newTiles.head
        val newTileVariation = newTile.variations.find(_.borders('T') == border).get
        composeCol1(newTileVariation)
      } else Vector.empty
    }

    def composeCol1(top: Tile): Grid[Tile] = composeRow1(top) +: composeCol(top)

    val cornerTiles     = corners(borderTiles)
    val tlCorner        = cornerTiles.head
    val tlCornerBorders = edgeTiles(borderTiles)(tlCorner)
    val tlCornerVariation = tlCorner.variations
      .find(t =>
        tlCornerBorders.contains(t.borders('T')) && tlCornerBorders.contains(t.borders('L')))
      .get

    composeCol1(tlCornerVariation)
  }

  def findSeaMonsters(tiles: Seq[Tile]): Int = {
    val composition = compose(tiles)
    val img         = composition.map(_.map(_.inside)).flatMap(_.transpose.map(_.flatten))

    def points(img: Grid[Boolean]): Set[Point] =
      (for {
        (row, y) <- img.zipWithIndex
        (col, x) <- row.zipWithIndex
        if col // skip empty pixels
      } yield Point(x, y)).toSet

    val imgPoints = points(img)
    val imgSize   = Point(img.head.length, img.length)

    def findSeaMonster(seaMonsterImg: Grid[Boolean]): Option[Int] = {
      val monsterPoints = points(seaMonsterImg)
      val size          = Point(seaMonsterImg.head.length, seaMonsterImg.length)

      Box(Position.zero, imgSize - size - Point(1, 1)).iterator
        .map(p => monsterPoints.map(p + _))
        .filter(_.subsetOf(imgPoints))
        .reduceOption(_ ++ _)
        .map(mps => (imgPoints -- mps).size)
    }
    val seaMonster: String =
      """                  #.
        |#    ##    ##    ###
        | #  #  #  #  #  #   """.stripMargin
    val monsterImg = seaMonster.linesIterator.map(_.toVector).toVector.map(_.map(_ == '#'))
    monsterImg.variations.flatMap(findSeaMonster).head
  }

  def parseTile(s: List[String]): Tile = {
    val id  = s.head.dropWhile(!_.isDigit).init.toInt
    val img = s.tail.map(_.toList.map(_ == '#')).toVector
    Tile(id, img.map(_.toVector))
  }

  val tiles: List[Tile] =
    Source
      .fromResource("day20.txt")
      .mkString
      .split("\n\n")
      .map(_.linesIterator.toList)
      .map(parseTile)
      .toList

  def partOne(): Long = corners(tiles).map(_.id.toLong).product
  def partTwo(): Int  = findSeaMonsters(tiles)

}
