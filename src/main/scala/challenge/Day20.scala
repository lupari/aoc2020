package challenge

import lib.Points.{Box, Point, Position}

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
    def borders: Map[Char, Border] =
      Map('L' -> img.map(_.head), 'R' -> img.map(_.last), 'T' -> img.head, 'B' -> img.last)
    def borderVariations: Seq[Border] = borders.values.flatMap(b => Seq(b, b.reverse)).toSeq
    def variations: Seq[Tile]         = img.variations.map(img => copy(img = img))
    def inside: Grid[Boolean]         = img.tail.init.map(_.tail.init)
  }

  def tilesOfBorder(tiles: Seq[Tile]): Map[Border, Seq[Tile]] =
    tiles.flatMap(t => t.borderVariations.map(_ -> t)).groupMap(_._1)(_._2)

  def edgeTiles(borderTiles: Map[Border, Seq[Tile]]): Map[Tile, Set[Border]] = {
    val uniqueBorders = // borders that can be fitted to a single tile. This will eliminate the inner tiles
      borderTiles.collect({ case (border, Seq(tile)) => border -> tile })
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

    def findTile(src: Tile, horizontally: Boolean): Option[Tile] = {
      val border = src.borders(if (horizontally) 'R' else 'B')
      val tiles  = borderTiles(border).toSet.filterNot(_.id == src.id)
      if (tiles.size == 1)
        tiles.head.variations.find(_.borders(if (horizontally) 'L' else 'T') == border)
      else None
    }

    def composeRow(left: Tile): Vector[Tile] =
      findTile(left, horizontally = true).map(composeRows).getOrElse(Vector.empty)

    def composeRows(left: Tile): Vector[Tile] = left +: composeRow(left)

    def composeColumn(top: Tile): Grid[Tile] =
      findTile(top, horizontally = false).map(composeImg).getOrElse(Vector.empty)

    def composeImg(top: Tile): Grid[Tile] = composeRows(top) +: composeColumn(top)

    val cornerTiles     = corners(borderTiles)
    val tlCorner        = cornerTiles.head
    val tlCornerBorders = edgeTiles(borderTiles)(tlCorner)
    val tlCornerVariant = tlCorner.variations
      .find(t =>
        tlCornerBorders.contains(t.borders('T')) && tlCornerBorders.contains(t.borders('L')))
      .get

    composeImg(tlCornerVariant)
  }

  def waterRoughness(tiles: Seq[Tile]): Int = {
    val composition = compose(tiles)
    val img         = composition.map(_.map(_.inside)).flatMap(_.transpose.map(_.flatten))

    def points(img: Grid[Boolean]): Set[Point] =
      (for {
        (row, y) <- img.zipWithIndex
        (col, x) <- row.zipWithIndex
        if col // skip empty pixels
      } yield Point(x, y)).toSet

    val imgPoints = points(img)
    val imgFrame  = Point(img.head.length, img.length)

    def countMonsters(img: Grid[Boolean]): Int = {
      val monsterPoints = points(img)
      val monsterFrame  = Point(img.head.length, img.length)

      Box(Position.zero, imgFrame - monsterFrame).iterator
        .map(p => monsterPoints.map(p + _))
        .filter(_.subsetOf(imgPoints))
        .reduceOption(_ ++ _)
        .map(_.size / monsterPoints.size)
        .getOrElse(0)
    }

    val seaMonster =
      // the extra tilde on top right is to prevent IDE from removing a trailing white space :D
      """                  #~
        |#    ##    ##    ###
        | #  #  #  #  #  #   """.stripMargin
    val monsterImg   = seaMonster.linesIterator.map(_.toVector).toVector.map(_.map(_ == '#'))
    val monsterCount = monsterImg.variations.map(countMonsters).find(_ > 0).get
    imgPoints.size - monsterCount * seaMonster.count(_ == '#')
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
  def partTwo(): Int  = waterRoughness(tiles)

}
