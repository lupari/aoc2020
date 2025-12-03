package challenge

import lib.GridImplicits.{Grid, GridInput}

import scala.io.Source

object Day17 {

  case class Cube(x: Int, y: Int, z: Int, q: Option[Int]) {
    def neighbors: Seq[Cube] = Cube.area((this, this), q.isDefined).diff(Seq(this))
  }
  object Cube {
    def area(bb: (Cube, Cube), hyper: Boolean): Seq[Cube] = {
      for {
        x <- (bb._1.x - 1) to (bb._2.x + 1)
        y <- (bb._1.y - 1) to (bb._2.y + 1)
        z <- (bb._1.z - 1) to (bb._2.z + 1)
        q <- if (hyper) (bb._1.q.get - 1) to (bb._2.q.get + 1) else 0 to 0
      } yield Cube(x, y, z, if (hyper) Some(q) else None)
    }
  }
  case class PocketDim(cubes: Map[Cube, Char]) {
    def hyper: Boolean   = cubes.head._1.q.isDefined
    def activeCount: Int = cubes.values.count(_ == '#')
    def bounds: (Cube, Cube) =
      (Cube(cubes.keys.minBy(_.x).x,
            cubes.keys.minBy(_.y).y,
            cubes.keys.minBy(_.z).z,
            if (hyper) Some(cubes.keys.minBy(_.q).q.get) else None),
       Cube(cubes.keys.maxBy(_.x).x,
            cubes.keys.maxBy(_.y).y,
            cubes.keys.maxBy(_.z).z,
            if (hyper) Some(cubes.keys.maxBy(_.q).q.get) else None))
  }

  def boot(pocketDim: PocketDim): Int = {

    def nextState(pd: PocketDim): PocketDim = {
      val expansion = Cube.area(pd.bounds, pd.hyper)
      val cubes = expansion.foldLeft(Map.empty[Cube, Char].withDefaultValue('.'))((acc, cube) => {
        cube.neighbors.count(pd.cubes(_) == '#') match {
          case 3                          => acc + (cube -> '#')
          case 2 if pd.cubes(cube) == '#' => acc + (cube -> '#')
          case _                          => acc + (cube -> '.')
        }
      })
      pd.copy(cubes = cubes)
    }

    (1 to 6).foldLeft(pocketDim)((a, _) => nextState(a)).activeCount
  }

  val input: Grid[Char] = Source.fromResource("day17.txt").mkString.toList.toGrid

  def partOne(): Int = {
    val pocketDim = PocketDim(
      input.map(kv => (Cube(kv._1.x, kv._1.y, 0, None), kv._2)).withDefaultValue('.'))
    boot(pocketDim)
  }
  def partTwo(): Int = {
    val pocketDim = PocketDim(
      input.map(kv => (Cube(kv._1.x, kv._1.y, 0, Some(0)), kv._2)).withDefaultValue('.'))
    boot(pocketDim)
  }

}
