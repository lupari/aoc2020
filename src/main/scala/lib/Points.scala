package lib

object Points {

  case class Point(x: Int, y: Int) {
    def +(p: Point): Point = Point(x + p.x, y + p.y)
    def *(n: Int): Point   = Point(x * n, y * n)
    def neighbors: List[Point] =
      List(Point(x, y - 1), Point(x + 1, y), Point(x, y + 1), Point(x - 1, y))
    def corners: List[Point] =
      List(Point(x - 1, y - 1), Point(x + 1, y - 1), Point(x - 1, y + 1), Point(x + 1, y + 1))
    def surroundings: List[Point] = neighbors ++ corners
    def rotate(deg: Int): Point = deg % 360 match {
      case 90 | -270  => Point(y, -x)
      case 180 | -180 => Point(-x, -y)
      case -90 | 270  => Point(-y, x)
      case _          => this
    }
    def manhattan(p: Point = Position.zero): Int = (p.x - x).abs + (p.y - y).abs
  }

  object Position {
    def zero: Point                  = Point(0, 0)
    def neighbors: List[Point]       = List(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))
    def corners: List[Point]         = List(Point(1, 1), Point(-1, 1), Point(1, -1), Point(-1, -1))
    def surroundings: List[Point]    = neighbors ++ corners
    def directions: Map[Char, Point] = List('E', 'W', 'N', 'S').zip(neighbors).toMap
  }

  case class Dir(p: Point, dir: Char) {
    def forward(n: Int = 1): Dir = dir match {
      case 'U' | 'N' => copy(p = p.copy(y = p.y - n))
      case 'D' | 'S' => copy(p = p.copy(y = p.y + n))
      case 'L' | 'W' => copy(p = p.copy(x = p.x - n))
      case 'R' | 'E' => copy(p = p.copy(x = p.x + n))
    }
    def rotate(clockwise: Boolean = true, n: Int = 0): Dir = dir match { // turn and move
      case 'U' => if (clockwise) Dir(Point(p.x + n, p.y), 'R') else Dir(Point(p.x - n, p.y), 'L')
      case 'D' => if (clockwise) Dir(Point(p.x - n, p.y), 'L') else Dir(Point(p.x + n, p.y), 'R')
      case 'L' => if (clockwise) Dir(Point(p.x, p.y - n), 'U') else Dir(Point(p.x, p.y + n), 'D')
      case 'R' => if (clockwise) Dir(Point(p.x, p.y + n), 'D') else Dir(Point(p.x, p.y - n), 'U')
    }
    def turn(d: Char): Dir = copy(dir = d)
    def turn(degrees: Int): Dir = {
      val dirs = Seq('U', 'R', 'D', 'L')
      val deg  = (degrees.abs / 90) % 360
      val i    = dirs.indexOf(dir) + (if (degrees < 0) dirs.length - deg else deg)
      copy(dir = dirs(i % dirs.length))
    }

  }

}
