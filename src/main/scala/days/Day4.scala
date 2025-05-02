package days

case class Pos(x: Int, y: Int) {
  def upLeft: Pos = Pos(x - 1, y - 1)
  def up: Pos = Pos(x, y - 1)
  def upRight: Pos = Pos(x + 1, y - 1)
  def left: Pos = Pos(x - 1, y)
  def right: Pos = Pos(x + 1, y)
  def downLeft: Pos = Pos(x - 1, y + 1)
  def down: Pos = Pos(x, y + 1)
  def downRight: Pos = Pos(x + 1, y + 1)

  def cross: List[Pos] = List(
    this.upLeft,
    this.downRight,
    this.upRight,
    this.downLeft
  )

  def inThreshold(geo: Pos): Boolean =
    x >= 0 && x < geo.x && y >= 0 && y < geo.y
}

object Pos {
  def getNeighbors(): List[Pos => Pos] = List(
    _.upLeft,
    _.up,
    _.upRight,
    _.left,
    _.right,
    _.downLeft,
    _.down,
    _.downRight
  )
}

object Day4 extends Day {
  val number: Int = 4;

  def getPositions(p: Pos, geo: Pos, size: Int): List[List[Pos]] =
    // Get neighbors positions around a position (if matching the geometry)
    Pos
      // For each neighbor direction (up-left, up, up-right, etc)
      .getNeighbors()
      .map(f =>
        Iterator
          // Build an iterator going from [P] to the direction's neighbor [P]
          .iterate(p)(f)
          // Do it [size] time
          .take(size)
          // Drop positions getting outside the designed dimensions
          .filter(_.inThreshold(geo))
          .toList
      )
      // Only take the list of positions exactly matching the expected length
      .filter(_.length == size)

  def getCross(p: Pos, geo: Pos): Option[List[Pos]] =
    // Get surrounding cross around a position (if matching the geometry)
    p.cross.filter(_.inThreshold(geo)) match {
      case l if l.length == 4 => Some(l)
      case _                  => None
    }

  def getString(lines: List[String])(ps: List[Pos]): String =
    // Convert list of positions to matching letters
    ps.map {
      case Pos(x, y) => {
        lines(y)(x)
      }
    }.mkString

  def isCrossedMAS(s: String): Boolean =
    // Verify if a string is a crossed MAS
    s.length == 4
      && s.forall("MS".contains)
      && s(0) != s(1)
      && s(2) != s(3)

  // Verify if a string is a XMAS
  def isXMAS: String => Boolean = _ == "XMAS"

  def part1(input: String) = {
    val lines: List[String] = input.linesIterator.toList
    val geom: Pos = Pos(lines(0).length, lines.length)
    lines.zipWithIndex
      .flatMap { case (s, y) =>
        s.iterator.zipWithIndex.toList
          .filter(_._1 == 'X')
          .flatMap { case (_, x) =>
            getPositions(Pos(x, y), geom, 4)
              .map(getString(lines))
              .filter(isXMAS)
          }
      }
      .length
      .toString
  }

  def part2(input: String) = {
    val lines: List[String] = input.linesIterator.toList
    val geom: Pos = Pos(lines(0).length, lines.length)
    lines.zipWithIndex
      .flatMap { case (s, y) =>
        s.iterator.zipWithIndex.toList
          .filter(_._1 == 'A')
          .flatMap { case (_, x) =>
            getCross(Pos(x, y), geom)
              .map(getString(lines))
              .filter(isCrossedMAS)
          }
      }
      .length
      .toString
  }
}
