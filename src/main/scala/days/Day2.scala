package days

object Day2 extends Day {
  val number: Int = 2;

  def checkFnP1(input: List[Int], order: Option[Boolean]): Boolean =
    input match {
      case Nil      => true
      case _ :: Nil => true
      case a :: b :: t => {
        val diff = a - b
        if (diff < -3 || diff > 3 || diff == 0) {
          return false
        }
        val curOrder = Some(diff > 0)
        val newOrder: Option[Boolean] = (order, curOrder) match {
          case (None, a)        => a
          case (a, b) if a == b => a
          case _                => return false
        }
        checkFnP1(b :: t, newOrder)
      }
    }

  def checkFnP2(
      input: List[Int]
  ): Boolean = {
    // Wanted to try a recursive approach like p1 but I gave up.. :(
    if (checkFnP1(input, None)) {
      return true
    }
    (0 until (input.length)).iterator
      .map { n =>
        {
          val (before, after) = input.splitAt(n)
          val newInput = before ++ after.tail
          checkFnP1(newInput, None)
        }
      }
      .dropWhile(_ == false)
      .nextOption
      .getOrElse(false)
  }

  def part1(input: String) =
    input.linesIterator
      .map(s => checkFnP1(s.split(" ").map(_.toInt).toList, None))
      .count(_ == true)
      .toString()

  def part2(input: String) =
    input.linesIterator
      .map(s => checkFnP2(s.split(" ").map(_.toInt).toList))
      .count(_ == true)
      .toString()
}
