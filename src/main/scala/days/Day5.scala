package days

object Day5 extends Day {
  val number: Int = 5;

  def parseInput(input: String): (Map[Int, List[Int]], List[List[Int]]) = {
    val splitted = input.split("\n\n")

    // x|y\nx2|y => Map(y -> List(x, x2))
    val orderConstraints: Map[Int, List[Int]] = splitted(0).linesIterator
      .map(
        _.split('|')
          .map(_.filter(_ != '|').toInt)
          .toList
      )
      .foldLeft(Map.empty: Map[Int, List[Int]]) {
        case (acc, (h :: h2 :: _)) =>
          acc.updatedWith(h) {
            case Some(existing) => Some(h2 :: existing)
            case None           => Some(List(h2))
          }
        case (acc, _) => acc
      }
    // x,y,z\na,b,c,d => [[x,y,z], [a,b,c,d]]
    val batches = splitted(1).linesIterator
      .map(_.split(",").map(_.toInt).toList)
      .toList
    (orderConstraints, batches)
  }

  def checkBatch(order: Map[Int, List[Int]])(xs: List[Int]): Boolean =
    xs.foldLeft(
      // Is still OK + List of predecessors
      (true, List()): (Boolean, List[Int])
    ) {
      case ((true, prev), x) => {
        // Check if predecessors include some page that should actually be after the current
        (order(x).forall({ o => !prev.contains(o) }), x :: prev)
      }
      // If flag is already false, let's keep it that way
      case ((false, _), _) => (false, List())
    }._1

  def fixBatch(order: Map[Int, List[Int]])(xs: List[Int]): List[Int] =
    // For each number of the batch
    xs
      // Get the number of relevant order rules
      .map(x => x -> order(x).filter(xs.contains))
      // Order by the number of previous pages (incidentally all rules are here)
      .sortBy(_._2.length)
      // Keep just the page number and voilà
      .map(_._1)

  def takeMiddleOne(xs: List[Int]): Int =
    xs(xs.length / 2)

  def part1(input: String) = {
    val (order, batches) = parseInput(input)
    batches
      .filter(checkBatch(order))
      .map(takeMiddleOne)
      .sum()
      .toString
  }

  def part2(input: String) = {
    val (order, batches) = parseInput(input)
    batches
      .filterNot(checkBatch(order))
      .map(fixBatch(order))
      .map(takeMiddleOne)
      .sum()
      .toString
  }
}
