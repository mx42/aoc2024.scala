package days

object Day1 extends Day {
  val number: Int = 1;

  def inputToLists(input: String): (List[Int], List[Int]) =
    input.linesIterator
      .map(l => {
        val line = l.split(" ")
        (line(0).toInt, line.last.toInt)
      })
      .foldLeft((Nil, Nil): Tuple2[List[Int], List[Int]]) {
        case ((accL, accR), (l, r)) => (l :: accL, r :: accR)
      }

  def part1(input: String) = {
    val (leftList, rightList) = inputToLists(input)
    leftList
      .sorted()
      .zip(rightList.sorted())
      .map((a, b) => (a - b).abs)
      .sum
      .toString()
  }

  def part2(input: String) = {
    val (leftList, rightList) = inputToLists(input)
    val counts =
      rightList.sorted
        .groupBy((n: Int) => n)
        .mapValues(_.length)

    leftList.map(n => counts.getOrElse(n, 0) * n).sum().toString()
  }
}
