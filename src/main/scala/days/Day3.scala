package days

import scala.util.matching.Regex

object Day3 extends Day {
  val number: Int = 3;

  def part1(input: String) =
    "mul\\(([0-9]+),([0-9]+)\\)".r
      .findAllMatchIn(input)
      .map(m => m.group(1).toInt * m.group(2).toInt)
      .sum()
      .toString()

  def part2(input: String) =
    "(do)\\(\\)|(don't)\\(\\)|mul\\(([0-9]+),([0-9]+)\\)".r
      .findAllMatchIn(input)
      .foldLeft((true, 0)) {
        // Activate flag if "do()" matched
        case ((_, acc), m) if m.group(1) != null => (true, acc)
        // Deactivate flag if "dont() matched"
        case ((_, acc), m) if m.group(2) != null => (false, acc)
        // Ignore mul() if deactivated
        case ((false, acc), _) => (false, acc)
        // Add mul() result to the accumulator
        case ((true, acc), m) =>
          (true, acc + m.group(3).toInt * m.group(4).toInt)
      }
      ._2
      .toString()

}
