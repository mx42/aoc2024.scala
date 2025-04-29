package days

import scala.io.Source

trait Day {
  val number: Int

  def getInput(): Option[String] = {
    val resourcePath = getClass.getResource(s"day${number}.txt")
    if (resourcePath != null) {
      Some(Source.fromURL(resourcePath).mkString)
    } else {
      None
    }
  }

  def solve(): Unit = {
    getInput() match {
      case Some(input) => {
        println(s"Day: $number")
        println(s"Part 1: ${part1(input)}")
        println(s"Part 2: ${part2(input)}")
      }
      case None =>
        println(s"Missing day $number input")
    }
  }

  def part1(input: String): String
  def part2(input: String): String
}
