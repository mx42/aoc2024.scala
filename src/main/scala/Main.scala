import days._

@main def main(dayNumber: Int): Unit = {
  dayNumber match {
    case 1 => Day1.solve()
    case 2 => Day2.solve()
    case 3 => Day3.solve()
    case 4 => Day4.solve()
    case 5 => Day5.solve()
    case _ => println(s"Day $dayNumber is not yet implemented.")
  }
}
