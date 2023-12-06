package fr.sncf.itnovem.adventofcode2023

object Day06 extends AdventOfCode {
  override def fileName: String = "aoc06.txt"

  private case class Race(time: Long, duration: Long)

  private def computeNumberOfWinningStrategy(race: Race): Long = {
    (0 to race.time.toInt)
      .map(holdingTime => (race.time - holdingTime) * holdingTime)
      .count(_ > race.duration)
  }

  override def execute(): (Any, Any) = {

    val parsedInput1 = input.map(_.split(":")(1).split(" ").filter(_ != "").map(_.toLong))

    val star1 = parsedInput1.head.indices
      .map(i => Race(parsedInput1(0)(i), parsedInput1(1)(i)))
      .map(computeNumberOfWinningStrategy)
      .product

    val parsedInput2 = input.map(_.split(":")(1).replace(" ", "").toLong)

    val star2 = computeNumberOfWinningStrategy(Race(parsedInput2(0), parsedInput2(1)))
    (star1, star2)
  }

}
