package fr.sncf.itnovem.adventofcode2023

import scala.util.Try

object Day01 extends AdventOfCode {
  override def fileName: String = "aoc01.txt"

  override def execute(): (Any, Any) = {
    val star1 = input
      .map(_.map(char => Try(char.toString.toInt)).filter(_.isSuccess).map(_.get))
      .map(list => 10 * list.head + list.last)
      .sum

    val dict = Map(
      "one" -> 1,
      "two" -> 2,
      "three" -> 3,
      "four" -> 4,
      "five" -> 5,
      "six" -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine" -> 9,
      "1" -> 1,
      "2" -> 2,
      "3" -> 3,
      "4" -> 4,
      "5" -> 5,
      "6" -> 6,
      "7" -> 7,
      "8" -> 8,
      "9" -> 9,
    )

    val star2 = input.map(line => {
      val calibrationValues = dict
        .keys.toArray
        .flatMap(d => Array((d, line.indexOf(d)), (d, line.lastIndexOf(d))))
        .filter(_._2 != -1)
        .sortBy(_._2)
        .map(_._1)

      10 * dict(calibrationValues.head) + dict(calibrationValues.last)
    }).sum

    (star1, star2)
  }

}
