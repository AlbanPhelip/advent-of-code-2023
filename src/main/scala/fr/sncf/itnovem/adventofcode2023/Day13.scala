package fr.sncf.itnovem.adventofcode2023

object Day13 extends AdventOfCode {
  override def fileName: String = "aoc13.txt"

  override val input: Array[String] = Array(
    "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",
    "..#.##.#.",
    "..##..##.",
    "#.#.##.#.",
    "",
    "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#"
  )

  private def compute(input: Array[String]): Int = {
    val index = input
      .slice(0, input.length - 1)
      .zip(input.slice(1, input.length))
      .zipWithIndex
      .filter(x => x._1._1 == x._1._2)
      .lastOption.map(_._2)

    index match {
      case Some(i) =>
        var count    = 0
        var finished = false

        while (!finished) {
          if (i - count < 0 || i + count + 1 >= input.length) {
            finished = true
          }
          else if (input(i - count) == input(i + count + 1)) {
            count += 1
          } else {
            finished = true
          }
        }

        i - count + 1

      case None => 0
    }
  }

  private def computeSquare(input: Array[String]): Int = {
    100 * compute(input) + compute(input.map(_.toCharArray).transpose.map(_.mkString))
  }

  override def execute(): (Any, Any) = {

    val data = input
      .mkString(",")
      .split(",,")
      .map(_.split(","))

    val star1 = data.map(computeSquare).sum

    val star2 = 0

    (star1, star2)
  }

}
