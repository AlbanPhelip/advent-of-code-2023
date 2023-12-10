package fr.sncf.itnovem.adventofcode2023

import scala.annotation.tailrec

object Day09 extends AdventOfCode {
  override def fileName: String = "aoc09.txt"

  @tailrec
  private def reduce(line: Array[Array[Int]]): Array[Array[Int]] = {
    val currentLine = line.last

    val result = currentLine
      .slice(0, currentLine.length - 1)
      .zip(currentLine.slice(1, currentLine.length))
      .map(x => x._2 - x._1)

    if (!result.exists(_ != 0)) {
      line :+ result
    } else {
      reduce(line :+ result)
    }
  }

  private def extrapolateByTheBack(line: Array[Array[Int]]): Int = {
    line.map(_.last).sum
  }

  private def extrapolateByTheFront(line: Array[Array[Int]]): Int = {
    line.map(_.head).zipWithIndex.map(x => x._1 * (1 - 2*(x._2 % 2))).sum
  }

  override def execute(): (Any, Any) = {
    val reducedInput = input
      .map(_.split(" ").map(_.toInt))
      .map(Array(_))
      .map(reduce)

    val star1 = reducedInput
      .map(extrapolateByTheBack)
      .sum

    val star2 = reducedInput
      .map(extrapolateByTheFront)
      .sum

    (star1, star2)
  }

}
