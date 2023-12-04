package fr.sncf.itnovem.adventofcode2023

object Day04 extends AdventOfCode {
  override def fileName: String = "aoc04.txt"

  private val inputSize: Int = input.length

  private val tuples: Map[Int, Array[Array[Int]]] = input
    .map(_.split(":"))
    .map(x =>
      (
        x(0).split(" ").filter(_ != "")(1).toInt,
        x(1).trim.split("\\|").map(_.trim.split(" ").filter(_ != "").map(_.toInt))
      )
    )
    .toMap

  private def isWinning(line: (Int, Array[Int], Array[Int])): Int = {
    val cardNumber = line._1
    val arr1 = line._2
    val arr2 = line._3
    val winningNumbers = arr1.intersect(arr2).length

    if (winningNumbers == 0) 1
    else {
      val upperBound = scala.math.min(cardNumber + winningNumbers, inputSize)
      (cardNumber + 1 to upperBound)
        .map(i => isWinning((i, tuples(i)(0), tuples(i)(1))))
        .sum + 1
    }
  }

  override def execute(): (Any, Any) = {

    val star1 = input
      .map(_.split(":")(1).trim.split("\\|").map(_.trim.split(" ").filter(_ != "").map(_.toInt)))
      .map { case Array(card1, card2) => card1.intersect(card2).length }
      .filter(_ != 0)
      .map(x => scala.math.pow(2, x - 1))
      .sum

    val star2 = (1 to inputSize)
      .map(i => isWinning((i, tuples(i)(0), tuples(i)(1))))
      .sum

    (star1, star2)
  }

}
