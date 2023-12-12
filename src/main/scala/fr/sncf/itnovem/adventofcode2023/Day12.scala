package fr.sncf.itnovem.adventofcode2023

object Day12 extends AdventOfCode {
  override def fileName: String = "aoc12.txt"

  private def generateCombinations(n: Int, k: Int): List[List[Int]] = {
    def generateCombinationsHelper(currentCombination: List[Int], remainingSquares: List[Int]): List[List[Int]] = {
      if (currentCombination.length == k) {
        List(currentCombination.reverse)
      } else {
        remainingSquares match {
          case Nil => List.empty
          case head :: tail =>
            generateCombinationsHelper(head :: currentCombination, tail) ++
              generateCombinationsHelper(currentCombination, tail)
        }
      }
    }

    generateCombinationsHelper(List.empty, (1 to n).toList)
  }

  private def deal(line: String, values: List[Int]) = {
    val arr           = line.toCharArray.zipWithIndex
    val questionMarks = arr.filter(_._1 == '?').map(_._2).zipWithIndex

    val numberOfQuestionMarks   = questionMarks.length
    val initialNumberOfHashtags = arr.count(_._1 == '#')
    val finalNumberOfHashtags   = values.sum

    generateCombinations(numberOfQuestionMarks, finalNumberOfHashtags - initialNumberOfHashtags)
      .map(_.map(_ - 1))
      .map { combination =>
        arr.map { case (char, index) =>
          if (char == '?') {
            if (combination.contains(questionMarks.filter(_._1 == index).head._2)) '#' else '.'
          } else {
            char
          }
        }.mkString
      }
      .map(_.split("\\.").filter(_.nonEmpty).map(_.length).toList)
      .count(_ == values)
  }

  override def execute(): (Any, Any) = {

    val star1 = input
      .map(_.split(" "))
      .map(x => (x(0), x(1).split(",").map(_.toInt).toList))
      .map(x => deal(x._1, x._2))
      .sum

    val star2 = input
      .map(_.split(" "))
      .map(x => ((1 to 5).map(_ => x(0)).mkString("?"), (1 to 5).flatMap(_ => x(1).split(",").map(_.toInt)).toList))
      .zipWithIndex
      .filter(_._2 <= 1)
      .map(_._1)
    //.map(x => deal(x._1, x._2))

    (star1, star2)
  }

}
