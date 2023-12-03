package fr.sncf.itnovem.adventofcode2023

import scala.util.Try

object Day03 extends AdventOfCode {
  override def fileName: String = "aoc03.txt"

  private val symbols = Array('=', '-', '*', '/', '+', '$', '&', '%', '#', '@')
  private val lineSize = input.head.length

  private def slidingCheck(x: Int, slidingSize: Int): Array[Int] = {
    val upperLine = if (x == 0) None else Some(input(x - 1))
    val currentLine = input(x)
    val lowerLine = if (x == input.length - 1) None else Some(input(x + 1))

    (0 to lineSize - slidingSize) .map { index =>

      val upperChars: List[Char] = upperLine.map(_.slice(index - 1, index + slidingSize + 1).toList).getOrElse(List())
      val lowerChars: List[Char] = lowerLine.map(_.slice(index - 1, index + slidingSize + 1).toList).getOrElse(List())
      val leftChar = Try(currentLine(index - 1))
      val currentValue = currentLine.slice(index, index + slidingSize)
      val rightChar = Try(currentLine(index + slidingSize))
      val leftAndRightChars: List[Char] = List(leftChar, rightChar).filter(_.isSuccess).map(_.get)
      val adjacentChars: List[Char] = upperChars ++ lowerChars ++ leftAndRightChars

      if (Try(currentValue.toInt).isSuccess && leftChar.map(!_.isDigit).getOrElse(true) && rightChar.map(!_.isDigit).getOrElse(true)) {
        if(adjacentChars.map(symbols.contains(_)).reduce(_ || _)) currentValue.toInt else 0
      } else 0
    }.toArray
  }

  override def execute(): (Any, Any) = {

    val star1 = (1 to 3).map(slidingSize => input.indices.flatMap(slidingCheck(_, slidingSize)).sum).sum

    val star2 = 0

    (star1, star2)
  }

}
