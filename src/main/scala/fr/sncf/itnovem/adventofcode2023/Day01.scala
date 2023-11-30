package fr.sncf.itnovem.adventofcode2023

object Day01 extends AdventOfCode {
  override def fileName: String = "aoc01.txt"

  override def execute(): (Any, Any) = {
    val star1 = input.map(_.toInt).sum
    val star2 = input.map(_.toInt).sum

    (star1, star2)
  }

}
