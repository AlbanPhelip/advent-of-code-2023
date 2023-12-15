package fr.sncf.itnovem.adventofcode2023

object Day15 extends AdventOfCode {
  override def fileName: String = "aoc15.txt"

  //override val input: Array[String] = Array("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

  private def computeHash(input: String): Int = {
    input.toCharArray
      .map(_.toInt)
      .foldLeft(0)((acc, x) => ((acc + x) * 17) % 256)
  }

  override def execute(): (Any, Any) = {

    val star1 = input.head
      .split(",")
      .map(computeHash)
      .sum

    val star2 = 0

    (star1, star2)
  }

}
