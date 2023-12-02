package fr.sncf.itnovem.adventofcode2023

object Day02 extends AdventOfCode {
  override def fileName: String = "aoc02.txt"

  private def toGame(s: String): Int = s.split(" ")(1).toInt

  private def toDraw(s: String): Array[(Int, String)] = s
    .split("[,;]")
    .map(_.trim.split(" "))
    .map(x => (x.head.toInt, x(1)))

  private def isValid(draw: (Int, String)): Boolean = {
    draw._2 match {
      case "red"   => draw._1 <= 12
      case "green" => draw._1 <= 13
      case "blue"  => draw._1 <= 14
    }
  }

  override def execute(): (Any, Any) = {
    val star1 = input
      .map(_.split(":"))
      .map(arr => (toGame(arr.head), arr(1).split(";").map(toDraw(_).map(isValid)).map(_.reduce(_ & _)).reduce(_ & _)))
      .filter(_._2)
      .map(_._1)
      .sum

    val star2 = input
      .map(_.split(":")(1))
      .map(toDraw)
      .map(_.groupBy(_._2).map(_._2.map(_._1).max).product)
      .sum

    (star1, star2)
  }

}
