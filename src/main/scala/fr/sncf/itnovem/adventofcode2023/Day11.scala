package fr.sncf.itnovem.adventofcode2023

import scala.math.abs

object Day11 extends AdventOfCode {
  override def fileName: String = "aoc11.txt"

  case class Point(x: Int, y: Int)

  private def computeManhattanDistance(p1: Point, p2: Point): Int = {
    abs(p1.x - p2.x) + abs(p1.y - p2.y)
  }

  private val rowsWithoutHashtags: Array[Int] = input.zipWithIndex.filter(!_._1.contains("#")).map(_._2)

  private val columnsWithoutHashtags: Array[Int] = input
    .map(_.split(""))
    .transpose
    .zipWithIndex
    .filter(!_._1.contains("#"))
    .map(_._2)

  private def computeDistances(numberOfLine: Int) = {
    val galaxies = input
      .map(_.split(""))
      .zipWithIndex
      .flatMap { case (line, y) =>
        line.zipWithIndex.map { case (value, x) =>
          if (value == "#") {
            Some(Point(x, y))
          } else {
            None
          }
        }
      }
      .filter(_.isDefined)
      .map(_.get)

    galaxies
      .flatMap(point => galaxies.map((point, _)))
      .map { case (p1, p2) =>
        val additionalRows =
          rowsWithoutHashtags.filter(r => r > scala.math.min(p1.y, p2.y) && r < scala.math.max(p1.y, p2.y))
        val additionalColumns =
          columnsWithoutHashtags.filter(c => c > scala.math.min(p1.x, p2.x) && c < scala.math.max(p1.x, p2.x))
        val initialDistance = computeManhattanDistance(p1, p2)

        initialDistance + (numberOfLine - 1) * (additionalRows.length.toLong + additionalColumns.length.toLong)
      }
      .sum / 2
  }

  override def execute(): (Any, Any) = {

    val star1 = computeDistances(2)

    val star2 = computeDistances(1000000)

    (star1, star2)
  }

}
