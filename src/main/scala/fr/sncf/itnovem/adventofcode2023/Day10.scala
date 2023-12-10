package fr.sncf.itnovem.adventofcode2023

import scala.util.Try

object Day10 extends AdventOfCode {
  override def fileName: String = "aoc10.txt"

  private case class Point(x: Int, y: Int) {
    def add(point: Point): Point = {
      Point(x + point.x, y + point.y)
    }
  }

  private val startingPosition: Point = {
    val startingLine   = input.indexWhere(_.contains("S"))
    val startingColumn = input(startingLine).indexOf("S")

    Point(startingColumn, startingLine)
  }

  private def getNextPipe(point: Point, previousPoint: Point, value: String): Point = {
    val vector = value match {
      case "|" => (Point(0, -1), Point(0, 1))
      case "-" => (Point(1, 0), Point(-1, 0))
      case "L" => (Point(0, -1), Point(1, 0))
      case "J" => (Point(-1, 0), Point(0, -1))
      case "7" => (Point(0, 1), Point(-1, 0))
      case "F" => (Point(1, 0), Point(0, 1))
      case "S" => (Point(0, 0), Point(0, 0))
      case "." => (Point(0, 0), Point(0, 0))
      case "X" => (Point(0, 0), Point(0, 0))
    }

    if (point.add(vector._1) == previousPoint) {
      point.add(vector._2)
    } else {
      point.add(vector._1)
    }
  }

  private val grid: Array[Array[String]] = input.map(_.split(""))

  private val xMax = grid.head.length
  private val yMax = grid.length

  private def findSecondPosition(point: Point): Point = {
    val top    = point.add(Point(0, -1))
    val right  = point.add(Point(1, 0))
    val bottom = point.add(Point(0, 1))
    val left   = point.add(Point(-1, 0))

    val topValue   = Try(grid(top.y)(top.x)).toOption.getOrElse(".")
    val rightValue = Try(grid(right.y)(right.x)).toOption.getOrElse(".")
    val leftValue  = grid(left.y)(left.x)

    if (Array("|", "7", "F").contains(topValue)) {
      top
    } else if (Array("-", "7", "J").contains(rightValue)) {
      right
    } else if (Array("-", "L", "F").contains(leftValue)) {
      left
    } else {
      bottom
    }
  }

  private def isInside(point: Point, loopPoints: List[Point]): Boolean = {
    if (loopPoints.contains(point)) {
      false
    } else {
      var crossCount = 0
      for (i <- 0 to scala.math.min(xMax - point.x - 1 , yMax - point.y -1 )) {
        val currentPoint = Point(point.x + i, point.y + i)
        val currentValue1 = grid(currentPoint.y)(currentPoint.x)
        val currentValue  = if (currentValue1 == "S") "J" else currentValue1
        if (loopPoints.contains(currentPoint) && currentValue != "7" && currentValue != "L") {
          crossCount += 1
        }
      }

      crossCount % 2 == 1
    }
  }

  override def execute(): (Any, Any) = {

    var previousPosition = startingPosition
    var currentPosition  = findSecondPosition(startingPosition)
    var currentPipe      = grid(currentPosition.y)(currentPosition.x)

    var counter = 0
    var loopPoints: List[Point] = List(startingPosition, currentPosition)

    while (currentPipe != "S" | counter == 0) {
      val nextPosition = getNextPipe(currentPosition, previousPosition, currentPipe)

      loopPoints = loopPoints :+ currentPosition
      currentPipe = grid(nextPosition.y)(nextPosition.x)
      previousPosition = currentPosition
      currentPosition = nextPosition
      counter += 1
    }

    val star1 = scala.math.ceil(counter.toDouble / 2).toInt


    var inside = 0
    for (y <- grid.indices) {
      for (x <- grid.head.indices) {
        if (isInside(Point(x, y), loopPoints)) {
          inside += 1
        }
      }
    }

    val star2 = inside

    (star1, star2)
  }

}
