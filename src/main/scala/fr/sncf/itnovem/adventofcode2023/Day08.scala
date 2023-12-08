package fr.sncf.itnovem.adventofcode2023

import scala.annotation.tailrec

object Day08 extends AdventOfCode {
  override def fileName: String = "aoc08.txt"

  override def execute(): (Any, Any) = {

    val instructions = input.head.toCharArray
    val elements = input.tail.tail
      .map(_.split(" = "))
      .map(x => (x(0), x(1).replace("(", "").replace(")", "").split(", ")))
      .map(x => (x._1, (x._2(0), x._2(1))))
      .toMap

    var i   = 0
    var acc = "AAA"
    while (acc != "ZZZ") {
      instructions(i % instructions.length) match {
        case 'L' => acc = elements(acc)._1
        case 'R' => acc = elements(acc)._2
      }
      i += 1
    }

    val star1 = i

    def getStep(start: String): Int = {
      var i = 0
      var acc = start
      while (!acc.endsWith("Z")) {
        instructions(i % instructions.length) match {
          case 'L' => acc = elements(acc)._1
          case 'R' => acc = elements(acc)._2
        }
        i += 1
      }
      i
    }

    @tailrec
    def gcd(x: Long, y: Long): Long = if (y == 0) x else gcd(y, x % y)

    def findSmallestValues(a: Long, b: Long): Long = {
      a * (b / gcd(a, b))
    }

    val star2 = elements
      .filter(_._1.endsWith("A"))
      .keys
      .map(getStep)
      .map(_.toLong)
      .reduce(findSmallestValues)

    (star1, star2)
  }

}