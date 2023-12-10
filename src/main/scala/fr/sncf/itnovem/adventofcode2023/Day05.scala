package fr.sncf.itnovem.adventofcode2023

object Day05 extends AdventOfCode {
  override def fileName: String = "aoc05.txt"

  override val input: Array[String] = Array(
    "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4"
  )

  case class Instruction(destination: Long, source: Long, range: Long)

  val seeds = input.head.split(": ")(1).split(" ").map(_.toLong)

  val instructions = input.tail.tail
    .mkString(",")
    .split(",,")
    .map(_.split(","))
    .map(_.tail)
    .map(_.map(_.split(" ").map(_.toLong)).map(x => {
      Instruction(x(0), x(1), x(2))
    }))
  .map(_.mkString(","))
  .foreach(println)

  //val maps = instructions.map(x => x)

  override def execute(): (Any, Any) = {
    val star1 = 0

    val star2 = 0

    (star1, star2)
  }

}
