package fr.sncf.itnovem.adventofcode2023

object Day07 extends AdventOfCode {
  override def fileName: String = "aoc07.txt"

  private def handType(cards: String): String = {
    cards.toCharArray.groupBy(identity).values.map(_.length).toList.sorted match {
      case List(5)          => "1 - Five of a kind"
      case List(1, 4)       => "2 - Four of a kind"
      case List(2, 3)       => "3 - Full house"
      case List(1, 1, 3)    => "4 - Three of a kind"
      case List(1, 2, 2)    => "5 - Two pairs"
      case List(1, 1, 1, 2) => "6 - One pair"
      case _                => "7 - High card"
    }
  }

  private def replaceJackByJoker(cards: (String, String)): List[(String, String)] = {
    if (!cards._2.contains("J")) {
      List(cards)
    } else {
      cardValuesStar.filter(_._1 != 'J').toList
        .sortBy(_._2)
        .map(_._1)
        .map(c => cards._2.replaceFirst("J", c.toString))
        .flatMap(x => replaceJackByJoker((cards._1, x)))
    }
  }

  private val cardValuesStar = Map(
    'A' -> "A",
    'K' -> "B",
    'Q' -> "C",
    'J' -> "D",
    'T' -> "E",
    '9' -> "F",
    '8' -> "G",
    '7' -> "H",
    '6' -> "I",
    '5' -> "J",
    '4' -> "K",
    '3' -> "L",
    '2' -> "M"
  )

  private def handToCardValue(hand: String): String = {
    hand.toCharArray.map(cardValuesStar).mkString
  }

  private val cardValuesStar2 = Map(
    'A' -> "A",
    'K' -> "B",
    'Q' -> "C",
    'T' -> "E",
    '9' -> "F",
    '8' -> "G",
    '7' -> "H",
    '6' -> "I",
    '5' -> "J",
    '4' -> "K",
    '3' -> "L",
    '2' -> "M",
    'J' -> "N",
  )

  private def handToCardValue2(hand: String): String = {
    hand.toCharArray.map(cardValuesStar2).mkString
  }

  override def execute(): (Any, Any) = {

    val star1 = input
      .map(_.split(" "))
      .map(arr => (arr(0), arr(1).toLong))
      .groupBy(x => handType(x._1))
      .toList
      .sortBy(_._1)
      .reverse
      .flatMap(_._2.sortBy(x => handToCardValue(x._1)).reverse)
      .map(_._2)
      .zipWithIndex
      .map(x => x._1 * (x._2 + 1))
      .sum

    val star2 = input
      .map(_.split(" "))
      .map(arr => (arr(0), arr(1).toLong))
      .map(x => (replaceJackByJoker((x._1, x._1)), x._2))
      .map(x => x._1.map(y => (y, x._2)).minBy(x => handType(x._1._2)))
      .groupBy(x => handType(x._1._2))
      .toList
      .sortBy(_._1)
      .reverse
      .flatMap(_._2.sortBy(x => handToCardValue2(x._1._1)).reverse)
      .map(_._2)
      .zipWithIndex
      .map(x => x._1 * (x._2 + 1))
      .sum

    (star1, star2)
  }

}
