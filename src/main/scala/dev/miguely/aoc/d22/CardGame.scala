package dev.miguely.aoc.d22

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

object CardGame extends App {
  val linput = "input/d22.txt"
  val sinput = "input/small/d22.txt"
  val input = linput

  val player1 = new ArrayBuffer[Long]
  val player2 = new ArrayBuffer[Long]
  var readingDeck = player1
  for (line <- Source.fromFile(input).getLines())
    if (line == "") readingDeck = player2
    else if (!line.startsWith("Player")) {
      readingDeck.addOne(line.toLong)
    }

  println(
    calculateScore(
      getWinnerBasicCombat(
        new ArrayBuffer[Long].addAll(player1),
        new ArrayBuffer[Long].addAll(player2)
      )
    )
  )
  
  println(
    calculateScore(
      player1WinsRecursiveCombat(
        new ArrayBuffer[Long].addAll(player1),
        new ArrayBuffer[Long].addAll(player2)
      )._2
    )
  )

  def getWinnerBasicCombat(p1: ArrayBuffer[Long], p2: ArrayBuffer[Long]): ArrayBuffer[Long] = {
    while (p1.size > 0 && p2.size > 0)
      if (p1.head > p2.head) {
        move(p2, p1)
      } else {
        move(p1, p2)
      }

    if (p1.size == 0) {
      return p2
    } else {
      return p1
    }
  }

  def player1WinsRecursiveCombat(
      p1: ArrayBuffer[Long],
      p2: ArrayBuffer[Long]
  ): (Boolean, ArrayBuffer[Long]) = {
    val history1: ArrayBuffer[List[Long]] =
      new ArrayBuffer[List[Long]]()
    val history2: ArrayBuffer[List[Long]] =
      new ArrayBuffer[List[Long]]()
    while (p1.size > 0 && p2.size > 0) {
      val p1h = p1.toList
      val p2h = p2.toList
      // println(s"Initial p1 $p1")
      // println(s"Initial p2 $p2")
      if (history1.contains(p1h) && history2.contains(p2h)) {
        // println("-------Winning by history------")
        // println(history1.find(_ == p1h))
        // println(history2.find(_ == p2h))
        // println("-------Winning by history------")
        return (true, p1)
      }
      history1.addOne(p1h)
      history2.addOne(p2h)
      if (p1.size > p1.head && p2.size > p2.head) {
        if (
          player1WinsRecursiveCombat(
            p1.slice(1, (1 + p1.head).toInt),
            p2.slice(1, (1 + p2.head).toInt)
          )._1
        )
          move(p2, p1)
        else
          move(p1, p2)
      } else if (p1.head > p2.head)
        move(p2, p1)
      else
        move(p1, p2)
      // println(s"p1 $p1")
      // println(s"p2 $p2")
    }
    if (p1.size == 0) return (false, p2)
    else return (true, p1)
  }

  def move(loser: ArrayBuffer[Long], winner: ArrayBuffer[Long]) {
    winner += winner.head
    winner += loser.head
    loser.remove(0)
    winner.remove(0)
  }

  def calculateScore(winner: ArrayBuffer[Long]): BigInt = {
    var reverse = winner.reverse
    var result = BigInt(0)
    for (i <- 0 to reverse.size - 1)
      result += (i + 1) * reverse(i)
    result
  }

}
