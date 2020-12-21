package dev.miguely.aoc.d10

import scala.io.Source
import scala.collection.mutable.HashMap

object JoltageAdapter extends App {
  val adapters =
    (for (line <- Source.fromFile("input/d10.txt").getLines()) yield line.toLong).toList.sorted
  val map = new HashMap[Long, Long]()
  var currentJoltage: Long = 0
  var diff1: Long = 0
  var diff3: Long = 1
  for (j <- 1 to 3)
    map.put(j, 1)
  for (i <- 0 to adapters.size - 1) {
    val joltage = adapters(i)
    joltage - currentJoltage match {
      case 1 =>
        diff1 = diff1 + 1
      case 2 => ()
      case 3 => diff3 = diff3 + 1
      case _ =>
        ()
    }
    var count: Long = if (i < 4) {1} else {0}
    for (j <- joltage - 3 to joltage - 1)
      count = count + (map.get(j) match {
        case None         => 0
        case Some(c: Long) => c
      })
    map.put(joltage, count)
    currentJoltage = joltage
  }

  println(diff1 * diff3)
  println(map(adapters.last))

}
