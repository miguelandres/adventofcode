package dev.miguely.aoc.d10

import scala.io.Source
import scala.collection.mutable.HashMap

object JoltageAdapter extends App {

  val linput = "input/d10.txt"
  val sinput = "input/small/d10.txt"
  val input = linput

  val adapters =
    (for (line <- Source.fromFile(input).getLines()) yield line.toInt).toList.sorted
  val adapterSet = adapters.toSet
  val map = new HashMap[Int, BigInt]()
  var currentJoltage: Int = 0
  var diff1: Int = 0
  var diff3: Int = 1
  for (j <- 1 to 3)
    if (adapterSet.contains(j))
      map.put(j, BigInt(1))
  println(map)
  for (i <- 0 to adapters.size - 1) {
    val joltage = adapters(i)
    joltage - currentJoltage match {
      case 1 => diff1 = diff1 + 1
      case 2 => ()
      case 3 => diff3 = diff3 + 1
      case _ => ()
    }

    var count = map.getOrElse(joltage, BigInt(0))
    for (j <- joltage - 3 to joltage - 1) {
      count = map.get(j) match {
        case None    => count
        case Some(x) => x + count
      }
      println(j)
    }
    map.put(joltage, count)
    currentJoltage = joltage
  }

  println(map)
  println(diff1 * diff3)
  println(map(adapters.last))

}
