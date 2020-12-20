package dev.miguely.aoc.d3

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Slopes extends App {
  var map = new ArrayBuffer[String]()
  Source.fromFile("input/d3.txt").getLines().foreach(map += _)

  val directions = Array[(Int, Int)]((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

  var mult: Long = 1
  for ((dj, di) <- directions) {
    var j = 0
    var i = 0
    var count = 0
    while (i < map.size) {
      if (map(i)(j) == '#') count = count + 1
      j += dj
      j %= map(i).size
      i += di
    }
    mult = mult * count
  }
  println(mult)
}
