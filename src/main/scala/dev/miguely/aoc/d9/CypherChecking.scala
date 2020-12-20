package dev.miguely.aoc.d9

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}
import scala.io.Source

object CypherChecking extends App {
  var preambleSize = 25
  var array = new ArrayBuffer[Long]()
  var map = new HashMap[Long, Int]()
  var fullList = new ArrayBuffer[Long]()
  var firstInvalid: Long = 0

  breakable {
    Source.fromFile("input/d9.txt").getLines().foreach { line =>
      var num = line.toLong
      fullList += num
      if (array.size == preambleSize) {
        if (!checkPreamble(num)) {
          firstInvalid = num
          break
        }
        removeFirst()
      }
      addNumber(num)
    }
  }
  println(firstInvalid)

  breakable {
    for (i <- 0 to fullList.size - 2) {
      var sum: Long = fullList(i)
      var subList = new ArrayBuffer[Long]()
      subList += sum
      var end: Option[Int] = None
      breakable {
        for (j <- (i + 1) to (fullList.size - 1)) {
          sum = sum + fullList(j)
          subList += fullList(j)
          if (sum == firstInvalid) {
            end = Some(j)
            break
          }
        }
      }
      end match {
        case None => ()
        case Some(endIndex) =>
          var sorted = subList.sortInPlace()
          println(sorted)
          var result: Long = sorted(0) + sorted(sorted.size - 1)
          println(result)
          break
      }
    }
  }

  def checkPreamble(num: Long): Boolean = {
    for (i <- array)
      if (map.getOrElse(num - i, 0) > 0 && num - i != i) return true
    return false
  }
  def addNumber(num: Long) = {
    array += num
    map.put(num, 1 + map.getOrElse(num, 0))
  }

  def removeFirst() = {
    var num = array(0)
    array.remove(0)
    map.getOrElse(num, 1) match {
      case n if n == 1 => map.remove(num)
      case n if n != 1 => map.put(num, n - 1)
    }
  }
}
