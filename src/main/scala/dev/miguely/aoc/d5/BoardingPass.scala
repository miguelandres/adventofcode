package dev.miguely.aoc.d5

import java.lang.Integer

object BoardingPass extends App {

  /**
    * Part 1: find the max ID of boarding pass you've seen.
    * 
    * Boarding pass IDs are simple: they're basically binary where F/L = 0 B/R = 1
    * So we first replace these in every string, and then parse it as a binary string
    * 
    * We use Java's underlying Integer.valueOf (didn't know we could do that!)
    * 
    * Finally we report the max among all the scanned boarding passes.
    * 
    * Part2: We need to find a seat that is empty, but surrounded by a taken seat
    * on each side. 
    * 
    * While reading the numbers, we fill a binary array up to 1023 (the max value
    * possible with that binary length). then we just iterate between all seats that are
    * not in the first or last row (0-7 or 1016-1023) to find one which is false and 
    * whose neighbors are true
    */
  var arr = Array.fill(1024) { false }
  var max = 0
  scala.io.Source
    .fromFile("input/d5.txt")
    .getLines()
    .foreach(line => {
      var curr = Integer.valueOf(
        line
          .replace('F', '0')
          .replace('B', '1')
          .replace('R', '1')
          .replace('L', '0'),
        2
      )
      if (curr > max) max = curr
      arr(curr) = true

    })
  println(max)
  for (i <- 8 to 1015) {
    if (!arr(i) && arr(i - 1) && arr(i + 1))
      println(i)
  }
}
