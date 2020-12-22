package dev.miguely.aoc.d11

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.control.Breaks._

object SeatSimulator extends App {
  var linput = "input/d11.txt"
  var sinput = "input/small/d11.txt"
  var input = linput
  var map = new SeatMap
  var directions = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

  parseMap()
  println(map.simulateAdjacentSeats())
  println(map.simulateVisibleSeats())

  def parseMap() {
    var row = 0
    var column = 0

    Source.fromFile(input).getLines().foreach { line =>
      column = 0
      var newRow = new ArrayBuffer[Seat]()
      for (i <- 0 to line.size - 1)
        newRow += Seat(
          row,
          i,
          line(i) match {
            case '.' => None
            case _   => Some(false)
          }
        )
      map.addRow(newRow.toSeq)
      row += 1
    }
  }

  case class Seat(x: Int, y: Int, taken: Option[Boolean]) {
    def countTakenSurroundingSeats(): Int = {
      var count = 0
      (for (i <- x - 1 to x + 1; j <- y - 1 to y + 1)
        if (i >= 0 && i < map.maxX && j >= 0 && j < map.maxY && (i != x || j != y)) {
          count = map.rows(i)(j).taken match {
            case None        => count
            case Some(false) => count
            case Some(true)  => count + 1
          }
        })
      count
    }

  }

  class SeatMap {
    def maxX = rows.size
    def maxY = _maxY
    var rows = new ArrayBuffer[Seq[Seat]]()
    var mapVisible: ArrayBuffer[ArrayBuffer[Int]] = null
    private var _maxY = 0

    def addRow(row: Seq[Seat]) = {
      rows.addOne(row)
      _maxY = row.size
    }

    private def simulateOneRound(funcSeat: (Seat => Boolean),funcUnseat: (Seat => Boolean)): Boolean = {
      var newRows = new ArrayBuffer[Seq[Seat]]
      var moved = false
      for (i <- 0 to maxX - 1) {
        var newRow = new ArrayBuffer[Seat]
        for (j <- 0 to maxY - 1) {
          var seat = rows(i)(j)
          newRow += (seat.taken match {
            case None => seat
            case Some(false) =>
              if (funcSeat(seat)) {
                moved = true
                Seat(i, j, Some(true))
              } else {
                seat
              }
            case Some(true) =>
              if (funcUnseat(seat)) {
                moved = true
                Seat(i, j, Some(false))
              } else seat
          })
        }
        newRows += newRow.toSeq
      }
      if (moved) rows = newRows
      moved
    }

    def simulateAdjacentSeats(): Int = {
      while (simulateOneRound(_.countTakenSurroundingSeats ==0,_.countTakenSurroundingSeats > 3)) {}
      var count = 0;
      for (row <- rows; seat <- row) seat.taken match {
        case Some(true) => count += 1
        case _          => ()
      }
      count
    }

    def simulateVisibleSeats(): Int = {
      mapVisible = ArrayBuffer.fill(maxX)(ArrayBuffer.fill(maxY)(0))
      while (simulateOneRound(seat => mapVisible(seat.x)(seat.y) == 0,seat => mapVisible(seat.x)(seat.y) > 4)) {
        mapVisible = ArrayBuffer.fill(maxX)(ArrayBuffer.fill(maxY)(0))
        for (x <- 0 to maxX - 1; y <- 0 to maxY - 1)
          if (rows(x)(y).taken == Some(true)) {
            for (direction <- directions) {
              var i = x + direction._1
              var j = y + direction._2
              breakable {
                while (i >= 0 && i < maxX && j >= 0 && j < maxY) {
                  rows(i)(j).taken match {
                    case Some(_) =>
                      mapVisible(i)(j) += 1
                      break
                    case None => ()
                  }
                  i += direction._1
                  j += direction._2
                }
              }
            }
          }
      }
      var count = 0;
      for (row <- rows; seat <- row) seat.taken match {
        case Some(true) => count += 1
        case _          => ()
      }
      count
    }
  }
}
