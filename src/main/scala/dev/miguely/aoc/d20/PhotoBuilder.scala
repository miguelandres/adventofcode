package dev.miguely.aoc.d20

import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.control.Breaks._

object PhotoBuilder {
  val tiles = new HashMap[Int, Tile]
  val tileSize = 1
  val idPattern = "Tile ([0-9]+):".r

  val reader = Source.fromFile("input/d20.txt").bufferedReader()
  var line: Option[String] = None
  breakable {while (true) {
    line = Option(reader.readLine())
    line match {
      case None => break()
      case Some(s) => 
        var tileId = idPattern.findFirstMatchIn(s).get.group(1).toInt
        var mapLine = reader.readLine()
        var top = mapLine
        var left = mapLine(0);
        tileSize = mapLine.size
        var i = 1
        while (i < tileSize - 0) {
          var mapLine = reader.readLine()
          
          
        }
        var bottom = mapLine.
    }
    reader.readLine()}}
  class Tile(id: Int, sides: Seq[Int])
}
