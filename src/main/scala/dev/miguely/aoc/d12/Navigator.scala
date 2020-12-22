package dev.miguely.aoc.d22

import scala.io.Source

object Navigator extends App {
  val linput = "input/d12.txt"
  val sinput = "input/small/d12.txt"
  val input = linput

  var position = (0L, 0L)
  var waypointPosition = (0L, 0L)
  var waypoint = (1L, 10L)

  var direction = 90L

  Source.fromFile(input).getLines().foreach { line =>
    val instruction = parseInstruction(line)
    executeInstruction(instruction)
  }

  println(Math.abs(position._1) + Math.abs(position._2))
  println(Math.abs(waypointPosition._1) + Math.abs(waypointPosition._2))

  def parseInstruction(line: String): (Char, Long) =
    return (line(0), line.slice(1, line.size).toLong)

  def executeInstruction(instruction: (Char, Long)) {
    instruction match {
      case (cardinal, _) if "NESW".contains(cardinal) =>
        position = movePos(instruction, position)
        waypoint = movePos(instruction, waypoint)
      case ('R', int) => rotate(int)
      case ('L', int) => rotate(-int)
      case ('F', int) => forward(int)
      case _          => ()

    }
    // println(instruction)
    // println(s"position: $position")
    // println(s"waypoint: $waypoint")
    // println(s"waypointPosition: $waypointPosition")

  }

  def movePos(instruction: (Char, Long), pos: (Long, Long)): (Long, Long) =
    instruction match {
      case ('N', n) => (pos._1 + n, pos._2)
      case ('S', n) => (pos._1 - n, pos._2)
      case ('E', n) => (pos._1, pos._2 + n)
      case ('W', n) => (pos._1, pos._2 - n)
    }

  def rotate(degrees: Long) {
    direction += degrees
    while (direction < 0) direction = direction + 360
    direction %= 360

    var rotation = degrees
    while (rotation < 0) rotation += 360
    rotation match {
      case 90  => waypoint = (-waypoint._2, waypoint._1)
      case 180 => waypoint = (-waypoint._1, -waypoint._2)
      case 270 => waypoint = (waypoint._2, -waypoint._1)
    }
  }

  def forward(distance: Long) {
    waypointPosition = (
      waypointPosition._1 + distance * waypoint._1,
      waypointPosition._2 + distance * waypoint._2
    )
    direction match {
      case 0   => position = movePos(('N', distance), position)
      case 90  => position = movePos(('E', distance), position)
      case 180 => position = movePos(('S', distance), position)
      case 270 => position = movePos(('W', distance), position)
    }
  }
}
