package dev.miguely.aoc.d1

import scala.io.Source
import scala.Console.println
import scala.util.control.Breaks._
import scala.collection.immutable.HashMap
object ExpenseReport extends App {
  val source = Source.fromFile("input/d1.txt")
  try {
    val entries = source.getLines().map(_.toInt)

    var map = HashMap[Int,(List[List[Int]])]()
    breakable {
      for (entry <- entries) {
        var newMap = map
        //println (map.keySet)
        map.keysIterator foreach(key => {
          map.get(key).get.foreach (list => {
            if (list.size == 2 && key + entry == 2020) {
              println(entry * list(1) * list(0))
              break
            }
            else if (list.size == 1) {
              var options = newMap.getOrElse(key + entry, List[List[Int]]())
              newMap = newMap + (key+entry -> options.appended(List(list(0),entry)))
            }
          })
        })
        var options = newMap.getOrElse(entry, List[List[Int]]()) 
        map = newMap + (entry -> options.appended(List[Int](entry)))
      }
    }
  } finally source.close()
}
