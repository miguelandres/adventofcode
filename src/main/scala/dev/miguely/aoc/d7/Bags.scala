package dev.miguely.aoc.d7

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object Bags extends App {
  val parentBag = "(.+) bags contain".r
  val childBag = "([0-9]+) (.+) bags?".r
  val graph = new Graph()

  scala.io.Source
    .fromFile("input/d7.txt")
    .getLines()
    .foreach { line =>
      var parent = parentBag.findFirstMatchIn(line).get.group(1)
      graph.getOrAddNode(parent)
      line
        .split(" contain ")(1)
        .split(",")
        .foreach { part =>
          childBag
            .findAllMatchIn(part)
            .foreach(m => graph.addEdge(parent, m.group(2), m.group(1).toInt))
        }
    }

  println(graph.getOrAddNode("shiny gold").countAncestors())

  println(graph.getOrAddNode("shiny gold").calculateChildrenRecursiveCost())

  class Graph {
    class Node(var name: String) {
      val children = new ArrayBuffer[(Node, Int)]()
      val parents = new ArrayBuffer[(Node, Int)]()

      def countAncestors(visitedAncestors: HashSet[String] = new HashSet()): Int =
        return parents.foldLeft(0) { (result, tuple) =>
          if (visitedAncestors.contains(tuple._1.name)) return result
          else {
            visitedAncestors += tuple._1.name
            return result + tuple._1.countAncestors(visitedAncestors)
          }
        }

      def calculateChildrenRecursiveCost(): Int =
        return children.foldLeft(0) { (result, tuple) =>
          result + tuple._2 * (1 + tuple._1.calculateChildrenRecursiveCost())
        }
    }
    val nodes = new HashMap[String, Node]()

    def addEdge(parent: String, child: String, cost: Int = 1): Unit = {
      var childNode = getOrAddNode(child)
      var parentNode = getOrAddNode(parent)
      childNode.parents += ((parentNode, cost))
      parentNode.children += ((childNode, cost))
    }

    def getOrAddNode(name: String): Node = {
      if (!nodes.contains(name)) {
        nodes += (name -> new Node(name));
      }
      nodes(name)
    }
  }
}
