package dev.miguely.aoc.d2p1
import scala.io.Source
import scala.util.matching.Regex

case class Policy(char: String, min: Int, max: Int)

object PasswordPolicy {
  val passwordPattern: Regex = "^([0-9]+)-([0-9]+) ([^\\s]): ([^\\s]+)$".r

  def main(args: Array[String]): Unit = {
    var count = 0
    Source.fromFile("input/d2p1.txt")
    .getLines()
    .foreach( line =>{
      var matchesOption = passwordPattern.findFirstMatchIn(line)
      val matches = matchesOption.get
      val policy = Policy(
        matches.group(3),
        matches.group(1).toInt,
        matches.group(2).toInt
      )
      if (compliesWithPolicy(matches.group(4), policy)) count = count + 1
    })
    println(count)
  }

  def compliesWithPolicy(password: String, policy: Policy): Boolean = {
    val matches = policy.char.r.findAllIn(password).length
    return (matches >= policy.min && matches <= policy.max)
  }
}
