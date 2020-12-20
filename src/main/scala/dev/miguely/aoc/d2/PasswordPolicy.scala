package dev.miguely.aoc.d2
import scala.io.Source
import scala.util.matching.Regex

//Counting Policy is first part
case class CountingPolicy(char: String, min: Int, max: Int)
//PositionalPolicy is second part
case class PositionalPolicy(char: Char, pos1: Int, pos2: Int)

object PasswordPolicy {
  val passwordPattern: Regex = "^([0-9]+)-([0-9]+) ([^\\s]): ([^\\s]+)$".r

  def main(args: Array[String]): Unit = {
    var count = 0
    Source
      .fromFile("input/d2.txt")
      .getLines()
      .foreach { line =>
        var matchesOption = passwordPattern.findFirstMatchIn(line)
        val matches = matchesOption.get
        // Create CountingPolicy for the first part and PositionalPolicy for the second part
        // val policy = CountingPolicy(
        // matches.group(3),
        // matches.group(1).toInt,
        // matches.group(2).toInt
        // )
        val policy = PositionalPolicy(
          matches.group(3).charAt(0),
          matches.group(1).toInt - 1,
          matches.group(2).toInt - 1
        )
        if (compliesWithPolicy(matches.group(4), policy))
          count = count + 1
      }
    println(count)
  }

  def compliesWithPolicy(password: String, policy: CountingPolicy): Boolean = {
    val matches = policy.char.r.findAllIn(password).length
    return (matches >= policy.min && matches <= policy.max)
  }

  def compliesWithPolicy(
      password: String,
      policy: PositionalPolicy
  ): Boolean =
    return (password.charAt(policy.pos1) != password.charAt(policy.pos2) &&
      (password.charAt(policy.pos1) == policy.char ||
        password.charAt(policy.pos2) == policy.char))
}
