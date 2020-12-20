package dev.miguely.aoc.d6

object CustomsDeclaration extends App {

  var questions = Array.fill(26) { false }
  var questionCount = Array.fill(26) { 0 }
  var sumAnyone = 0;
  var sumEveryone = 0;
  var count = 0
  scala.io.Source
    .fromFile("input/d6.txt")
    .getLines()
    .foreach(line => {
      if (line.size == 0) {
        sumAnyone += reportAnyone()
        sumEveryone += reportEveryone(count)
        questions = Array.fill(26) { false }
        questionCount = Array.fill(26) { 0 }
        count = 0
      } else {
        line.foreach(char => {
          questions(char - 'a') = true
          questionCount(char - 'a') = questionCount(char - 'a') + 1
        })
        count = count + 1
      }
    })
  //sumAnyone += reportAnyone()
  //sumEveryone += reportEveryone(count)

  println(sumAnyone)
  println(sumEveryone)

  def reportAnyone(): Int = {
    questions.foldRight[Int](0)((present, count) => {
      if (present) count + 1 else count
    })
  }

  def reportEveryone(people: Int): Int = {
    questionCount.foldRight[Int](0)((count, result) => {
      if (count == people) result + 1 else result
    })
  }
}
