package dev.miguely.aoc.d8

import scala.collection.mutable.ArrayBuffer
import scala.util.{Try, Success, Failure}
import scala.util.control.Breaks.{break, breakable}

class AsmRunner {
  var accumulator = 0;
  var index = 0;
  abstract class Instruction(val num: Int) {
    var hasRun = false
    // runs the instruction and returns next instruction to run, or None if the instruction cant be run
    def run(
        patchedInstructions: ArrayBuffer[Instruction] = instructions
    ): Try[Instruction] = Try {
      if (hasRun) throw (new Exception())
      hasRun = true
      doRun()
      updateIndex()
      patchedInstructions(index)
    }
    protected def updateIndex(): Unit = { index = index + 1 }
    protected def doRun(): Unit = {}
  }

  case class Nop(override val num: Int) extends Instruction(num)

  case class Jump(override val num: Int) extends Instruction(num) {
    override protected def updateIndex(): Unit = { index = index + num }
  }
  case class Acc(override val num: Int) extends Instruction(num) {
    override protected def doRun(): Unit = { accumulator = accumulator + num }
  }

  private val instructions = new ArrayBuffer[Instruction]()
  def addInstruction(instruction: Instruction) = {
    instructions += instruction
  }

  private def runInternal(
      patchedInstructions: ArrayBuffer[Instruction] = instructions
  ): Boolean = {
    var ins: Try[Instruction] = Success(instructions(index))
    while (ins.isSuccess) {
      ins = ins.get.run(patchedInstructions)
    }
    return index == instructions.size
  }

  def run(): Boolean = {
    runInternal()
  }

  def fix() = {
    var patchedInstructions = instructions.clone()
    breakable {
      for (i <- 0 to patchedInstructions.size - 1) {
        accumulator = 0
        index = 0
        for (instruction <- instructions) instruction.hasRun = false
        for (instruction <- patchedInstructions) instruction.hasRun = false
        var patch = instructions(i) match {
          case a: Acc  => None
          case n: Nop  => Some(new Jump(n.num))
          case j: Jump => Some(new Nop(j.num))
        }
        patch match {
          case Some(ins) =>
            patchedInstructions(i) = ins
            if (runInternal(patchedInstructions)) {
              println(accumulator);
              break
            }
            patchedInstructions(i) = instructions(i)
          case _ => ()
        }
      }
    }
  }
}

object AsmBoot extends App {
  val runner = new AsmRunner()
  scala.io.Source
    .fromFile("input/d8.txt")
    .getLines()
    .foreach(line => {
      val parts = line.split(" ")
      runner.addInstruction(parts(0) match {
        case "nop" => new runner.Nop(parseNum(parts(1)))
        case "jmp" => new runner.Jump(parseNum(parts(1)))
        case "acc" => new runner.Acc(parseNum(parts(1)))
      })
    })
  runner.run()
  println(runner.accumulator)
  runner.fix()
  def parseNum(str: String): Int = {
    var num = if (str(0) == '-') -1 else 1
    num = num * str.substring(1).toInt
    return num
  }

}
