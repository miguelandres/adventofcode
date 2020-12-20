package dev.miguely.aoc.d8

import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import scala.util.Success
import scala.util.Failure

class AsmRunner {
  var accumulator = 0;
  var index = 0;
  class Instruction {
    private var _hasRun = false
    def hasRun = _hasRun
    // runs the instruction and returns next instruction to run, or None if the instruction cant be run
    def run(): Try[Instruction] = Try{
      if (_hasRun) throw (new Exception())
      _hasRun = true
      doRun()
      updateIndex()
      instructions(index)
    }
    protected def updateIndex(): Unit = { index = index + 1 }
    protected def doRun(): Unit = {}
  }

  case class Jump(val num: Int) extends Instruction {
    override protected def updateIndex(): Unit = { index = index + num }
  }
  case class Acc(val num: Int) extends Instruction {
    override protected def doRun(): Unit = { accumulator = accumulator + num }
  }

  private val instructions = new ArrayBuffer[Instruction]()
  def addInstruction(instruction: Instruction)= {
    instructions += instruction
  }

  def run() ={
    var ins: Try[Instruction] = Success(instructions(index))
    while (ins.isSuccess) {
      ins = ins.get.run()
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
        case "nop" => new runner.Instruction()
        case "jmp" => new runner.Jump(parseNum(parts(1)))
        case "acc" => new runner.Acc(parseNum(parts(1)))
      })
    })
    runner.run()
    println(runner.accumulator)
  def parseNum(str: String): Int = {
    var num = if (str(0) == '-') -1 else 1
    num = num * str.substring(1).toInt
    return num
  }

}
