package de.hfu.monkey

import de.hfu.monkey.lexer.Lexer

import java.io.FileReader
import scala.io.Source
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length == 1) {
      val filename = args(0)
      println(execute(filename))
    } else {
      dumpFace()
      repl()
    }
  }

  private def toString(value: Value): String = value match {
    case IntegerValue(v) => v.toString
    case BooleanValue(v) => v.toString
    case NoValue => "None"
    case ReturnValue(value) => toString(value)
    case FunctionValue(parameters, body) => "<<function definition>>"
  }

  private def evaluate(lexer: Lexer, stack: Stack = Stack()): String =
    try {
      toString(Program(lexer).evaluate(stack))
    } catch {
      case MonkeyException(message) => message
      case e: RuntimeException => e.getMessage + " (replace by MonkeyEcpetion)"
    }


  def execute(filename: String): String = {
    val reader = new FileReader(filename)
    val actual = evaluate(Lexer(reader))
    actual
  }

  private def dumpFace() {
    val face = Source.fromFile("monkey.face").getLines.toList.mkString("\n")
    println(face)

  }

  private def repl(): Unit = {
    println("Welcome to the monkey REPL")
    val context = Stack()
    while (true) {
      val input = readLine(">> ")
      if (input.trim() == "exit") {
        println("terminating REPL")
        return
      }
      val actual = evaluate(Lexer(input), context)
      println(actual)
    }
  }
}
