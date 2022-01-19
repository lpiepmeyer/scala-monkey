package de.hfu.monkey


import de.hfu.monkey.lexer.Lexer

import java.io.FileReader
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length == 1) {
      val filename = args(0)
      execute(filename)
    } else {
      repl()
    }
  }

  def toString(value: Value): String = value match {
    case IntegerValue(v) => v.toString
    case BooleanValue(v) => v.toString
    case NoValue => "None"
    case ReturnValue(value) => toString(value)
    case FunctionValue(parameters, body) => "<<function definition>>"
  }

  def evaluate(lexer: Lexer, stack: Stack = Stack()): String =
    try {
      toString(Program(lexer).evaluate(stack))
    } catch {
      case MonkeyException(message) => message
      case e: RuntimeException => e.getMessage + " (replace by MonkeyEcpetion)"
    }


  private def execute(filename: String): Unit = {
    val reader = new FileReader(filename)
    val actual = evaluate(Lexer(reader))
    println(actual)
  }

  private def repl(): Unit = {
    println("starting REPL")
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
