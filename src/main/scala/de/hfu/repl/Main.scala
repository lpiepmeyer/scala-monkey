package de.hfu.repl

import de.hfu.evaluator.{Context, Evaluator}
import de.hfu.lexer.TokenIterator
import de.hfu.parser.Parser

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

  private def execute(filename: String): Unit = {
    val reader = new FileReader(filename)
    val actual = Evaluator(new Parser(TokenIterator(reader)).parseProgram(), Context())
    println(actual)
  }

  private def repl(): Unit = {
    println("starting REPL")
    val context = Context()
    while (true) {
      val input = readLine(">> ")
      if (input.trim() == "exit") {
        println("terminating REPL")
        return
      }
      val actual = Evaluator(new Parser(TokenIterator(input)).parseProgram(), context)
      println(actual)
    }
  }
}
