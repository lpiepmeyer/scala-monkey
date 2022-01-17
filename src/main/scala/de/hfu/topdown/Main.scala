package de.hfu.topdown


import de.hfu.topdown.lexer.Lexer

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
    val actual = de.hfu.topdown.Program(Lexer(reader)).evaluate(Context())
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
      val actual = Program(Lexer(input)).evaluate(context)
      println(actual)
    }
  }
}
