package de.hfu.repl

import de.hfu.evaluator.{Context, Evaluator}
import de.hfu.lexer.TokenIterator
import de.hfu.parser.Parser

import java.io.FileReader

object Main {
  def execute(filename:String) {
    val reader=new FileReader(filename)
    val actual = Evaluator(new Parser(TokenIterator(reader)).parseProgram(),new Context)
    println(actual)
  }

  def main(args: Array[String]): Unit = {
    val filename=args(0)
    execute(filename)
  }
}
