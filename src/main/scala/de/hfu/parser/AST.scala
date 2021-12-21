package de.hfu.parser

import de.hfu.lexer.{IdentifierToken, TokenIterator}

abstract class Node {

}
abstract class Statement() extends Node
case class Expression() extends Node
case class Program(statements: List[Statement])

case class LetStatement(token :IdentifierToken, expression: Expression) extends Statement
case class ReturnStatement(expression: Expression) extends Statement



object Main extends App{
  def parseReturn(): Unit ={
    val it=new TokenIterator("return 23;")
    val parser=new Parser(it)
    val program=parser.parseProgram()
    println(program)
  }
def parseLet(): Unit ={
  val it=new TokenIterator("let x=23;")
  val parser=new Parser(it)
  val program=parser.parseProgram()
  println(program)
}

  parseLet
  parseReturn
}

case object Expression