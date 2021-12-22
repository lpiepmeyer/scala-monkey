package de.hfu.parser

import de.hfu.lexer.TokenIterator

abstract class Node {

}
abstract class Statement() extends Node
abstract class Expression() extends Node
case class Program(statements: List[Statement])

case class LetStatement(name :String, expression: Expression) extends Statement
case class ReturnStatement(expression: Expression) extends Statement
case class ExpressionStatement(expression: Expression) extends Statement

case class BoolLiteral(value: Boolean) extends Expression
case class IntegerLiteral(value: Int) extends Expression
case class Identifier(value: String) extends Expression


object Main extends App{
  def parseReturn(): Unit ={
    val it=new TokenIterator("return 23;")
    val parser=new Parser(it)
    val program=parser.parseProgram()
    println(program)
  }
def parseLet(): Unit ={
  val it=new TokenIterator("let y = true;")
  val parser=new Parser(it)
  val program=parser.parseProgram()
  println(program)

}



  parseLet
  parseReturn
}

case object Expression