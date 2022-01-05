package de.hfu.parser

import de.hfu.lexer.Token

abstract class Node {

}
abstract class Statement() extends Node
abstract class Expression() extends Node
case class Program(statements: List[Statement])
case class BlockStatement(statements: List[Statement])

case class  IfExpression(condition : Expression, consequence:BlockStatement, alternative:Option[BlockStatement]) extends Expression


case class LetStatement(name :String, expression: Expression) extends Statement{
  override def toString: String ="let "+name+" = "+expression
}
case class ReturnStatement(expression: Expression) extends Statement{
  override def toString()="return "+expression
}
case class ExpressionStatement(expression: Expression) extends Statement

case class BoolLiteral(value: Boolean) extends Expression{
  override def toString()=value.toString
}
case class IntegerLiteral(value: Int) extends Expression{
  override def toString()=value.toString
}
case class Identifier(value: String) extends Expression{
  override def toString()=value.toString
}
case class PrefixExpression(operator: Token, right: Expression) extends Expression
case class InfixExpression(operator: Token,left: Expression, right: Expression) extends Expression

case class  FunctionLiteral(parameters: List[Identifier], body: BlockStatement) extends Expression
