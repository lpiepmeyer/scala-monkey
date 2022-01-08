package de.hfu.parser

import de.hfu.lexer.Precedence._
import de.hfu.lexer.{Precedence => _, _}

import scala.collection.mutable.ListBuffer

abstract class Node {}

object Statement {
  def apply(lexer: TokenIterator): Statement = lexer.currentToken match {
    case LetToken => LetStatement(lexer)
    case ReturnToken => ReturnStatement(lexer)
    case _ => ExpressionStatement(lexer)
  }
}

abstract class Statement() extends Node

object Expression {
  def apply(lexer: TokenIterator, precedence: Precedence = LOWEST): Expression = {
    var leftExpression = createPrefix(lexer)
    while (lexer.peekToken != SemicolonToken && precedence < lexer.peekPrecedence()) {
      lexer.nextTokens()
      leftExpression = createInfix(lexer, leftExpression)
    }
    leftExpression
  }

  private def parseGroupedExpression(lexer: TokenIterator): Expression = {
    lexer.nextTokens()
    val result = Expression(lexer)
    lexer.expectPeek(RightParenthesisToken)
    result
  }

  private def createPrefix(lexer: TokenIterator): Expression = lexer.currentToken match {
    case IntegerToken(literal) => IntegerLiteral(literal.toInt)
    case TrueToken => BoolLiteral(true)
    case FalseToken => BoolLiteral(false)
    case IdentifierToken(name) => Identifier(name)
    case LeftParenthesisToken => parseGroupedExpression(lexer)
    case IfToken => IfExpression(lexer)
    case FunctionToken => FunctionLiteral(lexer)
    case BangToken | MinusToken => PrefixExpression(lexer)
    case _ => throw new RuntimeException
  }

  private def createInfix(lexer: TokenIterator, leftExpression: Expression): Expression = lexer.currentToken match {
    case PlusToken | MinusToken | SlashToken | AsteriskToken | EqualsToken | NotEqualsToken | LessThanToken | GreaterThanToken => InfixExpression(lexer, leftExpression)
    case LeftParenthesisToken => CallExpression(lexer, leftExpression)
    case _ => leftExpression
  }
}

abstract class Expression() extends Node

object Program {
  def apply(lexer: TokenIterator): Program = {
    val result: ListBuffer[Statement] = ListBuffer()
    do {
      result.addOne(Statement(lexer))
    } while (lexer.nextTokens())
    Program(result.toList)
  }
}

case class Program(statements: List[Statement]) extends Node

object BlockStatement {
  def apply(lexer: TokenIterator): BlockStatement = {
    val statements: ListBuffer[Statement] = ListBuffer()
    while (lexer.nextTokens() && lexer.currentToken != RightBraceToken && lexer.currentToken != EOFToken) {
      statements.addOne(Statement(lexer))
    }
    BlockStatement(statements.toList)
  }
}

case class BlockStatement(statements: List[Statement]) extends Node

object IfExpression {

  def apply(lexer: TokenIterator): Expression = {
    lexer.expectPeek(LeftParenthesisToken)
    lexer.nextTokens()
    val condition = Expression(lexer)
    lexer.expectPeek(RightParenthesisToken)
    lexer.expectPeek(LeftBraceToken)
    val consequence = BlockStatement(lexer)
    if (lexer.peekToken == ElseToken) {
      lexer.nextTokens()
      lexer.expectPeek(LeftBraceToken)
      val alternative = BlockStatement(lexer)
      return IfExpression(condition, consequence, Some(alternative))
    }
    IfExpression(condition, consequence, None)
  }

}

case class IfExpression(condition: Expression, consequence: BlockStatement, alternative: Option[BlockStatement]) extends Expression

object LetStatement {
  def apply(lexer: TokenIterator): LetStatement = {
    if (!lexer.nextTokens()) throw new RuntimeException
    (lexer.currentToken, lexer.peekToken) match {
      case (identifier: IdentifierToken, AssignmentToken) if lexer.nextTokens() && lexer.nextTokens() =>
        val expression = Expression(lexer)
        if (lexer.peekToken == SemicolonToken) lexer.nextTokens()
        LetStatement(identifier.literal, expression)
      case _ => throw new RuntimeException
    }
  }
}

case class LetStatement(name: String, expression: Expression) extends Statement

object ReturnStatement {
  def apply(lexer: TokenIterator): ReturnStatement = {
    if (!lexer.nextTokens()) throw new RuntimeException
    val expression = Expression(lexer)
    if (lexer.peekToken == SemicolonToken) lexer.nextTokens()
    ReturnStatement(expression)
  }
}

case class ReturnStatement(expression: Expression) extends Statement

object ExpressionStatement {
  def apply(lexer: TokenIterator): ExpressionStatement = {
    val result = ExpressionStatement(Expression(lexer))
    if (lexer.peekToken == SemicolonToken) lexer.nextTokens()
    result
  }
}

case class ExpressionStatement(expression: Expression) extends Statement

case class BoolLiteral(value: Boolean) extends Expression

case class IntegerLiteral(value: Int) extends Expression

object Identifier {
  def apply(lexer: TokenIterator): Identifier = {
    val result = lexer.currentToken match {
      case IdentifierToken(name) => Identifier(name)
      case _ => throw new RuntimeException
    }
    lexer.nextTokens()
    result
  }
}

case class Identifier(value: String) extends Expression

object PrefixExpression {
  def apply(lexer: TokenIterator): PrefixExpression = {
    val token = lexer.currentToken
    if (!lexer.nextTokens())
      throw new RuntimeException
    val expression = Expression(lexer, PREFIX)
    PrefixExpression(token, expression)
  }
}

case class PrefixExpression(operator: Token, right: Expression) extends Expression

object InfixExpression {
  def apply(lexer: TokenIterator, left: Expression): Expression = {
    val precedence = lexer.curPrecedence()
    val operator = lexer.currentToken
    lexer.nextTokens()
    val right = Expression(lexer, precedence)
    InfixExpression(operator, left, right)
  }
}

case class InfixExpression(operator: Token, left: Expression, right: Expression) extends Expression

object FunctionLiteral {
  def apply(lexer: TokenIterator): Expression = {
    lexer.expectPeek(LeftParenthesisToken)
    val parameters = parseFunctionParameters(lexer)
    lexer.expectPeek(LeftBraceToken)
    val body = BlockStatement(lexer)
    FunctionLiteral(parameters, body)
  }

  private def parseFunctionParameters(lexer: TokenIterator): List[Identifier] = {
    val result: ListBuffer[Identifier] = ListBuffer()
    if (lexer.peekToken == RightParenthesisToken) {
      lexer.nextTokens()
      return result.toList
    }
    do {
      lexer.nextTokens()
      result.addOne(Identifier(lexer))
    } while (lexer.currentToken == CommaToken)
    if (lexer.currentToken != RightParenthesisToken) throw new RuntimeException
    result.toList
  }
}

case class FunctionLiteral(parameters: List[Identifier], body: BlockStatement) extends Expression

object CallExpression {

  def apply(lexer: TokenIterator, function: Expression): CallExpression = {
    val arguments = parseCallArguments(lexer)
    CallExpression(function, arguments)
  }

  private def parseCallArguments(lexer: TokenIterator): List[Expression] = {
    val result: ListBuffer[Expression] = ListBuffer()
    if (lexer.peekToken == RightParenthesisToken) {
      lexer.nextTokens()
      return result.toList
    }
    do {
      lexer.nextTokens()
      result.addOne(Expression(lexer))
      lexer.nextTokens()
    } while (lexer.currentToken == CommaToken)
    if (lexer.currentToken != RightParenthesisToken) throw new RuntimeException
    result.toList
  }

}

case class CallExpression(function: Expression, arguments: List[Expression]) extends Expression

