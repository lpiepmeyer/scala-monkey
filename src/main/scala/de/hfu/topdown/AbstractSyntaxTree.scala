package de.hfu.topdown

import de.hfu.evaluator.{BooleanValue, IntegerValue}
import de.hfu.lexer.{Precedence => _, _}

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
  def apply(lexer: TokenIterator): Expression = Equality(lexer)
}

abstract class Expression() extends Node

object Program {
  def apply(lexer: TokenIterator): Program = lexer.currentToken match {
    case EOFToken => Program(List())
    case _ => Program(StatementList(lexer, EOFToken))
  }
}

case class Program(statements: List[Statement]) extends Node {
  override def toString: String = statements.mkString(";\n") + (if (statements.nonEmpty) ";")
}

object StatementList {
  def build(lexer: TokenIterator, head: Statement, sentinel: Token): List[Statement] = lexer.currentToken match {
    case token: Token if token == sentinel =>
      val result = List(head)
      result
    case _ =>
      head :: build(lexer, Statement(lexer), sentinel)
  }

  def apply(lexer: TokenIterator, sentinel: Token): List[Statement] =
    build(lexer, Statement(lexer), sentinel)
}

object BlockStatement {
  def apply(lexer: TokenIterator): BlockStatement = {
    lexer.expectCurrent(LeftBraceToken)
    val statements = lexer.currentToken match {
      case RightBraceToken => List()
      case _ => StatementList(lexer, RightBraceToken)
    }
    val result = BlockStatement(statements)
    lexer.expectCurrent(RightBraceToken)
    result
  }
}

case class BlockStatement(statements: List[Statement]) extends Node {
  override def toString: String = statements match {
    case List() => "{}"
    case _ => "{\n\t" + statements.mkString(";\n") + (if (statements.nonEmpty) ";") + "\n}"
  }
}

object IfExpression {

  def apply(lexer: TokenIterator): Expression = {
    lexer.expectCurrent(IfToken)
    lexer.expectCurrent(LeftParenthesisToken)
    val condition = Expression(lexer)
    lexer.expectCurrent(RightParenthesisToken)
    if (lexer.currentToken != LeftBraceToken) throw new RuntimeException("found " + lexer.currentToken + " expected " + LeftBraceToken)
    val consequence = BlockStatement(lexer)
    if (lexer.currentToken == ElseToken) {
      lexer.nextTokens()
      if (lexer.currentToken != LeftBraceToken) throw new RuntimeException("found " + lexer.currentToken + " expected " + LeftBraceToken)
      val alternative = BlockStatement(lexer)
      return IfExpression(condition, consequence, Some(alternative))
    }
    IfExpression(condition, consequence, None)
  }

}

case class IfExpression(condition: Expression, consequence: BlockStatement, alternative: Option[BlockStatement]) extends Expression {
  override def toString: String = "if( " + condition + " )" + consequence.toString + (alternative match {
    case None => ""
    case Some(block) => "else " + block.toString
  })
}

object LetStatement {
  def apply(lexer: TokenIterator): LetStatement = {
    lexer.nextTokens()
    val identifier = Identifier(lexer)
    lexer.expectCurrent(AssignmentToken)
    val expression = Expression(lexer)
    val result = LetStatement(identifier.value, expression)
    lexer.expectCurrent(SemicolonToken)
    result
  }
}

case class LetStatement(name: String, expression: Expression) extends Statement {
  override def toString: String = "let " + name + " = " + expression.toString
}

object ReturnStatement {
  def apply(lexer: TokenIterator): ReturnStatement = {
    if (!lexer.nextTokens()) throw new RuntimeException
    val expression = Expression(lexer)
    lexer.expectCurrent(SemicolonToken)
    ReturnStatement(expression)
  }
}

case class ReturnStatement(expression: Expression) extends Statement {
  override def toString: String = "return " + expression
}

object ExpressionStatement {
  def apply(lexer: TokenIterator): ExpressionStatement = {
    val result = ExpressionStatement(Expression(lexer))
    if (lexer.currentToken == SemicolonToken) lexer.nextTokens()
    result
  }
}

case class ExpressionStatement(expression: Expression) extends Statement {
  override def toString: String = expression.toString
}

object BoolLiteral {
  def apply(lexer: TokenIterator): BoolLiteral = {
    val result = lexer.currentToken match {
      case TrueToken => BoolLiteral(true)
      case FalseToken => BoolLiteral(false)
    }
    lexer.nextTokens()
    result
  }
}

case class BoolLiteral(value: Boolean) extends Expression {
  override def toString: String = value.toString

  def evaluate() = BooleanValue(value)
}

object IntegerLiteral {
  def apply(lexer: TokenIterator): IntegerLiteral = {
    val result = IntegerLiteral(lexer.currentToken.text.toInt)
    lexer.nextTokens()
    result
  }
}

case class IntegerLiteral(value: Int) extends Expression {
  override def toString: String = value.toString

  def evaluate() = IntegerValue(value)

}

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

case class Identifier(value: String) extends Expression {
  override def toString: String = value
}


object FunctionLiteral {
  def build(lexer: TokenIterator, head: Identifier): List[Identifier] = lexer.currentToken match {
    case CommaToken =>
      lexer.nextTokens()
      head :: build(lexer, Identifier(lexer))
    case _ => List(head)
  }


  def apply(lexer: TokenIterator): Expression = {
    lexer.expectCurrent(FunctionToken)
    lexer.expectCurrent(LeftParenthesisToken)
    val parameters = lexer.currentToken match {
      case RightParenthesisToken => List()
      case _ => build(lexer, Identifier(lexer))
    }
    lexer.expectCurrent(RightParenthesisToken)
    val body = BlockStatement(lexer)
    FunctionLiteral(parameters, body)
  }
}

case class FunctionLiteral(parameters: List[Identifier], body: BlockStatement) extends Expression {
  override def toString: String = "fn(" + parameters.mkString(", ") + ")" + body.toString
}


object ParameterList {
  def build(lexer: TokenIterator, head: Identifier): List[Identifier] = lexer.currentToken match {
    case CommaToken =>
      lexer.nextTokens()
      head :: build(lexer, Identifier(lexer))
    case _ => List(head)
  }

  def apply(lexer: TokenIterator): ParameterList =
    ParameterList(build(lexer, Identifier(lexer)))
}

case class ParameterList(identifiers: List[Identifier]) {
  override def toString: String = identifiers.mkString(", ")
}


object Primary {
  def apply(lexer: TokenIterator): Primary = {
    val expression = lexer.currentToken match {
      case IdentifierToken(_) => Identifier(lexer)
      case IntegerToken(_) => IntegerLiteral(lexer)
      case TrueToken | FalseToken => BoolLiteral(lexer)
      case LeftParenthesisToken =>
        lexer.nextTokens()
        val result = Expression(lexer)
        lexer.expectCurrent(RightParenthesisToken)
        result
      case FunctionToken => FunctionLiteral(lexer)
      case IfToken => IfExpression(lexer)
      case _ => CallExpression(lexer)
    }
    Primary(expression)
  }
}

case class Primary(expression: Expression) {
  override def toString: String = expression.toString
}


object CallExpression {

  def build(lexer: TokenIterator, head: Expression): List[Expression] = lexer.currentToken match {
    case CommaToken =>
      lexer.nextTokens()
      head :: build(lexer, Expression(lexer))
    case _ => List(head)
  }


  def apply(lexer: TokenIterator): CallExpression = {
    val function = Identifier(lexer)
    lexer.expectCurrent(LeftParenthesisToken)
    val arguments = lexer.currentToken match {
      case RightParenthesisToken => List()
      case _ => build(lexer, Expression(lexer))
    }
    lexer.expectCurrent(RightParenthesisToken)
    lexer.nextTokens()
    CallExpression(function, arguments)
  }
}

case class CallExpression(function: Expression, arguments: List[Expression]) extends Expression {
  override def toString: String = function.toString + "(" + arguments.mkString(", ") + ")"
}


object PointTerm {
  def createPair(lexer: TokenIterator): (Token, Unary) = {
    val operator = lexer.currentToken
    lexer.nextTokens()
    (operator, Unary(lexer))
  }

  def build(lexer: TokenIterator, head: (Token, Unary)): List[(Token, Unary)] = lexer.currentToken match {
    case AsteriskToken | SlashToken =>
      head :: build(lexer, createPair(lexer))
    case _ => List(head)
  }

  def apply(lexer: TokenIterator): PointTerm = {
    val left = Unary(lexer)
    lexer.currentToken match {
      case AsteriskToken | SlashToken =>
        val right = build(lexer, createPair(lexer))
        PointTerm(left, right)
      case _ => PointTerm(left, List())
    }
  }
}

case class PointTerm(left: Unary, right: List[(Token, Unary)]) extends Expression {
  override def toString: String = left.toString + right.map(pair => " " + pair._1.toString + " " + pair._2.toString).mkString("")
}


object DashTerm {
  def createPair(lexer: TokenIterator): (Token, PointTerm) = {
    val operator = lexer.currentToken
    lexer.nextTokens()
    (operator, PointTerm(lexer))
  }

  def build(lexer: TokenIterator, head: (Token, PointTerm)): List[(Token, PointTerm)] = lexer.currentToken match {
    case PlusToken | MinusToken =>
      head :: build(lexer, createPair(lexer))
    case _ => List(head)
  }

  def apply(lexer: TokenIterator): DashTerm = {
    val left = PointTerm(lexer)
    lexer.currentToken match {
      case PlusToken | MinusToken =>
        val right = build(lexer, createPair(lexer))
        DashTerm(left, right)
      case _ => DashTerm(left, List())
    }
  }
}

case class DashTerm(left: PointTerm, right: List[(Token, PointTerm)]) extends Expression {
  override def toString: String = left.toString + right.map(pair => " " + pair._1.toString + " " + pair._2.toString).mkString("")
}

object Unary {
  def build(lexer: TokenIterator, head: Token): List[Token] = lexer.currentToken match {
    case BangToken | MinusToken =>
      val operator = lexer.currentToken
      lexer.nextTokens()
      head :: build(lexer, operator)
    case _ => List(head)
  }

  def apply(lexer: TokenIterator): Unary = {
    val prefixes = lexer.currentToken match {
      case BangToken | MinusToken =>
        val operator = lexer.currentToken
        lexer.nextTokens()
        build(lexer, operator)
      case _ => List()
    }
    val pointTerm = Primary(lexer)
    Unary(prefixes, pointTerm)
  }
}

case class Unary(prefixes: List[Token], primary: Primary) extends Expression {
  override def toString: String = prefixes.mkString("") + primary.toString
}

object Comparison {
  def apply(lexer: TokenIterator): Comparison = {
    val left = DashTerm(lexer)
    val right = lexer.currentToken match {
      case LessThanToken | GreaterThanToken =>
        val operator = lexer.currentToken
        lexer.nextTokens()
        val term = DashTerm(lexer)
        Some((operator, term))
      case _ => None
    }
    Comparison(left, right)
  }
}

case class Comparison(left: DashTerm, right: Option[(Token, DashTerm)]) extends Expression {
  override def toString: String = left.toString + (right match {
    case None => ""
    case Some(pair) => pair._1.toString + pair._2.toString
  })
}

object Equality {
  def apply(lexer: TokenIterator): Equality = {
    val left = Comparison(lexer)
    val right = lexer.currentToken match {
      case EqualsToken | NotEqualsToken =>
        val operator = lexer.currentToken
        lexer.nextTokens()
        val term = Comparison(lexer)
        Some((operator, term))
      case _ => None
    }
    Equality(left, right)
  }
}

case class Equality(left: Comparison, right: Option[(Token, Comparison)]) extends Expression {
  override def toString: String = left.toString + (right match {
    case None => ""
    case Some(pair) => pair._1.toString + pair._2.toString
  })
}
