package de.hfu.monkey

import de.hfu.monkey.lexer._

abstract class Node {
  def evaluate(stack: Stack): Value
}

object Statement {
  def apply(lexer: Lexer): Statement = lexer.currentToken match {
    case LetToken => LetStatement(lexer)
    case ReturnToken => ReturnStatement(lexer)
    case _ => ExpressionStatement(lexer)
  }
}

abstract class Statement() extends Node

object Expression {
  def apply(lexer: Lexer): Expression = Equality(lexer)
}

abstract class Expression() extends Node

object ParanthizedExpression {
  def apply(lexer: Lexer): ParanthizedExpression = {
    lexer.expectCurrent(LeftParenthesisToken)
    val result = ParanthizedExpression(Expression(lexer))
    lexer.expectCurrent(RightParenthesisToken)
    result
  }
}

case class ParanthizedExpression(expression: Expression) extends Expression {
  override def toString: String = "(" + expression + ")"

  override def evaluate(stack: Stack): Value = expression.evaluate(stack)
}

object Program {
  def apply(lexer: Lexer): Program = lexer.currentToken match {
    case EOFToken => Program(List())
    case _ => Program(StatementList(lexer, EOFToken))
  }
}

case class Program(statements: List[Statement]) extends Node {
  override def toString: String = statements.mkString("\n")

  override def evaluate(stack: Stack): Value =
    statements
      .map(_.evaluate(stack))
      .map(_ match {
        case ReturnValue(v) => return v
        case v => v
      }).last
}

object StatementList {
  def build(lexer: Lexer, head: Statement, sentinel: Token): List[Statement] = lexer.currentToken match {
    case token: Token if token == sentinel =>
      val result = List(head)
      result
    case _ =>
      head :: build(lexer, Statement(lexer), sentinel)
  }

  def apply(lexer: Lexer, sentinel: Token): List[Statement] =
    build(lexer, Statement(lexer), sentinel)
}

object BlockStatement {
  def apply(lexer: Lexer): BlockStatement = {
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
    case _ => "{\n\t" + statements.mkString("\n") + (if (statements.nonEmpty) "\n}")
  }

  override def evaluate(stack: Stack): Value =
    statements
      .map(_.evaluate(stack))
      .map(_ match {
        case v: ReturnValue => return v
        case v => v
      }).last
}

object IfExpression {

  def apply(lexer: Lexer): Expression = {
    lexer.expectCurrent(IfToken)
    lexer.expectCurrent(LeftParenthesisToken)
    val condition = Expression(lexer)
    lexer.expectCurrent(RightParenthesisToken)
    if (lexer.currentToken != LeftBraceToken) throw MonkeyException(LeftBraceToken, lexer.currentToken)
    val consequence = BlockStatement(lexer)
    if (lexer.currentToken == ElseToken) {
      lexer.nextToken()
      if (lexer.currentToken != LeftBraceToken) throw MonkeyException(LeftBraceToken, lexer.currentToken)
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

  override def evaluate(stack: Stack): Value = condition.evaluate(stack) match {
    case BooleanValue(false) | NoValue if alternative.isDefined => alternative.get.evaluate(stack)
    case BooleanValue(false) | NoValue => NoValue
    case _ => consequence.evaluate(stack)
  }

}

object LetStatement {
  def apply(lexer: Lexer): LetStatement = {
    lexer.nextToken()
    val identifier = Identifier(lexer)
    lexer.expectCurrent(AssignmentToken)
    val expression = Expression(lexer)
    val result = LetStatement(identifier.value, expression)

    lexer.skipToken(SemicolonToken)
    result
  }
}

case class LetStatement(name: String, expression: Expression) extends Statement {
  override def toString: String = "let " + name + " = " + expression.toString + ";"

  override def evaluate(stack: Stack): Value = stack(name) = expression.evaluate(stack)
}

object ReturnStatement {
  def apply(lexer: Lexer): ReturnStatement = {
     lexer.nextToken()
    val expression = Expression(lexer)
    lexer.skipToken(SemicolonToken)
    ReturnStatement(expression)
  }
}

case class ReturnStatement(expression: Expression) extends Statement {
  override def toString: String = "return " + expression + ";"

  override def evaluate(stack: Stack): Value = ReturnValue(expression.evaluate(stack))
}

object ExpressionStatement {
  def apply(lexer: Lexer): ExpressionStatement = {
    val result = ExpressionStatement(Expression(lexer))
    if (lexer.currentToken == SemicolonToken) lexer.nextToken()
    result
  }
}

case class ExpressionStatement(expression: Expression) extends Statement {
  override def toString: String = expression.toString + ";"

  override def evaluate(stack: Stack): Value = expression.evaluate(stack)
}

object BoolLiteral {
  def apply(lexer: Lexer): BoolLiteral = {
    val result = lexer.currentToken match {
      case TrueToken => BoolLiteral(true)
      case FalseToken => BoolLiteral(false)
      case token => throw MonkeyException("I tried to read a boolean value but found '" + token + "' which is neither 'true' nor 'false'")
    }
    lexer.nextToken()
    result
  }
}

case class BoolLiteral(value: Boolean) extends Expression {
  override def toString: String = value.toString

  def evaluate() = BooleanValue(value)

  override def evaluate(stack: Stack): Value = BooleanValue(value)
}

object IntegerLiteral {
  def apply(lexer: Lexer): IntegerLiteral = {
    val result = IntegerLiteral(lexer.currentToken.text.toInt)
    lexer.nextToken()
    result
  }
}

case class IntegerLiteral(value: Int) extends Expression {
  override def toString: String = value.toString

  def evaluate(stack: Stack) = IntegerValue(value)

}

object Identifier {
  def apply(lexer: Lexer): Identifier = {
    val result = lexer.currentToken match {
      case IdentifierToken(name) => Identifier(name)
      case unexpected: Token => throw MonkeyException("identifier", unexpected)
    }
    lexer.nextToken()
    result
  }
}

case class Identifier(value: String) extends Expression {
  override def toString: String = value

  override def evaluate(stack: Stack): Value = stack(value) match {
    case None => throw MonkeyException("identifier '" + this.value + "' not found")
    case Some(v) => v
  }
}


object FunctionLiteral {
  def build(lexer: Lexer, head: Identifier): List[Identifier] = lexer.currentToken match {
    case CommaToken =>
      lexer.nextToken()
      head :: build(lexer, Identifier(lexer))
    case _ => List(head)
  }


  def apply(lexer: Lexer): Expression = {
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

  override def evaluate(stack: Stack): Value = FunctionValue(parameters, body)
}


object Primary {
  def apply(lexer: Lexer): Primary = {
    val expression = lexer.currentToken match {
      case IdentifierToken(_) => Identifier(lexer)
      case IntegerToken(_) => IntegerLiteral(lexer)
      case TrueToken | FalseToken => BoolLiteral(lexer)
      case LeftParenthesisToken => ParanthizedExpression(lexer)
      case FunctionToken => FunctionLiteral(lexer)
      case IfToken => IfExpression(lexer)
    }
    Primary(expression)
  }
}

case class Primary(expression: Expression) extends Expression {
  override def toString: String = expression.toString

  override def evaluate(stack: Stack): Value = expression.evaluate(stack)
}


object CallExpression {

  def build(lexer: Lexer, head: Expression): List[Expression] = lexer.currentToken match {
    case CommaToken =>
      lexer.nextToken()
      head :: build(lexer, Expression(lexer))
    case _ => List(head)
  }


  def apply(lexer: Lexer): CallExpression = {
    val primary = Primary(lexer)
    if (lexer.currentToken != LeftParenthesisToken)
      return CallExpression(primary, None)
    lexer.expectCurrent(LeftParenthesisToken)
    val arguments = lexer.currentToken match {
      case RightParenthesisToken => List()
      case _ => build(lexer, Expression(lexer))
    }
    lexer.expectCurrent(RightParenthesisToken)
    CallExpression(primary, Some(arguments))
  }
}

case class CallExpression(primary: Primary, arguments: Option[List[Expression]]) extends Expression {
  override def toString: String = primary.toString + (if (arguments.nonEmpty) "(" + arguments.get.mkString(", ") + ")"; else "")

  override def evaluate(stack: Stack): Value = {
    if (arguments.isEmpty) return primary.evaluate(stack)
    val evaluatedArguments = arguments.get.map(_.evaluate(stack))
    val result = primary.evaluate(stack) match {
      case FunctionValue(parameters, body) =>
        val variables = parameters.map(_.value).zip(evaluatedArguments)
        val innerstack = stack.extend(variables)
        body.evaluate(innerstack)
      case _ => throw new RuntimeException
    }
    result match {
      case ReturnValue(value) => value
      case value => value
    }
  }
}


object PointTerm {
  def createPair(lexer: Lexer): (Token, Unary) = {
    val operator = lexer.currentToken
    lexer.nextToken()
    (operator, Unary(lexer))
  }

  def build(lexer: Lexer, head: (Token, Unary)): List[(Token, Unary)] = lexer.currentToken match {
    case AsteriskToken | SlashToken =>
      head :: build(lexer, createPair(lexer))
    case _ => List(head)
  }

  def apply(lexer: Lexer): PointTerm = {
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

  override def evaluate(stack: Stack): Value = right.foldLeft(left.evaluate(stack))((result: Value, pair) => (pair._1, pair._2.evaluate(stack)) match {
    case (AsteriskToken, t: IntegerValue) => multiply(result, t)
    case (SlashToken, t: IntegerValue) => divide(result, t)
    case (token, value) => throw MonkeyException("I tried to calculate a multiplication or divsion and did not expect the combiation of '" + token + "' and '" + value + "'")
  })

  private def multiply(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v * w)
    case (v, w) => throw MonkeyException("I tried to calculate a product and did not expect the operands of '" + v + "' and '" + w + "'")
  }

  private def divide(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v / w)
    case (v, w) => throw MonkeyException("I tried to calculate a division and did not expect the operands of '" + v + "' and '" + w + "'")
  }
}


object DashTerm {
  def createPair(lexer: Lexer): (Token, PointTerm) = {
    val operator = lexer.currentToken
    lexer.nextToken()
    (operator, PointTerm(lexer))
  }

  def build(lexer: Lexer, head: (Token, PointTerm)): List[(Token, PointTerm)] = lexer.currentToken match {
    case PlusToken | MinusToken =>
      head :: build(lexer, createPair(lexer))
    case _ => List(head)
  }

  def apply(lexer: Lexer): DashTerm = {
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

  override def evaluate(stack: Stack): Value =
    right.foldLeft(left.evaluate(stack))((result: Value, pair) => (pair._1, pair._2.evaluate(stack)) match {
      case (MinusToken, t: IntegerValue) => subtract(result, t)
      case (PlusToken, t: IntegerValue) => add(result, t)
      case (token, value) => throw MonkeyException("I tried to calculate a sum or difference and did not expect the operands of '" + token + "' and '" + value + "'")
    })

  private def add(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v + w)
    case (v, w) => throw MonkeyException("I tried to calculate a sum  and did not expect the operands of '" + v + "' and '" + w + "'")
  }

  private def subtract(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v - w)
    case (v, w) => throw MonkeyException("I tried to calculate a difference and did not expect the operands of '" + v + "' and '" + w + "'")
  }
}


object Unary {
  def build(lexer: Lexer, head: Token): List[Token] = lexer.currentToken match {
    case BangToken | MinusToken =>
      val operator = lexer.currentToken
      lexer.nextToken()
      head :: build(lexer, operator)
    case _ => List(head)
  }

  def apply(lexer: Lexer): Unary = {
    val prefixes = lexer.currentToken match {
      case BangToken | MinusToken =>
        val operator = lexer.currentToken
        lexer.nextToken()
        build(lexer, operator)
      case _ => List()
    }
    val pointTerm = CallExpression(lexer)
    Unary(prefixes, pointTerm)
  }
}

case class Unary(prefixes: List[Token], call: CallExpression) extends Expression {
  override def toString: String = prefixes.mkString("") + call.toString

  override def evaluate(stack: Stack): Value = prefixes.foldLeft(call.evaluate(stack))((term, token) => token match {
    case MinusToken => minus(term)
    case BangToken => not(term)
  })

  def minus(option: Value): Value = option match {
    case IntegerValue(v: Int) => IntegerValue(-v)
    case _ => NoValue
  }

  def not(option: Value): Value = option match {
    case BooleanValue(v: Boolean) => BooleanValue(!v)
    case NoValue => BooleanValue(true)
    case _ => BooleanValue(false)
  }
}

object Comparison {
  def apply(lexer: Lexer): Comparison = {
    val left = DashTerm(lexer)
    val right = lexer.currentToken match {
      case LessThanToken | GreaterThanToken =>
        val operator = lexer.currentToken
        lexer.nextToken()
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
    case Some(pair) => " " + pair._1.toString + " " + pair._2.toString
  })

  override def evaluate(stack: Stack): Value = right match {
    case None => left.evaluate(stack)
    case Some((token, right)) => token match {
      case LessThanToken => lessThan(left.evaluate(stack), right.evaluate(stack))
      case GreaterThanToken => greaterThan(left.evaluate(stack), right.evaluate(stack))
      case token => throw MonkeyException("I tried to compare two values and did not expect '" + token + "'")
    }
  }

  private def lessThan(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => BooleanValue(v < w)
    case (v, w) => throw MonkeyException("I tried to compare two integers and did not expect the operands '" + v + "' and '" + w + "'")
  }

  private def greaterThan(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => BooleanValue(v > w)
    case (v, w) => throw MonkeyException("I tried to compare two integers and did not expect the operands '" + v + "' and '" + w + "'")
  }
}

object Equality {
  def apply(lexer: Lexer): Equality = {
    val left = Comparison(lexer)
    val right = lexer.currentToken match {
      case EqualsToken | NotEqualsToken =>
        val operator = lexer.currentToken
        lexer.nextToken()
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
    case Some(pair) => " " + pair._1.toString + " " + pair._2.toString
  })

  override def evaluate(stack: Stack): Value = right match {
    case None => left.evaluate(stack)
    case Some((token, term)) => token match {
      case EqualsToken => BooleanValue(left.evaluate(stack) == term.evaluate(stack))
      case NotEqualsToken => BooleanValue(left.evaluate(stack) != term.evaluate(stack))
      case token => throw MonkeyException("I tried to see if two values are equal and did not expect '" + token + "'")
    }
  }
}
