package de.hfu.topdown

import de.hfu.topdown.lexer._

abstract class Node {
  def evaluate(context: Context): Value
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
    lexer.nextToken()
    val result = ParanthizedExpression(Expression(lexer))
    lexer.expectCurrent(RightParenthesisToken)
    result
  }
}

case class ParanthizedExpression(expression: Expression) extends Expression {
  override def toString: String = "(" + expression + ")"

  override def evaluate(context: Context): Value = expression.evaluate(context)
}

object Program {
  def apply(lexer: Lexer): Program = lexer.currentToken match {
    case EOFToken => Program(List())
    case _ => Program(StatementList(lexer, EOFToken))
  }
}

case class Program(statements: List[Statement]) extends Node {
  override def toString: String = statements.mkString(";\n") + (if (statements.nonEmpty) ";")

  override def evaluate(context: Context): Value =
    statements
      .map(_.evaluate(context))
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
    case _ => "{\n\t" + statements.mkString(";\n") + (if (statements.nonEmpty) ";") + "\n}"
  }

  override def evaluate(context: Context): Value =
    statements
      .map(_.evaluate(context))
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
    if (lexer.currentToken != LeftBraceToken) throw new RuntimeException("found " + lexer.currentToken + " expected " + LeftBraceToken)
    val consequence = BlockStatement(lexer)
    if (lexer.currentToken == ElseToken) {
      lexer.nextToken()
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

  override def evaluate(context: Context): Value = condition.evaluate(context) match {
    case BooleanValue(false) | NoValue if alternative.isDefined => alternative.get.evaluate(context)
    case BooleanValue(false) | NoValue => NoValue
    case _ => consequence.evaluate(context)
  }

}

object LetStatement {
  def apply(lexer: Lexer): LetStatement = {
    lexer.nextToken()
    val identifier = Identifier(lexer)
    lexer.expectCurrent(AssignmentToken)
    val expression = Expression(lexer)
    val result = LetStatement(identifier.value, expression)
    lexer.expectCurrent(SemicolonToken)
    result
  }
}

case class LetStatement(name: String, expression: Expression) extends Statement {
  override def toString: String = "let " + name + " = " + expression.toString + ";"

  override def evaluate(context: Context): Value = context(name) = expression.evaluate(context)
}

object ReturnStatement {
  def apply(lexer: Lexer): ReturnStatement = {
    if (!lexer.nextToken()) throw new RuntimeException
    val expression = Expression(lexer)
    lexer.expectCurrent(SemicolonToken)
    ReturnStatement(expression)
  }
}

case class ReturnStatement(expression: Expression) extends Statement {
  override def toString: String = "return " + expression + ";"

  override def evaluate(context: Context): Value = ReturnValue(expression.evaluate(context))
}

object ExpressionStatement {
  def apply(lexer: Lexer): ExpressionStatement = {
    val result = ExpressionStatement(Expression(lexer))
    if (lexer.currentToken == SemicolonToken) lexer.nextToken()
    result
  }
}

case class ExpressionStatement(expression: Expression) extends Statement {
  override def toString: String = expression.toString

  override def evaluate(context: Context): Value = expression.evaluate(context)
}

object BoolLiteral {
  def apply(lexer: Lexer): BoolLiteral = {
    val result = lexer.currentToken match {
      case TrueToken => BoolLiteral(true)
      case FalseToken => BoolLiteral(false)
    }
    lexer.nextToken()
    result
  }
}

case class BoolLiteral(value: Boolean) extends Expression {
  override def toString: String = value.toString

  def evaluate() = BooleanValue(value)

  override def evaluate(context: Context): Value = BooleanValue(value)
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

  def evaluate(context: Context) = IntegerValue(value)

}

object Identifier {
  def apply(lexer: Lexer): Identifier = {
    val result = lexer.currentToken match {
      case IdentifierToken(name) => Identifier(name)
      case _ => throw new RuntimeException
    }
    lexer.nextToken()
    result
  }
}

case class Identifier(value: String) extends Expression {
  override def toString: String = value

  override def evaluate(context: Context): Value = context(value) match {
    case None => throw new RuntimeException
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

  override def evaluate(context: Context): Value = FunctionValue(parameters, body)
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
      case _ => CallExpression(lexer)
    }
    Primary(expression)
  }
}

case class Primary(expression: Expression) extends Expression {
  override def toString: String = expression.toString

  override def evaluate(context: Context): Value = expression.evaluate(context)
}


object CallExpression {

  def build(lexer: Lexer, head: Expression): List[Expression] = lexer.currentToken match {
    case CommaToken =>
      lexer.nextToken()
      head :: build(lexer, Expression(lexer))
    case _ => List(head)
  }


  def apply(lexer: Lexer): CallExpression = {
    val function = Identifier(lexer)
    lexer.expectCurrent(LeftParenthesisToken)
    val arguments = lexer.currentToken match {
      case RightParenthesisToken => List()
      case _ => build(lexer, Expression(lexer))
    }
    lexer.expectCurrent(RightParenthesisToken)
    lexer.nextToken()
    CallExpression(function, arguments)
  }
}

case class CallExpression(function: Expression, arguments: List[Expression]) extends Expression {
  override def toString: String = function.toString + "(" + arguments.mkString(", ") + ")"

  override def evaluate(context: Context): Value = {
    val evaluatedArguments = arguments.map(_.evaluate(context))
    val result = function.evaluate(context) match {
      case FunctionValue(parameters, body) =>
        val variables = parameters.map(_.value).zip(evaluatedArguments)
        val innerContext = context.extend(collection.mutable.Map(variables: _*))
        body.evaluate(innerContext)
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

  override def evaluate(context: Context): Value = right.foldLeft(left.evaluate(context))((result: Value, pair) => (pair._1, pair._2.evaluate(context)) match {
    case (AsteriskToken, t: IntegerValue) => multiply(result, t)
    case (SlashToken, t: IntegerValue) => divide(result, t)
  })

  private def multiply(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v * w)
  }

  private def divide(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v / w)
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

  override def evaluate(context: Context): Value =
    right.foldLeft(left.evaluate(context))((result: Value, pair) => (pair._1, pair._2.evaluate(context)) match {
      case (MinusToken, t: IntegerValue) => subtract(result, t)
      case (PlusToken, t: IntegerValue) => add(result, t)
    })

  private def add(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v + w)
  }

  private def subtract(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v - w)
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
    val pointTerm = Primary(lexer)
    Unary(prefixes, pointTerm)
  }
}

case class Unary(prefixes: List[Token], primary: Primary) extends Expression {
  override def toString: String = prefixes.mkString("") + primary.toString

  override def evaluate(context: Context): Value = prefixes.foldLeft(primary.evaluate(context))((term, token) => token match {
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

  override def evaluate(context: Context): Value = right match {
    case None => left.evaluate(context)
    case Some((token, right)) => token match {
      case LessThanToken => lessThan(left.evaluate(context), right.evaluate(context))
      case GreaterThanToken => greaterThan(left.evaluate(context), right.evaluate(context))
    }
  }

  private def lessThan(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => BooleanValue(v < w)
  }

  private def greaterThan(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => BooleanValue(v > w)
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

  override def evaluate(context: Context): Value = right match {
    case None => left.evaluate(context)
    case Some((token, term)) => token match {
      case EqualsToken => BooleanValue(left.evaluate(context) == term.evaluate(context))
      case NotEqualsToken => BooleanValue(left.evaluate(context) != term.evaluate(context))
    }
  }
}
