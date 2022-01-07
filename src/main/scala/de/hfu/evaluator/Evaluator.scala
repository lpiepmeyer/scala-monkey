package de.hfu.evaluator

import de.hfu.lexer._
import de.hfu.parser._

object Evaluator {
  def apply(node: Node, context: Context = new Context): Value =
    evaluateNode(node, context) match {
      case NoValue => NoValue
      case IntegerValue(v: Int) => IntegerValue(v)
      case BooleanValue(v: Boolean) => BooleanValue(v)
      case ReturnValue(NoValue) => NoValue
      case ReturnValue(IntegerValue(v: Int)) => IntegerValue(v)
      case ReturnValue(BooleanValue(v: Boolean)) => BooleanValue(v)
      case _ => throw new RuntimeException
    }

  def evaluateNode(node: Node, context: Context): Value =
    node match {
      case Program(statements) => evaluateStatements(statements, unwrap = true, context)
      case BlockStatement(statements) => evaluateStatements(statements, unwrap = false, context)
      case ExpressionStatement(expression) => evaluateNode(expression, context)
      case IntegerLiteral(v) => IntegerValue(v)
      case BoolLiteral(v) => BooleanValue(v)
      case ReturnStatement(v) => ReturnValue(evaluateNode(v, context))
      case LetStatement(name, expression) => context(name) = evaluateNode(expression, context)
      case Identifier(name) => context(name)
      case PrefixExpression(operator, expression) => evaluate(operator, apply(expression, context))
      case InfixExpression(operator, left, right) => evaluate(operator, apply(left, context), apply(right, context))
      case IfExpression(condition, consequence, alternative) => evaluate(condition, consequence, alternative, context)
      case _ => throw new RuntimeException
    }

  def evaluateStatements(statements: List[Statement], unwrap: Boolean, context: Context): Value = {
    var result: Value = NoValue
    for (statement <- statements) {
      result = evaluateNode(statement, context)
      result match {
        case ReturnValue(v) if unwrap => return v
        case ReturnValue(_) if !unwrap => return result
        case _ =>
      }
    }
    result
  }

  def minus(option: Value): Value = option match {
    case IntegerValue(v: Int) => IntegerValue(-v)
    case _ => NoValue
  }

  def not(option: Value): Value = option match {
    case BooleanValue(v: Boolean) => BooleanValue(!v)
    case NoValue => BooleanValue(true)
    case _ => BooleanValue(false)
  }

  def evaluate(operator: Token, v: Value): Value = operator match {
    case MinusToken => minus(v)
    case BangToken => not(v)
  }


  def evaluate(condition: Expression, consequence: BlockStatement, alternative: Option[BlockStatement], context: Context): Value = apply(condition, context) match {
    case BooleanValue(false) | NoValue if alternative.isDefined => apply(alternative.get, context)
    case BooleanValue(false) | NoValue => NoValue
    case _ => apply(consequence, context)
  }


  def add(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v + w)
  }

  def subtract(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v - w)
  }

  def multiply(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v * w)
  }

  def divide(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => IntegerValue(v / w)
  }

  def equals(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => BooleanValue(v == w)
    case (BooleanValue(v: Boolean), BooleanValue(w: Boolean)) => BooleanValue(v == w)
  }

  def notEquals(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => BooleanValue(v != w)
    case (BooleanValue(v: Boolean), BooleanValue(w: Boolean)) => BooleanValue(v != w)
  }

  def lessThan(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => BooleanValue(v < w)
  }

  def greaterThan(left: Value, right: Value): Value = (left, right) match {
    case (IntegerValue(v: Int), IntegerValue(w: Int)) => BooleanValue(v > w)
  }

  def evaluate(operator: Token, left: Value, right: Value): Value = operator match {
    case PlusToken => add(left, right)
    case MinusToken => subtract(left, right)
    case AsteriskToken => multiply(left, right)
    case SlashToken => divide(left, right)
    case EqualsToken => equals(left, right)
    case NotEqualsToken => notEquals(left, right)
    case LessThanToken => lessThan(left, right)
    case GreaterThanToken => greaterThan(left, right)
    case _ => throw new RuntimeException
  }

}
