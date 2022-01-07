package de.hfu.evaluator

import de.hfu.lexer._
import de.hfu.parser._

object Evaluator {


  def apply(node: Node, context: Context): Value = node match {
    case Program(statements) => evaluateStatements(statements, unwrap = true, context)
    case BlockStatement(statements) => evaluateStatements(statements, unwrap = false, context)
    case ExpressionStatement(expression) => apply(expression, context)
    case IntegerLiteral(v) => IntegerValue(v)
    case BoolLiteral(v) => BooleanValue(v)
    case FunctionLiteral(parameters, body) => FunctionValue(parameters, body)
    case ReturnStatement(v) => ReturnValue(apply(v, context))
    case LetStatement(name, expression) => context(name) = apply(expression, context)
    case Identifier(name) => context(name) match {
      case None => throw new RuntimeException
      case Some(v) => v
    }
    case PrefixExpression(operator, expression) => evaluatePrefix(operator, apply(expression, context))
    case InfixExpression(operator, left, right) => evaluateInfix(operator, apply(left, context), apply(right, context))
    case IfExpression(condition, consequence, alternative) => evaluateIf(condition, consequence, alternative, context)
    case CallExpression(identifier, arguments) => evaluateCall(identifier, arguments, context)
    case _ => throw new RuntimeException
  }

  def evaluateStatements(statements: List[Statement], unwrap: Boolean, context: Context): Value = {
    var result: Value = NoValue
    for (statement <- statements) {
      result = apply(statement, context)
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

  def evaluatePrefix(operator: Token, v: Value): Value = operator match {
    case MinusToken => minus(v)
    case BangToken => not(v)
  }


  def evaluateIf(condition: Expression, consequence: BlockStatement, alternative: Option[BlockStatement], context: Context): Value = apply(condition, context) match {
    case BooleanValue(false) | NoValue if alternative.isDefined => apply(alternative.get, context)
    case BooleanValue(false) | NoValue => NoValue
    case _ => apply(consequence, context)
  }


  def evaluateCall(identifier: Expression, arguments: List[Expression], outerContext: Context): Value = {
    val evaluatedArguments = arguments.map(apply(_, outerContext))
    apply(identifier, outerContext) match {
      case FunctionValue(parameters, body) =>
        val variables = parameters.map(_.value).zip(evaluatedArguments)
        val context = outerContext.extend(collection.mutable.Map(variables: _*))
        apply(body, context)
      case _ => throw new RuntimeException
    }
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

  def evaluateInfix(operator: Token, left: Value, right: Value): Value = operator match {
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
