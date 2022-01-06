package de.hfu.evaluator

import de.hfu.lexer.{AsteriskToken, BangToken, MinusToken, PlusToken, SlashToken, Token}
import de.hfu.parser._

object Evaluator {
  def apply(node:Node):Option[AnyVal] =
    node match {
      case Program(statements)=>
        statements.map(apply(_)).last
      case ExpressionStatement(expression)=>apply(expression)
      case IntegerLiteral(v)=>Some(v)
      case BoolLiteral(v)=>Some(v)
      case PrefixExpression(operator, expression)=>evaluate(operator, apply(expression))
      case InfixExpression(operator,left,right)=>evaluate(operator, apply(left), apply(right))
      case _ => throw new RuntimeException
    }


  def minus(option: Option[AnyVal]): Option[AnyVal] = option match {
    case Some(v:Int)=>Some(-v)
    case _=>None
  }

  def not(option: Option[AnyVal]): Option[AnyVal] = option match {
    case Some(v:Boolean)=>Some(!v)
    case None=>Some(true)
    case _=>Some(false)
  }

  def evaluate(operator: Token, v: Option[AnyVal]): Option[AnyVal] =operator match {
    case MinusToken =>minus(v)
    case BangToken=>not(v)
  }

  def add(left: Option[AnyVal], right: Option[AnyVal]): Option[AnyVal] = (left, right) match{
    case (Some(v:Int), Some(w:Int))=>Some(v+w)
  }

  def subtract(left: Option[AnyVal], right: Option[AnyVal]): Option[AnyVal] = (left, right) match{
    case (Some(v:Int), Some(w:Int))=>Some(v-w)
  }

  def multiply(left: Option[AnyVal], right: Option[AnyVal]): Option[AnyVal] = (left, right) match{
    case (Some(v:Int), Some(w:Int))=>Some(v*w)
  }

  def divide(left: Option[AnyVal], right: Option[AnyVal]): Option[AnyVal] = (left, right) match{
    case (Some(v:Int), Some(w:Int))=>Some(v/w)
  }

  def evaluate(operator: Token, left: Option[AnyVal], right: Option[AnyVal]): Option[AnyVal] =operator match {
    case PlusToken =>add(left,right)
    case MinusToken=>subtract(left, right)
    case AsteriskToken=>multiply(left,right)
    case SlashToken=>divide(left,right)
    case _=>throw new RuntimeException
  }

}
