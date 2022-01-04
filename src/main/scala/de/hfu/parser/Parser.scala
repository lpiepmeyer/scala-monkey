package de.hfu.parser

import de.hfu.lexer._

import scala.collection.mutable.ListBuffer


object Precedence extends Enumeration {
  type Precedence = Value
  val LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, CALL = Value
}

class Parser(val lexer: TokenIterator) {

  import Precedence._

  var errors: ListBuffer[String] = ListBuffer()
  var curToken: Token = null
  var peekToken: Token = null


  val precedences = Map[Token, Precedence](
    EqualsToken -> EQUALS,
    NotEqualsToken -> EQUALS,
    LessThenToken -> LESSGREATER,
    GreaterThenToken -> LESSGREATER,
    PlusToken -> SUM,
    MinusToken -> SUM,
    SlashToken -> PRODUCT,
    AsteriskToken -> PRODUCT,
    LeftParanthesisToken -> CALL
  )
  

  def nextTokens(): Boolean = {
    if (!lexer.hasNext) return false
    val pair = lexer.next()
    curToken = pair._1
    peekToken = pair._2
    true
  }

  def parseLet() = {
    if (!nextTokens()) throw new RuntimeException
    (curToken, peekToken) match {
      case (identifier: IdentifierToken, AssignmentToken) if nextTokens() && nextTokens() =>
        val expression = parseExpression(LOWEST)
        LetStatement(identifier.literal, expression)
      case _ => throw new RuntimeException
    }
  }

  def parseReturn() = {
    if (!nextTokens()) throw new RuntimeException
    val expression = parseExpression(LOWEST)
    ReturnStatement(expression)
  }

  def parseExpressionStatement(): ExpressionStatement = {
    val result = ExpressionStatement(parseExpression(LOWEST))
    if (peekToken != SemicolonToken) throw new RuntimeException
    return result
  }

  def parseInfixExpression(left: Expression): Expression = {
    val precedence = curPrecedence()
    val operator = curToken
    nextTokens()
    val right = parseExpression(precedence)
    InfixExpression(operator, left, right)
  }

  def parseCallExpression(): Expression = {
    throw new RuntimeException
  }

  def parseGroupedExpression(): Expression = {
    throw new RuntimeException
  }

  def parseIfExpression(): Expression = {
    throw new RuntimeException
  }

  def parseFunctionLiteral(): Expression = {
    throw new RuntimeException
  }

  def createInfix(leftExpression: Expression) = curToken match {
    case PlusToken | MinusToken | SlashToken | AsteriskToken | EqualsToken | NotEqualsToken | LessThenToken | GreaterThenToken => parseInfixExpression(leftExpression)
    case LeftParanthesisToken => parseCallExpression()
    case _ => leftExpression
  }

  def createPrefix() = curToken match {
    case IntegerToken(literal) => IntegerLiteral(literal.toInt)
    case TrueToken => BoolLiteral(true)
    case FalseToken => BoolLiteral(false)
    case IdentifierToken(name) => Identifier(name)
    case LeftParanthesisToken => parseGroupedExpression()
    case IfToken => parseIfExpression()
    case FunctionToken => parseFunctionLiteral()
    case BangToken | MinusToken => parsePrefixExpression()
    case _ => throw new RuntimeException
  }


  def getPrecedence(token: Token) = precedences.get(token) match {
    case None => LOWEST
    case Some(precedence) => precedence
  }

  def peekPrecedence(): Precedence = getPrecedence(peekToken)

  def curPrecedence(): Precedence = getPrecedence(curToken)


  def parseExpression(precedence: Precedence): Expression = {
    var leftExpression = createPrefix()

    while (peekToken != SemicolonToken && precedence < peekPrecedence()) {
      nextTokens()
      leftExpression = createInfix(leftExpression)
    }
    leftExpression
  }

  def parsePrefixExpression(): Expression = {
    val token = curToken
    if (!nextTokens()) throw new RuntimeException
    val expression = parseExpression(LOWEST)
    PrefixExpression(token, expression)
  }

  def addStatement(statements: ListBuffer[Statement], statement: Some[Statement]): Unit = {
    if (peekToken == SemicolonToken && statement.isDefined)
      statements.addOne(statement.get)
    else throw new RuntimeException
  }


  def parseProgram(): Program = {
    val result: ListBuffer[Statement] = ListBuffer()
    while (nextTokens()) {
      curToken match {
        case LetToken =>
          val statement = parseLet()
          addStatement(result, Some(statement))
        case ReturnToken =>
          val statement = parseReturn()
          addStatement(result, Some(statement))
        case _ =>
          val statement = parseExpressionStatement()
          addStatement(result, Some(statement))
      }
    }
    Program(result.toList)
  }

}

