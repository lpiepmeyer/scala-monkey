package de.hfu.parser

import de.hfu.lexer._

import scala.collection.mutable.ListBuffer


object Precedence extends Enumeration {
  type Precedence = Value
  val LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, PREFIX, CALL = Value
}

class Parser(val lexer: TokenIterator) {

  import Precedence._

  private def currentToken: Token = lexer.currentToken
  private def peekToken: Token = lexer.peekToken


  private val precedences = Map[Token, Precedence](
    EqualsToken -> EQUALS,
    NotEqualsToken -> EQUALS,
    LessThanToken -> LESSGREATER,
    GreaterThanToken -> LESSGREATER,
    PlusToken -> SUM,
    MinusToken -> SUM,
    SlashToken -> PRODUCT,
    AsteriskToken -> PRODUCT,
    LeftParenthesisToken -> CALL
  )

  private def nextTokens(): Boolean = {
    lexer.next()
    currentToken != EOFToken
  }


  private def expectPeek(token: Token): Unit ={
    if (peekToken != token)
      throw new RuntimeException
    nextTokens()
  }

  private def parseLet(): LetStatement = {
    if (!nextTokens()) throw new RuntimeException
    (currentToken, peekToken) match {
      case (identifier: IdentifierToken, AssignmentToken) if nextTokens() && nextTokens() =>
        val expression = parseExpression()
        if (peekToken == SemicolonToken) nextTokens()
        LetStatement(identifier.literal, expression)
      case _ => throw new RuntimeException
    }
  }


  private def parseReturn(): ReturnStatement = {
    if (!nextTokens()) throw new RuntimeException
    val expression = parseExpression()
    if (peekToken == SemicolonToken) nextTokens()
    ReturnStatement(expression)
  }


  private def parseExpressionStatement(): ExpressionStatement = {
    val result = ExpressionStatement(parseExpression())
    if (peekToken == SemicolonToken) nextTokens()
    result
  }


  private def parseInfixExpression(left: Expression): Expression = {
    val precedence = curPrecedence()
    val operator = currentToken
    nextTokens()
    val right = parseExpression(precedence)
    InfixExpression(operator, left, right)
  }


  private def parseCallArguments(): List[Expression] = {
    val result: ListBuffer[Expression] = ListBuffer()
    if (peekToken == RightParenthesisToken) {
      nextTokens()
      return result.toList
    }
    do {
      nextTokens()
      result.addOne(parseExpression())
      nextTokens()
    }while (currentToken == CommaToken)
    if (currentToken != RightParenthesisToken) throw new RuntimeException
    result.toList
  }


  private def parseCallExpression(function: Expression): CallExpression = {
    val arguments = parseCallArguments()
    CallExpression(function, arguments)
  }


  private def parseGroupedExpression(): Expression = {
    nextTokens()
    val result = parseExpression()
    expectPeek(RightParenthesisToken)
    result
  }


  private def parseIfExpression(): Expression = {
    expectPeek(LeftParenthesisToken)
    nextTokens()
    val condition = parseExpression()
    expectPeek(RightParenthesisToken)
    expectPeek(LeftBraceToken)
    val consequence = parseBlockStatement()
    if (peekToken == ElseToken) {
      nextTokens()
      expectPeek(LeftBraceToken)
      val alternative = parseBlockStatement()
      return IfExpression(condition, consequence, Some(alternative))
    }
    IfExpression(condition, consequence, None)
  }


  private def parseIdentifier(): Identifier = {
    val result = currentToken match {
      case IdentifierToken(name) => Identifier(name)
      case _ => throw new RuntimeException
    }
    nextTokens()
    result
  }


  private def parseFunctionParameters(): List[Identifier] = {
    val result: ListBuffer[Identifier] = ListBuffer()
    if (peekToken == RightParenthesisToken) {
      nextTokens()
      return result.toList
    }
    do {
      nextTokens()
      result.addOne(parseIdentifier())
    }while (currentToken == CommaToken)
    if (currentToken != RightParenthesisToken) throw new RuntimeException
    result.toList
  }


  private def parseFunctionLiteral(): Expression = {
    expectPeek(LeftParenthesisToken)
    val parameters = parseFunctionParameters()
    expectPeek(LeftBraceToken)
    val body = parseBlockStatement()
    FunctionLiteral(parameters, body)
  }


  private def createInfix(leftExpression: Expression): Expression = currentToken match {
    case PlusToken | MinusToken | SlashToken | AsteriskToken | EqualsToken | NotEqualsToken | LessThanToken | GreaterThanToken => parseInfixExpression(leftExpression)
    case LeftParenthesisToken => parseCallExpression(leftExpression)
    case _ => leftExpression
  }


  private def createPrefix(): Expression = currentToken match {
    case IntegerToken(literal) => IntegerLiteral(literal.toInt)
    case TrueToken => BoolLiteral(true)
    case FalseToken => BoolLiteral(false)
    case IdentifierToken(name) => Identifier(name)
    case LeftParenthesisToken => parseGroupedExpression()
    case IfToken => parseIfExpression()
    case FunctionToken => parseFunctionLiteral()
    case BangToken | MinusToken => parsePrefixExpression()
    case _ => throw new RuntimeException
  }


  private def getPrecedence(token: Token) = precedences.get(token) match {
    case None => LOWEST
    case Some(precedence) => precedence
  }


  private def peekPrecedence(): Precedence = getPrecedence(peekToken)


  private def curPrecedence(): Precedence = getPrecedence(currentToken)


  private def parseExpression(precedence: Precedence = LOWEST): Expression = {
    var leftExpression = createPrefix()
    while (peekToken != SemicolonToken && precedence < peekPrecedence()) {
      nextTokens()
      leftExpression = createInfix(leftExpression)
    }
    leftExpression
  }


  private def parsePrefixExpression(): Expression = {
    val token = currentToken
    if (!nextTokens())
      throw new RuntimeException
    val expression = parseExpression(PREFIX)
    PrefixExpression(token, expression)
  }


  private def addStatement(statements: ListBuffer[Statement], statement: Some[Statement]): Unit = {
    if (statement.isDefined)
      statements.addOne(statement.get)
    else throw new RuntimeException
  }


  private def parseStatement(): Statement = currentToken match {
    case LetToken => parseLet()
    case ReturnToken => parseReturn()
    case _ => parseExpressionStatement()
  }


  def parseProgram(): Program = {
    val result: ListBuffer[Statement] = ListBuffer()
    do {
      addStatement(result, Some(parseStatement()))
    } while (nextTokens())
    Program(result.toList)
  }


  private def parseBlockStatement(): BlockStatement = {
    val statements: ListBuffer[Statement] = ListBuffer()
    while (nextTokens() && currentToken != RightBraceToken && currentToken != EOFToken) {
      addStatement(statements, Some(parseStatement()))
    }
    BlockStatement(statements.toList)
  }

}

