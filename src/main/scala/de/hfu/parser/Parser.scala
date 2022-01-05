package de.hfu.parser

import de.hfu.lexer._

import scala.collection.mutable.ListBuffer


object Precedence extends Enumeration {
  type Precedence = Value
  val LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, CALL = Value
}

class Parser(val lexer: TokenIterator) {

  import Precedence._

  private var curToken: Token = null
  private var peekToken: Token = null
  nextTokens()

  private val precedences = Map[Token, Precedence](
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


  private def nextTokens(): Boolean = {
    val pair = lexer.next()
    curToken = pair._1
    peekToken = pair._2
    curToken!=EOFToken
  }

  def parseLet(): LetStatement = {
    if (!nextTokens()) throw new RuntimeException
    (curToken, peekToken) match {
      case (identifier: IdentifierToken, AssignmentToken) if nextTokens() && nextTokens() =>
        val expression = parseExpression()
        if (peekToken == SemicolonToken) nextTokens()
        LetStatement(identifier.literal, expression)
      case _ => throw new RuntimeException
    }
  }

  def parseReturn(): ReturnStatement = {
    if (!nextTokens()) throw new RuntimeException
    val expression = parseExpression()
    if (peekToken == SemicolonToken) nextTokens()
    ReturnStatement(expression)
  }

  def parseExpressionStatement(): ExpressionStatement = {
    val result = ExpressionStatement(parseExpression())
    if (peekToken == SemicolonToken) nextTokens()
    result
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
    nextTokens()
    val result =parseExpression()
  if(peekToken != RightParanthesisToken )
    throw new RuntimeException
  nextTokens()

     result
  }

  def parseIfExpression(): Expression = {

    if(peekToken!=LeftParanthesisToken) throw new RuntimeException
    nextTokens()
    nextTokens()
    val condition=parseExpression()
    if(peekToken!=RightParanthesisToken) throw new RuntimeException
    nextTokens()
    if(peekToken!=LeftBraceToken) throw new RuntimeException
    nextTokens()
    val consequence = parseBlockStatement()
    if(peekToken==ElseToken) {
      nextTokens()
      if(peekToken!=LeftBraceToken) throw new RuntimeException
      nextTokens()
      val alternative=parseBlockStatement()
      return IfExpression(condition, consequence, Some(alternative))
    }
    IfExpression(condition, consequence, None)
  }

  def parseIdentifier() : Identifier={
    val result=curToken match {
      case IdentifierToken(name)=>Identifier(name)
      case _=>throw new RuntimeException
    }
    nextTokens()
    result
  }

  def parseFunctionParameters():List[Identifier] = {
    val result: ListBuffer[Identifier] = ListBuffer()
    if (peekToken==RightParanthesisToken) {
      nextTokens()
      return result.toList
    }
  //  nextTokens()
    nextTokens()
    result.addOne(parseIdentifier())
    while (curToken==CommaToken){
      nextTokens()
      result.addOne(parseIdentifier())
    }
    if (curToken!=RightParanthesisToken) throw new RuntimeException
    result.toList
  }

  def parseFunctionLiteral(): Expression = {
    if(peekToken!=LeftParanthesisToken) throw new RuntimeException
    nextTokens()
    val parameters=parseFunctionParameters()
    if(peekToken!=LeftBraceToken) throw new RuntimeException
    nextTokens()
    val body=parseBlockStatement()
    FunctionLiteral(parameters, body)
  }

  def createInfix(leftExpression: Expression): Expression = curToken match {
    case PlusToken | MinusToken | SlashToken | AsteriskToken | EqualsToken | NotEqualsToken | LessThenToken | GreaterThenToken => parseInfixExpression(leftExpression)
    case LeftParanthesisToken => parseCallExpression()
    case _ => leftExpression
  }

  def createPrefix(): Expression = curToken match {
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


  private def getPrecedence(token: Token) = precedences.get(token) match {
    case None => LOWEST
    case Some(precedence) => precedence
  }

  def peekPrecedence(): Precedence = getPrecedence(peekToken)

  def curPrecedence(): Precedence = getPrecedence(curToken)


  def parseExpression(precedence: Precedence=LOWEST): Expression = {
    var leftExpression = createPrefix()

    while (peekToken != SemicolonToken && precedence < peekPrecedence()) {
      nextTokens()
      leftExpression = createInfix(leftExpression)
    }
    leftExpression
  }

  def parsePrefixExpression(): Expression = {
    val token = curToken
    if (!nextTokens())
      throw new RuntimeException
    val expression = parseExpression()
    PrefixExpression(token, expression)
  }

  def addStatement(statements: ListBuffer[Statement], statement: Some[Statement]): Unit = {
    if ( statement.isDefined)
      statements.addOne(statement.get)
    else throw new RuntimeException
  }

def parseStatement(): Statement = curToken match {
    case LetToken => parseLet()
    case ReturnToken => parseReturn()
    case _ => parseExpressionStatement()
}


  def parseProgram(): Program = {
    val result: ListBuffer[Statement] = ListBuffer()
    do {
       addStatement(result, Some(parseStatement()))
    }while (nextTokens())
    Program(result.toList)
  }


  def parseBlockStatement() :BlockStatement ={
    val statements: ListBuffer[Statement] = ListBuffer()
    while( nextTokens() && curToken!=RightBraceToken && curToken!=EOFToken) {
      addStatement(statements, Some(parseStatement()))
    }
    BlockStatement(statements.toList)
  }

}

