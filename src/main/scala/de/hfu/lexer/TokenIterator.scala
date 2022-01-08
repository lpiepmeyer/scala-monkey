package de.hfu.lexer

import de.hfu.lexer.Precedence._

import java.io.{Reader, StreamTokenizer, StringReader}
import scala.collection.mutable.ListBuffer

object TokenIterator {
  def apply(reader: Reader): TokenIterator = new TokenIterator(new StreamTokenizer(reader))

  def apply(input: String): TokenIterator = apply(new StringReader(input))
}

class TokenIterator private(val tokenizer: StreamTokenizer) extends Iterator[(Token, Token)] {

  private class Lexer(val tokenizer: StreamTokenizer) {

    private val charToToken = Map('+' -> PlusToken, '-' -> MinusToken, '*' -> AsteriskToken, '/' -> SlashToken, '!' -> BangToken, '(' -> LeftParenthesisToken, ')' -> RightParenthesisToken, '{' -> LeftBraceToken, '}' -> RightBraceToken, '<' -> LessThanToken, '>' -> GreaterThanToken, 0 -> EOFToken, ';' -> SemicolonToken, ',' -> CommaToken, '=' -> AssignmentToken)
    tokenizer.slashStarComments(false)
    tokenizer.slashSlashComments(false)
    tokenizer.ordinaryChar('/')
    tokenizer.ordinaryChar('-')
    private var currentToken = tokenizer.nextToken


    private def lookahead(found: Token, notFound: Token): Token = {
      currentToken = tokenizer.nextToken()
      if (currentToken != '=') return notFound
      nextToken()
      found
    }

    def nextToken(): Token = {
      if (currentToken == '=') return lookahead(EqualsToken, AssignmentToken)
      if (currentToken == '!') return lookahead(NotEqualsToken, BangToken)

      val result = tokenizer.ttype match {
        case _ if charToToken.keySet.contains(currentToken.toChar) =>
          charToToken(currentToken.toChar)
        case StreamTokenizer.TT_NUMBER => IntegerToken(tokenizer.nval.toInt.toString)
        case StreamTokenizer.TT_WORD => Token.lookupIdent(tokenizer.sval)
        case StreamTokenizer.TT_EOF=>EOFToken
        case _ => IllegalToken(currentToken.toString)
      }
      currentToken = tokenizer.nextToken()
      result
    }
  }

  private val lexer = new Lexer(tokenizer)
  private val firstThree = (1 to 3).map(_ => lexer.nextToken()).to(ListBuffer)

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

  def peekPrecedence(): Precedence = getPrecedence(peekToken)

  def currentToken: Token = firstThree.head

  def peekToken: Token = firstThree(1)

  def curPrecedence(): Precedence = getPrecedence(currentToken)

  private def getPrecedence(token: Token) = precedences.get(token) match {
    case None => LOWEST
    case Some(precedence) => precedence
  }

  def expectPeek(token: Token): Unit = {
    if (peekToken != token)
      throw new RuntimeException
    nextTokens()
  }

  def nextTokens(): Boolean = {
    next()
    currentToken != EOFToken
  }

  override def hasNext: Boolean =
    !(firstThree(1) == EOFToken && firstThree(2) == EOFToken)

  override def next(): (Token, Token) = {
    val result = (firstThree.head, firstThree(1))
    firstThree(0) = firstThree(1)
    firstThree(1) = firstThree(2)
    firstThree(2) = lexer.nextToken()
    result
  }
}