package de.hfu.monkey.lexer

import de.hfu.monkey.MonkeyException

import java.io.{Reader, StreamTokenizer, StringReader}

object Lexer {
  def apply(input: String): Lexer = apply(new StringReader(input))

  def apply(reader: Reader): Lexer = new Lexer(new StreamTokenizer(reader))
}


class Lexer(val tokenizer: StreamTokenizer) {

  private val charToToken = Map('+' -> PlusToken, '-' -> MinusToken, '*' -> AsteriskToken, '/' -> SlashToken, '!' -> BangToken, '(' -> LeftParenthesisToken, ')' -> RightParenthesisToken, '{' -> LeftBraceToken, '}' -> RightBraceToken, '<' -> LessThanToken, '>' -> GreaterThanToken, 0 -> EOFToken, ';' -> SemicolonToken, ',' -> CommaToken, '=' -> AssignmentToken)
  tokenizer.slashStarComments(true)
  tokenizer.slashSlashComments(true)
  tokenizer.ordinaryChar('/')
  tokenizer.ordinaryChar('-')
  private var monkeyToken = next()

  def currentToken: Token = monkeyToken

  def expectCurrent(expectedToken: Token): Unit = {
    if (monkeyToken != expectedToken)
      throw MonkeyException(expectedToken, monkeyToken)
    nextToken()
  }

  def nextToken(): Boolean = {
    monkeyToken = next()
    monkeyToken != EOFToken
  }

  private def next(): Token = tokenizer.nextToken() match {
    case '=' => lookahead(EqualsToken, AssignmentToken)
    case '!' => lookahead(NotEqualsToken, BangToken)
    case c =>
      val result = toMonkeyToken(c)
      result
  }

  private def lookahead(found: Token, notFound: Token): Token = tokenizer.nextToken() match {
    case '=' =>
      found
    case _ =>
      tokenizer.pushBack()
      notFound
  }

  def skipToken(token: Token) = if (currentToken == token) nextToken()

  private def toMonkeyToken(simpleToken: Int): Token = tokenizer.ttype match {
    case _ if charToToken.keySet.contains(simpleToken.toChar) => (charToToken(simpleToken.toChar))
    case StreamTokenizer.TT_NUMBER => IntegerToken(tokenizer.nval.toInt.toString)
    case StreamTokenizer.TT_WORD => Token.lookupIdent(tokenizer.sval)
    case StreamTokenizer.TT_EOF => EOFToken
    case _ => throw MonkeyException("I tried to read your code, but found the illegal token '" + simpleToken.toChar + "'")
  }

}