package de.hfu.lexer

class OldLexer(val input: String) {

  val charToToken = Map('+' -> PlusToken, '-' -> MinusToken, '*' -> AsteriskToken, '/' -> SlashToken, '!' -> BangToken, '(' -> LeftParanthesisToken, ')' -> RightParanthesisToken, '{' -> LeftBraceToken, '}' -> RightParanthesisToken, '<' -> LessThenToken, '>' -> GreaterThenToken, 0 -> EOFToken, ';' -> SemicolonToken, ',' -> CommaToken,'='->AssignmentToken)
  var position = 0 // current position in input (points to current char)
  var readPosition = 0 // current reading position in input (after current char)
  var ch: Char = 0 // current char under examination
  readChar()

  def isDigit(c: Char) = c.isDigit

  def isLetter(c: Char) = c.isLetter

  def readChar() {
    if (readPosition >= input.length) {
      ch = 0
    } else {
      ch = input(readPosition)
    }
    position = readPosition
    readPosition += 1
  }

  def peekChar(): Char =
    if (readPosition >= input.length)
      0
    else
      input(readPosition)

  def skipWhitespace() {
    while (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
      readChar()
    }
  }

  def read(predicate: Char => Boolean): String = {
    val to = input.indexWhere(c => !predicate(c), position)
    val result = input.substring(position, to)
    position = to
    readPosition = position
    result
  }

  def readNumber() = read(isDigit)

  def readIdentifier() = read(isLetter)

  def tokenFactory(): Token =
    if (ch == '=' && peekChar() == '=') {
      readChar()
      EqualsToken
    } else if (ch == '!' && peekChar() == '=') {
      readChar()
      NotEqualsToken
    }
    else if (charToToken.keySet.contains(ch)) {
      charToToken(ch)
    } else if (isLetter(ch)) {
      val identifier = readIdentifier()
      Token.lookupIdent(identifier)
    } else if (isDigit(ch)) {
      val number = readNumber()
      IntegerToken(number)
    } else {
      IllegalToken(ch.toString)
    }

  def nextToken(): Token = {
    skipWhitespace()
    val result = tokenFactory()
    readChar()
    result
  }
}

