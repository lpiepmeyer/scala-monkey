package de.hfu.lexer



import java.io.{StreamTokenizer, StringReader}
import scala.collection.mutable.ListBuffer

class Lexer(val input: String) {

  val charToToken = Map('+' -> PlusToken, '-' -> MinusToken, '*' -> AsteriskToken, '/' -> SlashToken, '!' -> BangToken, '(' -> LeftParanthesisToken, ')' -> RightParanthesisToken, '{' -> LeftBraceToken, '}' -> RightParanthesisToken, '<' -> LessThenToken, '>' -> GreaterThenToken, 0 -> EOFToken, ';' -> SemicolonToken, ',' -> CommaToken, '=' -> AssignmentToken)
  val tokenizer = new StreamTokenizer(new StringReader(input))
  var currentToken = tokenizer.nextToken


  def lookahead(found: Token, notFound: Token): Token = {
    currentToken = tokenizer.nextToken()
    if (currentToken != '=') notFound
    nextToken()
    found
  }

  def nextToken(): Token = {
    if (currentToken == '=') lookahead(EqualsToken, AssignmentToken)
    if (currentToken == '!') lookahead(NotEqualsToken, BangToken)

    val result = tokenizer.ttype match {
      case StreamTokenizer.TT_NUMBER => IntegerToken(tokenizer.nval.toInt.toString)
      case StreamTokenizer.TT_WORD => Token.lookupIdent(tokenizer.sval)
      case _ if (charToToken.keySet.contains(currentToken.toChar)) =>
        charToToken(currentToken.toChar)
      case _ => IllegalToken(currentToken.toString)
    }
    currentToken = tokenizer.nextToken()
    result
  }
}

class TokenIterator(val input: String) extends Iterator[(Token, Token)]{
  private val lexer=new Lexer(input)
  private val firstThree =(1 to 3).map(v=>lexer.nextToken()).to(ListBuffer)

  override def hasNext: Boolean = firstThree(1)==EOFToken && firstThree(2)==EOFToken

  override def next(): (Token, Token) = {
    val result=(firstThree(0),firstThree(1))
    firstThree(0)=firstThree(1)
    firstThree(1)=firstThree(2)
    firstThree(2)=lexer.nextToken()
    return result
  }
}