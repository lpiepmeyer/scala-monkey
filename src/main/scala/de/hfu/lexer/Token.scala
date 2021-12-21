package de.hfu.lexer

sealed abstract class Token( val text:String){
  override def toString: String = text
}

object Token{
  val keywords=List(FunctionToken,LetToken,TrueToken,FalseToken,IfToken,ElseToken,ReturnToken)
    .map(token=>token.toString.toLowerCase->token)
    .toMap
  def lookupIdent(ident :String) :Token =
    keywords.withDefaultValue(IdentifierToken(ident))(ident)
}

case class IllegalToken(literal:String) extends Token(literal)
case object EOFToken extends Token("EOF")
case class IdentifierToken(literal : String) extends Token(literal)
case class IntegerToken(literal : String) extends Token(literal)
case object AssignmentToken extends Token("=")
case object PlusToken extends Token("+")
case object MinusToken extends Token("-")
case object BangToken extends Token("!")
case object AsteriskToken extends Token("*")
case object SlashToken extends Token("/")
case object LessThenToken extends Token("<")
case object GreaterThenToken extends Token(">")
case object EqualsToken extends Token("=")
case object NotEqualsToken extends Token("!=")
case object CommaToken extends Token(",")
case object SemicolonToken extends Token(";")
case object LeftParanthesisToken extends Token("(")
case object RightParanthesisToken extends Token(")")
case object RightBraceToken extends Token("}")
case object LeftBraceToken extends Token("{")

case object FunctionToken extends Token("FN")
case object LetToken extends Token("LET")
case object TrueToken extends Token("TRUE")
case object FalseToken extends Token("FALSE")
case object IfToken extends Token("IF")
case object ElseToken extends Token("ELSE")
case object ReturnToken extends Token("RETURN")
