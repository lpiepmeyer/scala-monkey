import de.hfu.lexer._
import org.scalatest.funsuite.AnyFunSuite

class TokenIteratorTest extends AnyFunSuite {
  private val code =
    """let five = 5;
      |let ten = 10;
      |
      |let add = fn(x, y) {
      |  x + y;
      |};
      |
      |let result = add(five, ten);
      |!-/*5;
      |5 < 10 > 5;
      |
      |if (5 < 10) {
      |	return true;
      |} else {
      |	return false;
      |}
      |
      |10 == 10;
      |10 != 9;
      |2-4;""".stripMargin
  val expected = List(
    LetToken,
    IdentifierToken("five"),
    AssignmentToken,
    IntegerToken("5"),
    SemicolonToken,
    LetToken,
    IdentifierToken("ten"),
    AssignmentToken,
    IntegerToken("10"),
    SemicolonToken,
    LetToken,
    IdentifierToken("add"),
    AssignmentToken,
    FunctionToken,
    LeftParenthesisToken,
    IdentifierToken("x"),
    CommaToken,
    IdentifierToken("y"),
    RightParenthesisToken,
    LeftBraceToken,
    IdentifierToken("x"),
    PlusToken,
    IdentifierToken("y"),
    SemicolonToken,
    RightBraceToken,
    SemicolonToken,
    LetToken,
    IdentifierToken("result"),
    AssignmentToken,
    IdentifierToken("add"),
    LeftParenthesisToken,
    IdentifierToken("five"),
    CommaToken,
    IdentifierToken("ten"),
    RightParenthesisToken,
    SemicolonToken,
    BangToken,
    MinusToken,
    SlashToken,
    AsteriskToken,
    IntegerToken("5"),
    SemicolonToken,
    IntegerToken("5"),
    LessThenToken,
    IntegerToken("10"),
    GreaterThenToken,
    IntegerToken("5"),
    SemicolonToken,
    IfToken,
    LeftParenthesisToken,
    IntegerToken("5"),
    LessThenToken,
    IntegerToken("10"),
    RightParenthesisToken,
    LeftBraceToken,
    ReturnToken,
    TrueToken,
    SemicolonToken,
    RightBraceToken,
    ElseToken,
    LeftBraceToken,
    ReturnToken,
    FalseToken,
    SemicolonToken,
    RightBraceToken,
    IntegerToken("10"),
    EqualsToken,
    IntegerToken("10"),
    SemicolonToken,
    IntegerToken("10"),
    NotEqualsToken,
    IntegerToken("9"),
    SemicolonToken,
    IntegerToken("2"),
    MinusToken,
    IntegerToken("4")
  )

  test("all tokens") {
    for((i,(token,_)) <- Iterator.from(0).zip(new TokenIterator(code).toSeq)){
      assert(token==expected(i))
    }
  }
}
