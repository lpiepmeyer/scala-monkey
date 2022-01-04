import de.hfu.lexer._
import org.scalatest.funsuite.AnyFunSuite

class TokenIteratorTest extends AnyFunSuite {
  val code =
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
    LeftParanthesisToken,
    IdentifierToken("x"),
    CommaToken,
    IdentifierToken("y"),
    RightParanthesisToken,
    LeftBraceToken,
    IdentifierToken("x"),
    PlusToken,
    IdentifierToken("y"),
    SemicolonToken,
    RightParanthesisToken,
    SemicolonToken,
    LetToken,
    IdentifierToken("result"),
    AssignmentToken,
    IdentifierToken("add"),
    LeftParanthesisToken,
    IdentifierToken("five"),
    CommaToken,
    IdentifierToken("ten"),
    RightParanthesisToken,
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
    LeftParanthesisToken,
    IntegerToken("5"),
    LessThenToken,
    IntegerToken("10"),
    RightParanthesisToken,
    LeftBraceToken,
    ReturnToken,
    TrueToken,
    SemicolonToken,
    RightParanthesisToken,
    ElseToken,
    LeftBraceToken,
    ReturnToken,
    FalseToken,
    SemicolonToken,
    RightParanthesisToken,
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
