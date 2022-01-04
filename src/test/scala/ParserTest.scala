import de.hfu.lexer._
import de.hfu.parser._
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {

  private def check(testCases: List[(String, Program)]): Unit ={
    for((input, expected)<-testCases){
      val actual=new Parser(new TokenIterator(input)).parseProgram()
      assert(actual==expected)
    }
  }

  test("parse let expression") {
    val testCases=List(
      ("let x = 5;", Program(List(LetStatement("x",IntegerLiteral(5))))),
      ("let y = true;", Program(List(LetStatement("y",BoolLiteral(true))))),
      ("let foobar = y;",Program(List(LetStatement("foobar",Identifier("y")))))
    )
    check(testCases)
  }
  test("parse return expression") {
    val testCases=List(
      ("return 5;", Program(List(ReturnStatement(IntegerLiteral(5))))),
      ("return true;", Program(List(ReturnStatement(BoolLiteral(true))))),
      ("return foobar;",Program(List(ReturnStatement(Identifier("foobar")))))
    )
    check(testCases)
  }
  test("parse integer expression") {
    val testCases=List(
      ("5;", Program(List(ExpressionStatement(IntegerLiteral(5))))),
    )
    check(testCases)
  }
  test("parse boolean expression") {
    val testCases=List(
      ("false;", Program(List(ExpressionStatement(BoolLiteral(false))))),
    )
    check(testCases)
  }
  test("parse identifier expression") {
    val testCases=List(
      ("foobar;", Program(List(ExpressionStatement(Identifier("foobar"))))),
    )
    check(testCases)
  }

  test("parse prefix expression") {
    val testCases=List(
      ("!5;", Program(List(ExpressionStatement(PrefixExpression(BangToken, IntegerLiteral(5)))))),
      ("!!5;", Program(List(ExpressionStatement(PrefixExpression(BangToken,PrefixExpression(BangToken,IntegerLiteral(5))))))),
      ("!foobar;", Program(List(ExpressionStatement(PrefixExpression(BangToken, Identifier("foobar")))))),
      ("-foobar;", Program(List(ExpressionStatement(PrefixExpression(MinusToken, Identifier("foobar")))))),
      ("!true;", Program(List(ExpressionStatement(PrefixExpression(BangToken, BoolLiteral(true)))))),
      ("!false;", Program(List(ExpressionStatement(PrefixExpression(BangToken, BoolLiteral(false)))))),
    )
    check(testCases)
  }

  test("parse simple infix expression") {
    val testCases=List(
      ("3+4;", Program(List(ExpressionStatement(InfixExpression(PlusToken, IntegerLiteral(3), IntegerLiteral(4)))))),
      ("3-4;", Program(List(ExpressionStatement(InfixExpression(MinusToken, IntegerLiteral(3), IntegerLiteral(4)))))),
      ("3*4;", Program(List(ExpressionStatement(InfixExpression(AsteriskToken, IntegerLiteral(3), IntegerLiteral(4)))))),
      ("3/4;", Program(List(ExpressionStatement(InfixExpression(SlashToken, IntegerLiteral(3), IntegerLiteral(4)))))),
      ("3<4;", Program(List(ExpressionStatement(InfixExpression(LessThenToken, IntegerLiteral(3), IntegerLiteral(4)))))),
      ("3>4;", Program(List(ExpressionStatement(InfixExpression(GreaterThenToken, IntegerLiteral(3), IntegerLiteral(4)))))),
      ("3==4;", Program(List(ExpressionStatement(InfixExpression(EqualsToken, IntegerLiteral(3), IntegerLiteral(4)))))),
      ("3!=4;", Program(List(ExpressionStatement(InfixExpression(NotEqualsToken, IntegerLiteral(3), IntegerLiteral(4)))))),
    )
    check(testCases)
  }
}
