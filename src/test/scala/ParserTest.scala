import de.hfu.lexer._
import de.hfu.parser._
import org.scalatest.funsuite.AnyFunSuite

// TODO: add more tests

class ParserTest extends AnyFunSuite {

  private def check(testCases: List[(String, Program)]): Unit = {
    for ((input, expected) <- testCases) {
      val actual = new Parser(new TokenIterator(input)).parseProgram()
      assert(actual == expected)
    }
  }


  private def checkExpressions(testCases: List[(String, Expression)]): Unit = {
    for ((input, expected) <- testCases) {
      val actual = new Parser(new TokenIterator(input + ";")).parseProgram()
      assert(actual == Program(List(ExpressionStatement(expected))))
    }
  }
  /*
    private def checkExpressions(testCases: List[(String, Expression)]): Unit ={
      for((input, expected)<-testCases){
        val actual=new Parser(new TokenIterator(input)).parseExpression()
        assert(actual==expected)
      }
    }
   */

  test("parse let expression") {
    val testCases = List(
      ("let x = 5;", Program(List(LetStatement("x", IntegerLiteral(5))))),
      ("let y = true;", Program(List(LetStatement("y", BoolLiteral(true))))),
      ("let foobar = y;", Program(List(LetStatement("foobar", Identifier("y")))))
    )
    check(testCases)
  }


  test("parse return expression") {
    val testCases = List(
      ("return 5;", Program(List(ReturnStatement(IntegerLiteral(5))))),
      ("return true;", Program(List(ReturnStatement(BoolLiteral(true))))),
      ("return foobar;", Program(List(ReturnStatement(Identifier("foobar")))))
    )
    check(testCases)
  }


  test("parse integer expression") {
    val testCases = List(
      ("5", IntegerLiteral(5)),
    )
    checkExpressions(testCases)
  }


  test("parse boolean expression") {
    val testCases = List(
      ("false", BoolLiteral(false)),
    )
    checkExpressions(testCases)
  }


  test("parse identifier expression") {
    val testCases = List(
      ("foobar", Identifier("foobar")),
    )
    checkExpressions(testCases)
  }


  test("parse prefix expression") {
    val testCases = List(
      ("!5", PrefixExpression(BangToken, IntegerLiteral(5))),
      ("!!5", PrefixExpression(BangToken, PrefixExpression(BangToken, IntegerLiteral(5)))),
      ("!foobar", PrefixExpression(BangToken, Identifier("foobar"))),
      ("-foobar", PrefixExpression(MinusToken, Identifier("foobar"))),
      ("!true", PrefixExpression(BangToken, BoolLiteral(true))),
      ("!false", PrefixExpression(BangToken, BoolLiteral(false)))
    )
    checkExpressions(testCases)
  }


  test("parse simple infix expression") {
    val testCases = List(
      ("3+4", InfixExpression(PlusToken, IntegerLiteral(3), IntegerLiteral(4))),
      ("3-4", InfixExpression(MinusToken, IntegerLiteral(3), IntegerLiteral(4))),
      ("3*4", InfixExpression(AsteriskToken, IntegerLiteral(3), IntegerLiteral(4))),
      ("3/4", InfixExpression(SlashToken, IntegerLiteral(3), IntegerLiteral(4))),
      ("3<4", InfixExpression(LessThenToken, IntegerLiteral(3), IntegerLiteral(4))),
      ("3>4", InfixExpression(GreaterThenToken, IntegerLiteral(3), IntegerLiteral(4))),
      ("3==4", InfixExpression(EqualsToken, IntegerLiteral(3), IntegerLiteral(4))),
      ("3!=4", InfixExpression(NotEqualsToken, IntegerLiteral(3), IntegerLiteral(4))),
      ("foobar+barfoo", InfixExpression(PlusToken, Identifier("foobar"), Identifier("barfoo"))),
      ("foobar-barfoo", InfixExpression(MinusToken, Identifier("foobar"), Identifier("barfoo"))),
      ("foobar*barfoo", InfixExpression(AsteriskToken, Identifier("foobar"), Identifier("barfoo"))),
      ("foobar/barfoo", InfixExpression(SlashToken, Identifier("foobar"), Identifier("barfoo"))),
      ("foobar<barfoo", InfixExpression(LessThenToken, Identifier("foobar"), Identifier("barfoo"))),
      ("foobar>barfoo", InfixExpression(GreaterThenToken, Identifier("foobar"), Identifier("barfoo"))),
      ("foobar==barfoo", InfixExpression(EqualsToken, Identifier("foobar"), Identifier("barfoo"))),
      ("foobar!=barfoo", InfixExpression(NotEqualsToken, Identifier("foobar"), Identifier("barfoo"))),
    )
    checkExpressions(testCases)
  }


  test("parse infix expressions with precedences") {
    val testCases = List(
        ("-49 + 100 + -50", InfixExpression(PlusToken,InfixExpression(PlusToken,PrefixExpression(MinusToken,IntegerLiteral(49)),IntegerLiteral(100)),PrefixExpression(MinusToken,IntegerLiteral(50)))),
        ("-a * b", InfixExpression(AsteriskToken,PrefixExpression(MinusToken,Identifier("a")),Identifier("b"))),
        ("!-a", PrefixExpression(BangToken, PrefixExpression(MinusToken, Identifier("a")))),
        ("2+3+4", InfixExpression(PlusToken, InfixExpression(PlusToken, IntegerLiteral(2), IntegerLiteral(3)), IntegerLiteral(4))),
        ("2*3+4", InfixExpression(PlusToken, InfixExpression(AsteriskToken, IntegerLiteral(2), IntegerLiteral(3)), IntegerLiteral(4))),
        ("2*(3+4)", InfixExpression(AsteriskToken, IntegerLiteral(2), InfixExpression(PlusToken, IntegerLiteral(3), IntegerLiteral(4)))),
      )
        checkExpressions (testCases)
  }


  test("parse if expressions") {
    val testCases = List(

      ("if (x < y) { x }", IfExpression(InfixExpression(LessThenToken, Identifier("x"), Identifier("y")), BlockStatement(List(ExpressionStatement(Identifier("x")))), None)),
      ("if (x < y) { x }else{y}", IfExpression(InfixExpression(LessThenToken, Identifier("x"), Identifier("y")), BlockStatement(List(ExpressionStatement(Identifier("x")))), Some(BlockStatement(List(ExpressionStatement(Identifier("y"))))))),
    )
    checkExpressions(testCases)
  }


  test("parse function definition expressions") {
    val testCases = List(

      ("fn(x, y) { x + y; }", FunctionLiteral(List(Identifier("x"), Identifier("y")), BlockStatement(List(ExpressionStatement(InfixExpression(PlusToken, Identifier("x"), Identifier("y"))))))),
      ("fn() {}", FunctionLiteral(List(), BlockStatement(List()))),
    )
    checkExpressions(testCases)
  }


  test("parse call  expressions") {
    val testCases = List(
      ("add(1, 2 * 3, 4 + 5)", CallExpression(Identifier("add"), List(IntegerLiteral(1), InfixExpression(AsteriskToken, IntegerLiteral(2), IntegerLiteral(3)), InfixExpression(PlusToken, IntegerLiteral(4), IntegerLiteral(5))))),
    )
    checkExpressions(testCases)
  }
}
