package topdown

import de.hfu.monkey._
import de.hfu.monkey.lexer._
import org.scalatest.funsuite.AnyFunSuite

// TODO: add more tests

class ParserTest extends AnyFunSuite {

  private def check(tests: List[String], parser: Lexer => Node): Unit = {
    for (test <- tests) {
      val lexer = Lexer(test)
      val ast = parser(lexer)
      val actual = ast.toString
      assert(normalize(actual) == normalize(test))
      assert(lexer.currentToken == EOFToken)
    }
  }

  private def normalize(text: String) = text.replaceAll("\\s", "")


  test("parse block statement") {
    val testCases = List("{let foo=23; return foo;23;}")
    check(testCases, BlockStatement(_))
  }


  test("parse bool literal") {
    val testCases = List(
      "true",
      "false",
    )
    check(testCases, t => BoolLiteral(t))
  }


  test("parse call  expressions") {
    val tests = List(
      "add(1, 2 * 3, 4 + 5)",
      "add()",
      "add(foobar)",
    )
    check(tests, t => CallExpression(t))
  }


  test("parse comparison") {
    val testCases = List("1<4", "1>4", "foo>bar", "foo<bar")
    check(testCases, Comparison(_))
  }


  test("parse dash term") {
    val tests = List(
      "2+4",
      "2-4",
      "foo + bar",
      "2 + 3 - 4",
      "2 + 3 * 4 - 4 / 5",
      "2 + foo - bar",
      "2 * (3 + 4)",
    )
    check(tests, t => DashTerm(t))
  }


  test("parse equality") {
    val tests = List(
      "1 == 4",
      "true == true",
      "false == 1 > 2",
      "foo==bar"
    )
    check(tests, t => Equality(t))
  }


  test("parse function literal ") {
    val tests = List(
      "fn(a, b){a + b;}",
      "fn(a){2*a ;}",
      "fn(){}"
    )
    check(tests, t => FunctionLiteral(t))
  }


  test("parse identifier") {
    val tests = List("foobar")
    check(tests, t => Identifier(t))
  }


  test("parse if expression") {
    val testCases = List(
      "if (x < y) { x; }",
      "if (x < y) { x; }else{y;}",
    )
    check(testCases, IfExpression(_))
  }


  test("parse integer literal") {
    val testCases = List(
      "23"
    )
    check(testCases, t => IntegerLiteral(t))
  }


  test("parse let statement") {
    val testCases = List(
      "let x = 5;",
      "let y = true;",
      "let foobar = y;"
    )
    check(testCases, t => LetStatement(t))
  }


  test("parse paranthized expression") {
    val testCases = List(
      "(5)",
      "(f(2*3))",
      "(foobar)"
    )
    check(testCases, t => ParanthizedExpression(t))
  }


  test("parse point term") {
    val testCases = List(
      "2*3",
      "foo*bar",
      "foo*3/bar",
      "(2+3)*(5-4)",
    )
    check(testCases, PointTerm(_))
  }


  test("parse primary expression") {
  }


  test("parse program ") {
    val testCases = List(
      "let foo=23; return foo;",
      "let foo=23; 4711;"
    )
    check(testCases, Program(_))
  }


  test("parse return expression") {
    val testCases = List(
      "return 5;",
      "return true;",
      "return foobar;",
    )
    check(testCases, t => ReturnStatement(t))
  }

  test("parse statement") {
  }

  test("parse unary expression") {
    val testCases = List(
      "!5",
      "!!5",
      "!foobar",
      "-foobar",
      "!true",
      "!false"
    )
    check(testCases, Unary(_))
  }

  /*
    test("parse simple infix expression") {
      val testCases = List(
        ("3+4", InfixExpression(PlusToken, IntegerLiteral(3), IntegerLiteral(4))),
        ("3-4", InfixExpression(MinusToken, IntegerLiteral(3), IntegerLiteral(4))),
        ("3*4", InfixExpression(AsteriskToken, IntegerLiteral(3), IntegerLiteral(4))),
        ("3/4", InfixExpression(SlashToken, IntegerLiteral(3), IntegerLiteral(4))),
        ("3<4", InfixExpression(LessThanToken, IntegerLiteral(3), IntegerLiteral(4))),
        ("3>4", InfixExpression(GreaterThanToken, IntegerLiteral(3), IntegerLiteral(4))),
        ("3==4", InfixExpression(EqualsToken, IntegerLiteral(3), IntegerLiteral(4))),
        ("3!=4", InfixExpression(NotEqualsToken, IntegerLiteral(3), IntegerLiteral(4))),
        ("foobar+barfoo", InfixExpression(PlusToken, Identifier("foobar"), Identifier("barfoo"))),
        ("foobar-barfoo", InfixExpression(MinusToken, Identifier("foobar"), Identifier("barfoo"))),
        ("foobar*barfoo", InfixExpression(AsteriskToken, Identifier("foobar"), Identifier("barfoo"))),
        ("foobar/barfoo", InfixExpression(SlashToken, Identifier("foobar"), Identifier("barfoo"))),
        ("foobar<barfoo", InfixExpression(LessThanToken, Identifier("foobar"), Identifier("barfoo"))),
        ("foobar>barfoo", InfixExpression(GreaterThanToken, Identifier("foobar"), Identifier("barfoo"))),
        ("foobar==barfoo", InfixExpression(EqualsToken, Identifier("foobar"), Identifier("barfoo"))),
        ("foobar!=barfoo", InfixExpression(NotEqualsToken, Identifier("foobar"), Identifier("barfoo"))),
      )
      checkExpressions(testCases)
    }


    test("parse infix expressions with precedences") {
      val testCases = List(
        ("-49 + 100 + -50", InfixExpression(PlusToken, InfixExpression(PlusToken, PrefixExpression(MinusToken, IntegerLiteral(49)), IntegerLiteral(100)), PrefixExpression(MinusToken, IntegerLiteral(50)))),
        ("-a * b", InfixExpression(AsteriskToken, PrefixExpression(MinusToken, Identifier("a")), Identifier("b"))),
        ("!-a", PrefixExpression(BangToken, PrefixExpression(MinusToken, Identifier("a")))),
        ("2+3+4", InfixExpression(PlusToken, InfixExpression(PlusToken, IntegerLiteral(2), IntegerLiteral(3)), IntegerLiteral(4))),
        ("2*3+4", InfixExpression(PlusToken, InfixExpression(AsteriskToken, IntegerLiteral(2), IntegerLiteral(3)), IntegerLiteral(4))),
        ("2*(3+4)", InfixExpression(AsteriskToken, IntegerLiteral(2), InfixExpression(PlusToken, IntegerLiteral(3), IntegerLiteral(4)))),
      )
      checkExpressions(testCases)
    }
  */
}