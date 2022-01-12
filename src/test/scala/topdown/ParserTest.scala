package topdown

import de.hfu.lexer._
import de.hfu.topdown._
import org.scalatest.funsuite.AnyFunSuite

// TODO: add more tests

class ParserTest extends AnyFunSuite {

  private def checkInt(testCases: List[(String, IntegerLiteral)]): Unit = {
    for ((input, expected) <- testCases) {
      val lexer = TokenIterator(input)
      val actual = IntegerLiteral(lexer)
      assert(actual == expected)
      assert(lexer.currentToken == SemicolonToken)
    }
  }

  test("parse int literal") {
    val testCases = List(
      ("23;", IntegerLiteral(23)),
    )
    checkInt(testCases)
  }

  private def checkBool(testCases: List[(String, BoolLiteral)]): Unit = {
    for ((input, expected) <- testCases) {
      val lexer = TokenIterator(input)
      val actual = BoolLiteral(lexer)
      assert(actual == expected)
      assert(lexer.currentToken == SemicolonToken)
    }
  }

  test("parse bool literal") {
    val testCases = List(
      ("true;", BoolLiteral(true)),
      ("false;", BoolLiteral(false)),
    )
    checkBool(testCases)
  }


  private def checkIdentifier(testCases: List[(String, Identifier)]): Unit = {
    for ((input, expected) <- testCases) {
      val lexer = TokenIterator(input)
      val actual = Identifier(lexer)
      assert(actual == expected)
      assert(lexer.currentToken == SemicolonToken)
    }
  }

  test("parse identifier literal") {
    val testCases = List(
      ("foobar;", Identifier("foobar")),
    )
    checkIdentifier(testCases)
  }


  private def checkParameterList(testCases: List[(String, ParameterList)]): Unit = {
    for ((input, expected) <- testCases) {
      val lexer = TokenIterator(input)
      val actual = ParameterList(lexer)
      assert(actual == expected)
      assert(lexer.currentToken == SemicolonToken)
    }
  }

  test("parse parameter list ") {
    val testCases = List(
      ("foo, bar;", ParameterList(List(Identifier("foo"), Identifier("bar")))),
      ("x, y, z;", ParameterList(List(Identifier("x"), Identifier("y"), Identifier("z")))),
      ("foo ;", ParameterList(List(Identifier("foo")))),
      ("foo, bar,;", ParameterList(List())),
    )
    checkParameterList(testCases)
  }

  test("parse primary  ") {
    val lexer = TokenIterator("47")
    val actual = Primary(lexer)
    println(actual)
    /*    val testCases = List(
          ("47;", ParameterList(List(Identifier("foo"),Identifier("bar")))),
        )
        checkParameterList(testCases)
      */
  }


  private def check(testCases: List[(String, Program)]): Unit = {
    for ((input, expected) <- testCases) {
      val actual = Program(TokenIterator(input))
      assert(actual == expected)
    }
  }


  private def checkExpressions(testCases: List[(String, Expression)]): Unit = {
    for ((input, expected) <- testCases) {
      val actual = Program(TokenIterator(input + ";"))
      assert(actual == Program(List(ExpressionStatement(expected))))
    }
  }

  test("parse let expression") {
    println(LetStatement(TokenIterator("let x = 5;")))
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


  test("parse if expressions") {
    println(IfExpression(TokenIterator("if (x < y) { x }")))
    /*   val testCases = List(

         ("if (x < y) { x }", IfExpression(InfixExpression(LessThanToken, Identifier("x"), Identifier("y")), BlockStatement(List(ExpressionStatement(Identifier("x")))), None)),
         ("if (x < y) { x }else{y}", IfExpression(InfixExpression(LessThanToken, Identifier("x"), Identifier("y")), BlockStatement(List(ExpressionStatement(Identifier("x")))), Some(BlockStatement(List(ExpressionStatement(Identifier("y"))))))),
       )
       checkExpressions(testCases)

     */
  }


  test("parse function definition expressions") {
    val testCases = List(

      ("fn(x, y) { x + y; }", FunctionLiteral(List(Identifier("x"), Identifier("y")), BlockStatement(List(ExpressionStatement(InfixExpression(PlusToken, Identifier("x"), Identifier("y"))))))),
      ("fn() {}", FunctionLiteral(List(), BlockStatement(List()))),
    )
    checkExpressions(testCases)
  }

  /*
    test("parse call  expressions") {
      val testCases = List(
        ("add(1, 2 * 3, 4 + 5)", CallExpression(Identifier("add"), List(IntegerLiteral(1), InfixExpression(AsteriskToken, IntegerLiteral(2), IntegerLiteral(3)), InfixExpression(PlusToken, IntegerLiteral(4), IntegerLiteral(5))))),
      )
      checkExpressions(testCases)
    }
    */

  private def checkDashTerm(testCases: List[String]): Unit = {
    for (expected <- testCases) {
      val lexer = TokenIterator(expected + ";")
      assert(DashTerm(lexer).toString == expected)
      assert(lexer.currentToken == SemicolonToken)
    }
  }

  test("parse dashterm  ") {
    val testCases = List(
      "foo+bar",
      "2+3-4",
      "2+3*4-4/5",
    )
    checkDashTerm(testCases)
  }


  private def checkPointTerm(testCases: List[String]): Unit = {
    for (expected <- testCases) {
      val lexer = TokenIterator(expected + ";")
      assert(PointTerm(lexer).toString == expected)
      assert(lexer.currentToken == SemicolonToken)
    }
  }

  test("parse pointterm  ") {
    val testCases = List(
      "2*3",
      "foo*bar",
      "foo*3/bar",
      // "(2+3)*(5-4)",
    )
    checkPointTerm(testCases)
  }


  private def checkUnary(testCases: List[String]): Unit = {
    for (expected <- testCases) {
      val lexer = TokenIterator(expected + ";")
      assert(Unary(lexer).toString == expected)
      assert(lexer.currentToken == SemicolonToken)
    }
  }

  test("parse unary  ") {
    val testCases = List(
      "-!-!!23"
    )
    checkUnary(testCases)
  }


  test("parse factor  ") {
    println(PointTerm(TokenIterator("1+2-3+4")))
    /*  val testCases = List(
        ("2*3+4*5", CallExpression(Identifier("add"), List(IntegerLiteral(1), InfixExpression(AsteriskToken, IntegerLiteral(2), IntegerLiteral(3)), InfixExpression(PlusToken, IntegerLiteral(4), IntegerLiteral(5))))),
      )
      checkExpressions(testCases)

     */
  }
  test("parse comparison  ") {
    println(Comparison(TokenIterator("1<4")))
    /*  val testCases = List(
        ("2*3+4*5", CallExpression(Identifier("add"), List(IntegerLiteral(1), InfixExpression(AsteriskToken, IntegerLiteral(2), IntegerLiteral(3)), InfixExpression(PlusToken, IntegerLiteral(4), IntegerLiteral(5))))),
      )
      checkExpressions(testCases)

     */
  }

  test("parse equality  ") {
    println(Equality(TokenIterator("1==4")))
    println(Equality(TokenIterator("true==true")))
    /*  val testCases = List(
        ("2*3+4*5", CallExpression(Identifier("add"), List(IntegerLiteral(1), InfixExpression(AsteriskToken, IntegerLiteral(2), IntegerLiteral(3)), InfixExpression(PlusToken, IntegerLiteral(4), IntegerLiteral(5))))),
      )
      checkExpressions(testCases)

     */
  }

  test("parse block statement  ") {
    println(BlockStatement(TokenIterator("{let foo=23; return foo;}")))

    /*  val testCases = List(
        ("2*3+4*5", CallExpression(Identifier("add"), List(IntegerLiteral(1), InfixExpression(AsteriskToken, IntegerLiteral(2), IntegerLiteral(3)), InfixExpression(PlusToken, IntegerLiteral(4), IntegerLiteral(5))))),
      )
      checkExpressions(testCases)

     */
  }

  test("parse program   ") {
    println(Program(TokenIterator("let foo=23; return foo;")))

    /*  val testCases = List(
        ("2*3+4*5", CallExpression(Identifier("add"), List(IntegerLiteral(1), InfixExpression(AsteriskToken, IntegerLiteral(2), IntegerLiteral(3)), InfixExpression(PlusToken, IntegerLiteral(4), IntegerLiteral(5))))),
      )
      checkExpressions(testCases)

     */
  }

}


