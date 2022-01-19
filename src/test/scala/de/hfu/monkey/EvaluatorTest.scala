package de.hfu.monkey

import de.hfu.monkey.lexer._
import org.scalatest.funsuite.AnyFunSuite

class EvaluatorTest extends AnyFunSuite {


  private def checkBooleanValues(testCases: List[(String, Boolean)]): Unit =
    check(testCases.map(pair => (pair._1, BooleanValue(pair._2))))

  private def check(testCases: List[(String, Value)]): Unit = {
    for ((input, expected) <- testCases) {
      val actual = Program(Lexer(input)).evaluate(Stack())
      assert(actual == expected)
    }
  }

  private def checkIntegerValues(testCases: List[(String, Int)]): Unit =
    check(testCases.map(pair => (pair._1, IntegerValue(pair._2))))


  test("evaluate integer expressions") {
    val testCases = List(
      ("5", 5),
      ("-5", -5),
      ("2+3", 5),
      ("3-2", 1),
      ("1+2+3", 6),
      ("2 * 2 * 2 * 2 * 2", 32),
      ("-49 + 100 + -50", 1),
      ("5 * 2 + 10", 20),
      ("5 + 2 * 10", 25),
      ("20 + 2 * -10", 0),
      ("50 / 2 * 2 + 10", 60),
      ("2 * (5 + 10)", 30),
      ("3 * 3 * 3 + 10", 37),
      ("3 * (3 * 3) + 10", 37),
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    )
    checkIntegerValues(testCases)
  }

  test("evaluate boolean expressions") {
    val testCases = List(
      ("true", true),
      ("false", false),
      ("1 < 2", true),
      ("1 > 2", false),
      ("1 < 1", false),
      ("1 > 1", false),
      ("1 == 1", true),
      ("1 != 1", false),
      ("1 == 2", false),
      ("1 != 2", true),
      ("true == true", true),
      ("false == false", true),
      ("true == false", false),
      ("true != false", true),
      ("false != true", true),
      ("(1 < 2) == true", true),
      ("(1 < 2) == false", false),
      ("(1 > 2) == true", false),
      ("(1 > 2) == false", true),

    )
    checkBooleanValues(testCases)
  }


  test("evaluate if expressions") {
    val testCases = List(
      ("if (true) { 11 }", IntegerValue(11)),
      ("if (false) { 9 }", NoValue),
      ("if (1) { 12 }", IntegerValue(12)),
      ("if (1 < 2) { 13 }", IntegerValue(13)),
      ("if (1 > 2) { 15 }", NoValue),
      ("if (1 > 2) { 14 } else { 21 }", IntegerValue(21)),
      ("if (1 < 2) { 16 } else { 22 }", IntegerValue(16)),
    )
    check(testCases)
  }


  test("evaluate return expressions") {
    val testCases = List(
      ("return 10;", 10),
      ("return 11; 9;", 11),
      ("return 2 * 5; 9;", 10),
      ("9; return 2 * 5; 9;", 10),
      ("if (10 > 1) { return 10; }", 10),
      ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
    )
    checkIntegerValues(testCases)
  }


  test("evaluate boolean literal") {
    val testCases = List(
      ("true", true),
      ("false", false),
    )
    checkBooleanValues(testCases)
  }

  test("evaluate bang operator") {
    val testCases = List(

      ("!true", false),
      ("!false", true),
      ("!!false", false),
      ("!!true", true),
      ("!5", false),
      ("!!5", true),
    )
    checkBooleanValues(testCases)
  }

  test("evaluate let statements") {
    val testCases = List(
      ("let a = 5; a;", 5),
      ("let a = 5 * 5; a;", 25),
      ("let a = 5; let b = a; b;", 5),
      ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    )
    checkIntegerValues(testCases)
  }

  test("evaluate function calls statements") {
    val testCases = List(
      ("let identity = fn(x) { x; }; identity(5);", 5),
      ("let identity = fn(x) { return x; }; identity(5);", 5),
      ("let double = fn(x) { x * 2; }; double(5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
      ("fn(x) { x; }(5)", 5),
    )
    checkIntegerValues(testCases)
  }

}
