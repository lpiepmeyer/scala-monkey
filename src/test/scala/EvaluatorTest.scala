import de.hfu.evaluator.Evaluator
import de.hfu.lexer._
import de.hfu.parser._
import org.scalatest.funsuite.AnyFunSuite

class EvaluatorTest extends AnyFunSuite {


  private def check(testCases: List[(String, AnyVal)]): Unit ={
    for((input, expected)<-testCases){
      val actual=Evaluator(new Parser(new TokenIterator(input)).parseProgram())
      assert(actual==Some(expected))
    }
  }

  test("evaluate integer expressions") {
    val testCases=List(
      ("5", 5),
      ("-5", -5),
      ("2+3", 5),
      ("3-2", 1),
      ("1+2+3", 6),
      ("2 * 2 * 2 * 2 * 2", 32),
      ("-49 + 100 + -50", 1),  // TODO: fix this
      // Program(List(ExpressionStatement(PrefixExpression(-,InfixExpression(+,InfixExpression(+,IntegerLiteral(49),IntegerLiteral(100)),PrefixExpression(-,IntegerLiteral(50)))))))
      ("5 * 2 + 10", 20),
      ("5 + 2 * 10", 25),
      ("20 + 2 * -10", 0),
      ("50 / 2 * 2 + 10", 60),
      ("2 * (5 + 10)", 30),
      ("3 * 3 * 3 + 10", 37),
      ("3 * (3 * 3) + 10", 37),
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    )
    check(testCases)
  }

  test("evaluate boolean literal") {
    val testCases=List(
      ("true", true),
      ("false", false),
    )
    check(testCases)
  }

  test("evaluate bang operator") {
    val testCases=List(

      ("!true", false),
      ("!false", true),
      ("!!false", false),
      ("!!true", true),
      ("!5", false),
      ("!!5", true),
    )
    check(testCases)
  }


}
