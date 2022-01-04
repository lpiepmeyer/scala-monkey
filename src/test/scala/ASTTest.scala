import de.hfu.parser._
import org.scalatest.funsuite.AnyFunSuite

class ASTTest extends AnyFunSuite {

  private def check(testCases: List[(Statement, String)]): Unit ={
    for((statement, expected)<-testCases){
      assert(statement.toString==expected)
    }
  }

  test("test let toString") {
    val testCases=List(
      (LetStatement("x", IntegerLiteral(5)), "let x = 5"),
      (LetStatement("y", BoolLiteral(true)), "let y = true"),
      (LetStatement("y", Identifier("foobar")), "let y = foobar"),
    )
    check(testCases)
  }

  test("test return toString") {
    val testCases=List(
      (ReturnStatement(IntegerLiteral(5)), "return 5"),
      (ReturnStatement(BoolLiteral(true)), "return true"),
      (ReturnStatement(Identifier("foobar")), "return foobar"),
    )
    check(testCases)
  }
}