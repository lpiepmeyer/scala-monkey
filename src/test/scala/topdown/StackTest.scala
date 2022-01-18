package topdown

import de.hfu.monkey._
import org.scalatest.funsuite.AnyFunSuite

class StackTest extends AnyFunSuite {


  test("evaluate get") {
    val stack = Stack()
    stack("a") = IntegerValue(3)
    assert(stack("a") == Some(IntegerValue(3)))
  }
  test("evaluate update") {
    val stack = Stack()
    stack("a") = IntegerValue(3)
    assert(stack("a") == Some(IntegerValue(3)))
    stack("a") = IntegerValue(5)
    assert(stack("a") == Some(IntegerValue(5)))
  }

  test("evaluate extend") {
    val stack = Stack()
    stack("a") = IntegerValue(3)
    assert(stack("a") == Some(IntegerValue(3)))
    val extended = stack.extend(List(("a", IntegerValue(5))))
    assert(extended("a") == Some(IntegerValue(5)))
  }

}
