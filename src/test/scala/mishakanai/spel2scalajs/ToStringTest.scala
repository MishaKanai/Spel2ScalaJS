package mishakanai.spel2scalajs
import utest._
import scala.scalajs.js
import fastparse._

object ToStringTest extends TestSuite {
  def generate = (expression: String) => {
    val Parsed.Success(result, _) =
      parse(expression, ExpressionParser.expression(_))

    (expression, AstToString.astToString(result))
  }
  def tests = Tests {
    // string
    test("Simple string parsing") {
      val (original, result) = generate(" \"foo\"  ")
      assert(original.trim == result)
    }
    test("a") {
      val (original, result) = generate(" a + b / c  ")
      assert(original.trim == result)
    }
    test("b") {
      val (original, result) = generate(" (a + b) / c  ")
      assert(original.trim == result)
    }
    test("c") {
      val (original, result) = generate(" a % b / c  ")
      assert(original.trim == result)
    }
    test("d") {
      val (original, result) = generate(" a % b ** 12 / c  ")
      assert(original.trim == result)
    }
    test("e") {
      val (original, result) =
        generate(" a % b ** (12 / c) + foo(12 - 3 * (x || s ? 3 : 4))  ")
      assert(original.trim == result)
    }
    test("f") {
      val (original, result) =
        generate(
          " a % b ** (12 / c) + foo(12 - 3 * (x || s ? 3 : 4)) <= (4 + 4) - 3 ? x()?: 3 : 5 "
        )
      assert(original.trim == result)
    }
  }
}
