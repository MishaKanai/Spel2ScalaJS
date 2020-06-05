package mishakanai.spel2scalajs
import utest._
import scala.scalajs.js

object JSIntegrationTest extends TestSuite {
  def tests = Tests {
    test("Can use indexes into arrays") {
      val compiled = SpelEval
        .compileExpression(
          "{1, 2, {3, 4}}[2][0]"
        )
      // val result = compiled
      val t = compiled("type")
      assert(t == "parse_success")
      val evaluateFn =
        compiled("evaluate")
          .asInstanceOf[js.Function2[js.Dynamic, js.Dynamic, js.Dictionary[
            Any
          ]]]
      val res = evaluateFn(
        js.Dictionary[Any]().asInstanceOf[js.Dynamic],
        js.Dictionary[Any]().asInstanceOf[js.Dynamic]
      )
      val resType = res("type")
      assert(resType == "evaluation_success")
      val result = res("result")
      assert(result == 3)
    }
  }
}