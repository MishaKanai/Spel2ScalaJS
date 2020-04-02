package mishakanai.spel2scalajs
import utest._
import scala.scalajs.js

object EvalTest extends TestSuite {
  def tests = Tests {
    test("Can evaluate an inline list") {
      val result = SpelEval
        .evaluate("{1, 2, 3}", js.Dictionary[Any]().asInstanceOf[js.Dynamic])
      assert(result.asInstanceOf[js.Array[Any]].toSeq == List(1, 2, 3))
    }
    test("Can evaluate Elvis") {
      val result1 = SpelEval
        .evaluate("1 ?: 2", js.Dictionary[Any]().asInstanceOf[js.Dynamic])
      assert(result1.asInstanceOf[Float] == 1)
      val result2 = SpelEval
        .evaluate("null ?: 2", js.Dictionary[Any]().asInstanceOf[js.Dynamic])
      assert(result2.asInstanceOf[Float] == 2)
    }

    test(
      "Can evaluate a javascript defined method with dynamic number of arguments"
    ) {
      val dynamic = js.Dictionary(
        "foo" -> js.eval("(...args) => args.map(x => x * 2)"),
        "bar" -> 1
      )
      val result = SpelEval.evaluate(
        "foo(bar, 2, 3)",
        dynamic.asInstanceOf[js.Dynamic]
      )
      assert(result.asInstanceOf[js.Array[Any]].toSeq == List(2, 4, 6))
    }

    test(
      "Can evaluate a scala defined method"
    ) {
      def scalaFn = () => js.Array(1, 2, 3)
      val result = SpelEval.evaluate(
        "foo.baz()",
        js.Dictionary(
            "foo" -> js.Dictionary(
              "baz" -> scalaFn
            )
          )
          .asInstanceOf[js.Dynamic]
      )

      assert(
        result.asInstanceOf[js.Array[Any]].toSeq
          == Seq(
            1,
            2,
            3
          )
      )
    }

    test(
      "Can evaluate functions and variables"
    ) {
      def scalaCtxtFn = () => "calledFromContext"
      val ctxt = js
        .Dictionary(
          "foo" -> scalaCtxtFn,
          "f" -> "helloFromContext"
        )
        .asInstanceOf[js.Dynamic]

      def scalaVarsFn = () => "calledFromVariables"
      val variables = js
        .Dictionary(
          "foo" -> scalaVarsFn,
          "f" -> "helloFromVars"
        )
        .asInstanceOf[js.Dynamic]
      val result1a = SpelEval.evaluate(
        "#foo()",
        ctxt,
        variables
      )
      assert(
        result1a.asInstanceOf[String] == scalaVarsFn()
      )
      val result1b = SpelEval.evaluate(
        "#f",
        ctxt,
        variables
      )
      assert(
        result1b.asInstanceOf[String] == "helloFromVars"
      )
      val result2a = SpelEval.evaluate(
        "foo()",
        ctxt,
        variables
      )
      assert(
        result2a.asInstanceOf[String] == scalaCtxtFn()
      )
      val result2b = SpelEval.evaluate(
        "f",
        ctxt,
        variables
      )
      assert(
        result2b.asInstanceOf[String] == "helloFromContext"
      )
    }
  }
}
