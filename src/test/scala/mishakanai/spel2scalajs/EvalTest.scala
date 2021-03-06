package mishakanai.spel2scalajs
import utest._
import scala.scalajs.js

object EvalTest extends TestSuite {
  def tests = Tests {
    test("Can evaluate properties") {
      val result = SpelEval
        .evaluateFast(
          "foo",
          js.Dictionary[Any]({
              "foo" -> false
            })
            .asInstanceOf[js.Dynamic]
        )
      assert(result == false)
    }
    test("Can use indexes into arrays") {
      val result = SpelEval
        .evaluateFast(
          "{1, 2, {3, 4}}[2][0]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result == 3)
    }
    test("safe-navigate index (not null") {
      val result = SpelEval
        .evaluateFast(
          "{1, 2, 3}?[2]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result == 3)
    }
    test("safe-navigate index (null)") {
      val result = SpelEval
        .evaluateFast(
          "null?[2]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result == null)
    }
    test("Can evaluate negative number literals") {
      val result = SpelEval
        .evaluateFast(
          "-1",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result.asInstanceOf[Int] == -1)
    }
    test("Can evaluate an inline list") {
      val result = SpelEval
        .evaluateFast(
          "{1, 2, 3}",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result.asInstanceOf[js.Array[Any]].toSeq == List(1, 2, 3))
    }
    test("Can evaluate Elvis") {
      val result1 = SpelEval
        .evaluateFast("1 ?: 2", js.Dictionary[Any]().asInstanceOf[js.Dynamic])
      assert(result1.asInstanceOf[Float] == 1)
      val result2 = SpelEval
        .evaluateFast(
          "null ?: 2",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result2.asInstanceOf[Float] == 2)
    }
    test("Can evaluate SelectionAll: get evens") {
      val result1 = SpelEval
        .evaluateFast(
          "{1, 2, 3, 4, 5}.?[#this % 2 == 0]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result1.asInstanceOf[js.Array[Any]].toSeq == Seq(2, 4))
    }
    test("Safe nav SelectionAll (not null)") {
      val result1 = SpelEval
        .evaluateFast(
          "{1, 2, 3, 4, 5}?.?[#this % 2 == 0]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result1.asInstanceOf[js.Array[Any]].toSeq == Seq(2, 4))
    }
    test("Safe nav SelectionAll (null)") {
      val result1 = SpelEval
        .evaluateFast(
          "null?.?[#this % 2 == 0]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result1 == null)
    }
    test("Can evaluate SelectionLast: get even") {
      val result1 = SpelEval
        .evaluateFast(
          "{1, 2, 3, 4, 5}.$[#this % 2 == 0]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result1 == 4)
    }
    test("Safe-navigate SelectionLast (not null)") {
      val result1 = SpelEval
        .evaluateFast(
          "{1, 2, 3, 4, 5}?.$[#this % 2 == 0]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result1 == 4)
    }
    test("Safe-navigate SelectionFirst") {
      val result1 = SpelEval
        .evaluateFast(
          "null?.^[#this % 2 == 0]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(result1 == null)
    }

    test("Can evaluate Projection: * 2") {
      val result1 = SpelEval
        .evaluateFast(
          "{1, 2, 3, 4, 5}.![#this * 2]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(
        result1.asInstanceOf[js.Array[Any]].toSeq == Seq(2, 4, 6, 8, 10)
      )
    }
    test("Safe-navigate Projection") {
      val result1 = SpelEval
        .evaluateFast(
          "{1, 2, 3, 4, 5}?.![#this * 2]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(
        result1.asInstanceOf[js.Array[Any]].toSeq == Seq(2, 4, 6, 8, 10)
      )
    }
    test("Safe-navigate Projection") {
      val result1 = SpelEval
        .evaluateFast(
          "null?.![#this * 2]",
          js.Dictionary[Any]().asInstanceOf[js.Dynamic]
        )
      assert(
        result1 == null
      )
    }
    test(
      "Can evaluate a javascript defined method with dynamic number of arguments"
    ) {
      val dynamic = js.Dictionary(
        "foo" -> js.eval("(...args) => args.map(x => x * 2)"),
        "bar" -> 1
      )
      val result = SpelEval.evaluateFast(
        "foo(bar, 2, 3)",
        dynamic.asInstanceOf[js.Dynamic]
      )
      assert(result.asInstanceOf[js.Array[Any]].toSeq == List(2, 4, 6))
    }
    // BELOW: Nice to haves

    // test(
    //   "safe-navigate method call (not-null)"
    // ) {
    //   val dynamic = js.Dictionary(
    //     "foo" -> js.eval("(...args) => args.map(x => x * 2)"),
    //     "bar" -> 1
    //   )
    //   val result = SpelEval.evaluate(
    //     "foo?(bar, 2, 3)",
    //     dynamic.asInstanceOf[js.Dynamic]
    //   )
    //   assert(result.asInstanceOf[js.Array[Any]].toSeq == List(2, 4, 6))
    // }
    // test(
    //   "safe-navigate method call (null)"
    // ) {
    //   val dynamic = js.Dictionary(
    //     "foo" -> null,
    //     "bar" -> 1
    //   )
    //   val result = SpelEval.evaluate(
    //     "foo?(bar, 2, 3)",
    //     dynamic.asInstanceOf[js.Dynamic]
    //   )
    //   assert(result == null)
    // }

    test(
      "Can evaluate a scala defined method"
    ) {
      def scalaFn = () => js.Array(1, 2, 3)
      val result = SpelEval.evaluateFast(
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
      "foo.bar.baz != null ? foo.bar.baz : null"
    ) {
      def scalaFn = () => js.Array(1, 2, 3)
      val result = SpelEval.evaluateFast(
        "foo.bar.baz != null ? foo.bar.baz : null",
        js.Dictionary(
            "foo" -> js.Dictionary(
              "bar" -> js.Dictionary(
                "baz" -> null
              )
            )
          )
          .asInstanceOf[js.Dynamic]
      )

      assert(
        result == null
      )
    }
    test(
      ">="
    ) {
      var result = SpelEval.evaluateFast(
        "5 >=5",
        js.Dictionary(
            )
          .asInstanceOf[js.Dynamic]
      )

      assert(
        result == true
      )
      result = SpelEval.evaluateFast(
        "5 >= 4",
        js.Dictionary(
            )
          .asInstanceOf[js.Dynamic]
      )

      assert(
        result == true
      )
      result = SpelEval.evaluateFast(
        "5 >= 6",
        js.Dictionary(
            )
          .asInstanceOf[js.Dynamic]
      )

      assert(
        result == false
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
      val result1a = SpelEval.evaluateFast(
        "#foo()",
        ctxt,
        variables
      )
      assert(
        result1a.asInstanceOf[String] == scalaVarsFn()
      )
      val result1b = SpelEval.evaluateFast(
        "#f",
        ctxt,
        variables
      )
      assert(
        result1b.asInstanceOf[String] == "helloFromVars"
      )
      val result2a = SpelEval.evaluateFast(
        "foo()",
        ctxt,
        variables
      )
      assert(
        result2a.asInstanceOf[String] == scalaCtxtFn()
      )
      val result2b = SpelEval.evaluateFast(
        "f",
        ctxt,
        variables
      )
      assert(
        result2b.asInstanceOf[String] == "helloFromContext"
      )
    }
    test(
      "Can evaluate javascript methods"
    ) {
      val ctxt = js
        .Dictionary(
          "add" -> js.eval("(a, b) => a + b")
        )
        .asInstanceOf[js.Dynamic]
      val result = SpelEval.evaluateFast(
        "add(1, 2)",
        ctxt,
        js.eval("{}").asInstanceOf[js.Dynamic]
      )
      assert(result == 3)
    }
    test("Null behavior with boolean operators: !null") {
      val result2 = SpelEval.evaluateFast(
        """
        !null
        """,
        js.eval("({ })").asInstanceOf[js.Dynamic],
        js.eval("{}").asInstanceOf[js.Dynamic]
      )
      assert(result2 == true)
    }
    test("Null behavior with boolean operators: null || true") {
      val result2 = SpelEval.evaluateFast(
        """
        null || true
        """,
        js.eval("({ })").asInstanceOf[js.Dynamic],
        js.eval("({})").asInstanceOf[js.Dynamic]
      )
      assert(result2 == true)
    }
    test("Null behavior with boolean operators: null && true") {

      val result2 = SpelEval.evaluateFast(
        """
        null && true
        """,
        js.eval("({ })").asInstanceOf[js.Dynamic],
        js.eval("({})").asInstanceOf[js.Dynamic]
      )
      assert(result2 == null)
    }
    test("Null behavior with boolean operators: true && null") {

      val result2 = SpelEval.evaluateFast(
        """
        true && null
        """,
        js.eval("({ })").asInstanceOf[js.Dynamic],
        js.eval("({})").asInstanceOf[js.Dynamic]
      )
      assert(result2 == null)
    }
    test("Null behavior with boolean operators: true && null") {

      val result2 = SpelEval.evaluateFast(
        """
        true || null
        """,
        js.eval("({ })").asInstanceOf[js.Dynamic],
        js.eval("({})").asInstanceOf[js.Dynamic]
      )
      assert(result2 == true)
    }
    test("Boolean operators short circuit: &&") {

      val result2 = SpelEval.evaluateFast(
        """
        foo && foo()
        """,
        js.eval(
            "({" +
              "foo: false" +
              "})"
          )
          .asInstanceOf[js.Dynamic],
        js.eval("({})").asInstanceOf[js.Dynamic]
      )
      assert(result2 == false)
    }
    test("Boolean operators short circuit: ||") {

      val result2 = SpelEval.evaluateFast(
        """
        foo == false || foo()
        """,
        js.eval(
            "({" +
              "foo: false" +
              "})"
          )
          .asInstanceOf[js.Dynamic],
        js.eval("({})").asInstanceOf[js.Dynamic]
      )
      assert(result2 == true)
    }
    //
    test("test ternary") {

      val result2 = SpelEval.evaluateFast(
        // currentUserHasRole('ROLE_REGISTERED_USER')
        """
        currentUserHasRole('ROLE_SUPER') ? false : true
        """,
        js.eval(
            "({" +
              "currentUserHasRole: (arg) => false" +
              "})"
          )
          .asInstanceOf[js.Dynamic],
        js.eval("({})").asInstanceOf[js.Dynamic]
      )
      assert(result2 == true)
    }
  }
}
