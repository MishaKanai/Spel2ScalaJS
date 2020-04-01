package mishakanai.spel2scalajs

object MainTest extends TestSuite {
  def tests = Tests {
    test(
      "Can evaluate a javascript defined function with dynamic number of arguments"
    ) {

      val result = SpelParser
        .apply("foo.bar.baz(1, 2, 3)")
        .map(
          new Evaluator(
            DynamicJsParser.parseDynamicJs(
              js.eval("""
                {
                    foo: { bar: { baz: (...args) => args.map(x => x * 2) }}
                }
                """)
            )
          ).evaluate
        )

      assert(
        result == List(2, 4, 6)
      )
    }
  }
}
