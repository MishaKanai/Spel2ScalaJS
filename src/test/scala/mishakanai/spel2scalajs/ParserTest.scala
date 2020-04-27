package mishakanai.spel2scalajs
import utest._
import scala.scalajs.js
import fastparse._

object ParseTest extends TestSuite {
  def tests = Tests {
    // string
    test("Simple string parsing") {
      val Parsed.Success(result, _) =
        parse(" \"foo\" ", ExpressionParser.expression(_))
      assert(
        result == StringLiteral("foo")
      )
    }
    test("Simple string parsing: single quotes") {
      val Parsed.Success(result, _) =
        parse(" 'foo' ", ExpressionParser.expression(_))
      assert(
        result == StringLiteral("foo")
      )
    }
    test("String parsing: escape characters 1 deep") {
      val Parsed.Success(result, _) =
        parse(" \"-?> \\\"foo\\\" dfs9 \" ", ExpressionParser.expression(_))
      val expected = StringLiteral("-?> \"foo\" dfs9 ")
      assert(
        result == expected
      )
    }
    test("String parsing: escape characters 2 deep") {
      val Parsed.Success(result, _) =
        parse(
          "  \"hello, I was told \\\" Say \\\\\\\"hi\\\\\\\" \\\" \" ",
          ExpressionParser.expression(_)
        )
      val expected = StringLiteral("hello, I was told \" Say \\\"hi\\\" \" ")
      assert(
        result == expected
      )
    }
    test("quoting") {
      val Parsed.Success(result, _) =
        parse(
          "  \"hello, I was told \\\" Say \\\\\\\"hi 'jack' \\\\\\\" \\\" \" ",
          ExpressionParser.expression(_)
        )
      val expected =
        StringLiteral("hello, I was told \" Say \\\"hi 'jack' \\\" \" ")

      assert(
        result == expected
      )
    }
    test("mixed but starting with sq") {
      val Parsed.Success(result, _) =
        parse(
          "  'they said, \" tell them \\'I said hi\\' \" ' ",
          ExpressionParser.expression(_)
        )
      val expected =
        StringLiteral("they said, \" tell them 'I said hi' \" ")
      assert(
        result == expected
      )
    }
    test("literal (null") {
      val Parsed.Success(result, _) =
        parse(
          "  null ",
          ExpressionParser.expression(_)
        )
      assert(
        result == NullLiteral()
      )
    }
    // test("mixed but starting with sq (deeper)") {
    //   val result =
    //     parse(
    //       "  'they said, \" tell them \\'I said \\\"hi \\\\'yo\\\\' \\\" \\' \" ' ",
    //       ExpressionParser.literal(_)
    //     )
    //   val expected = Parsed
    //     .Success(
    //       StringLiteral(
    //         "they said, \" tell them \'I said \\\"hi \\'yo\\' \\\" \' \" "
    //       ),
    //       44
    //     )
    //   assert(
    //     result == expected
    //   )
    // }

    test("null") {
      val Parsed.Success(result, _) =
        parse("null ?: 1", ExpressionParser.expression(_))
      assert(
        result ==
          Elvis(NullLiteral(), NumberLiteral(1))
      )
    }
    test("Inline lists") {
      val Parsed.Success(result, _) =
        parse("{1,2,3,4}", ExpressionParser.expression(_))
      assert(
        result ==
          InlineList(
            List(
              NumberLiteral(1),
              NumberLiteral(2),
              NumberLiteral(3),
              NumberLiteral(4)
            )
          )
      )
    }
    test("Inline Map") {
      val Parsed.Success(result, _) =
        parse("{foo: 1 + 1, bar: 3 }", ExpressionParser.expression(_))
      assert(
        result ==
          InlineMap(
            Map(
              "foo" -> OpPlus(
                NumberLiteral(1),
                NumberLiteral(1)
              ),
              "bar" -> NumberLiteral(3)
            )
          )
      )
    }
    test("Parsing: foo.?[#this == \"z\"]") {
      val Parsed.Success(result, _) =
        parse("foo.?[#this == \"z\"]", ExpressionParser.expression(_))
      val expected =
        CompoundExpression(
          List(
            PropertyReference(false, "foo"),
            SelectionAll(
              false,
              OpEQ(VariableReference("this"), StringLiteral("z"))
            )
          )
        )
      assert(
        result == expected
      )
    }
    test("Parsing SelectionLast: {1}.$[#this % 2 == 0]") {
      val Parsed.Success(result, _) =
        parse("{1}.$[#this % 2 == 0]", ExpressionParser.expression(_))
      val expected =
        CompoundExpression(
          List(
            InlineList(
              List(
                NumberLiteral(1)
              )
            ),
            SelectionLast(
              false,
              OpEQ(
                OpModulus(VariableReference("this"), NumberLiteral(2)),
                NumberLiteral(0)
              )
            )
          )
        );
      assert(result == expected)
    }
    test("Parsing Projection: {1}.![#this * 2]") {
      val Parsed.Success(result, _) =
        parse("{1}.![#this * 2]", ExpressionParser.expression(_))
      val expected =
        CompoundExpression(
          List(
            InlineList(
              List(
                NumberLiteral(1)
              )
            ),
            Projection(
              false,
              OpMultiply(VariableReference("this"), NumberLiteral(2))
            )
          )
        );
      assert(result == expected)
    }
    test("simple ternary") {
      val Parsed.Success(result, _) =
        parse("true ? \"true\" : \"false\"", ExpressionParser.expression(_))
      val expected =
        Ternary(
          BooleanLiteral(true),
          StringLiteral("true"),
          StringLiteral("false")
        );
      assert(result == expected)
    }
    test(
      "Parsing: foo() && arr?[0] > 0 || \"fooc\""
    ) {
      val Parsed.Success(result, _) =
        parse(
          "foo() && arr?[0] > 0 || \"fooc\"",
          ExpressionParser.parse(_)
        )
      val expected =
        OpOr(
          OpAnd(
            MethodReference(false, "foo", List()),
            OpGT(
              CompoundExpression(
                List(
                  PropertyReference(false, "arr"),
                  Indexer(true, NumberLiteral(0))
                )
              ),
              NumberLiteral(0)
            )
          ),
          StringLiteral("fooc")
        )

      assert(
        result == expected
      )
    }
    test(
      " prop "
    ) {
      val Parsed.Success(result, _) =
        parse(
          " prop ",
          ExpressionParser.parse(_)
        )
      val expected =
        PropertyReference(false, "prop")

      assert(
        result == expected
      )
    }
    test(
      "arr?.![#this + 1]"
    ) {
      val Parsed.Success(result, _) =
        parse(
          "arr?.![#this + 1]",
          ExpressionParser.parse(_)
        )
      val expected =
        CompoundExpression(
          List(
            PropertyReference(false, "arr"),
            Projection(
              true,
              OpPlus(
                VariableReference("this"),
                NumberLiteral(1)
              )
            )
          )
        )

      assert(
        result == expected
      )
    }
    test(
      "Parsing: foo() && arr?[0] > 0 || \"fooc\" ? arr?.![#this + 1] : null "
    ) {
      val Parsed.Success(result, _) =
        parse(
          "foo() && arr?[0] > 0 || \"fooc\" ? arr?.![#this + 1] : null",
          ExpressionParser.parse(_)
        )
      val expected =
        Ternary(
          OpOr(
            OpAnd(
              MethodReference(false, "foo", List()),
              OpGT(
                CompoundExpression(
                  List(
                    PropertyReference(false, "arr"),
                    Indexer(true, NumberLiteral(0))
                  )
                ),
                NumberLiteral(0)
              )
            ),
            StringLiteral("fooc")
          ),
          CompoundExpression(
            List(
              PropertyReference(false, "arr"),
              Projection(
                true,
                OpPlus(
                  VariableReference("this"),
                  NumberLiteral(1)
                )
              )
            )
          ),
          NullLiteral()
        )

      assert(
        result == expected
      )
    }
  }
}
