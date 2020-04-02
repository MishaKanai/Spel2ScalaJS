package mishakanai.spel2scalajs
import utest._
import scala.scalajs.js

object ParseTest extends TestSuite {
  def tests = Tests {
    test("Inline lists") {
      val result = SpelParser.apply("{1,2,3,4}")
      assert(
        result == Some(
          InlineList(
            List(
              NumberLiteral(1),
              NumberLiteral(2),
              NumberLiteral(3),
              NumberLiteral(4)
            )
          )
        )
      )
    }
    test("Inline Map") {
      val result = SpelParser.apply("{foo: 1 + 1, bar: 3 }")
      assert(
        result == Some(
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
      )
    }
    test("Parsing: foo.?[#this == \"z\"]") {
      val result = SpelParser.apply("foo.?[#this == \"z\"]")
      val expected = Some(
        CompoundExpression(
          List(
            PropertyReference(false, "foo"),
            SelectionAll(
              false,
              OpEQ(VariableReference("this"), StringLiteral("\"z\""))
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
      val result = SpelParser.apply(
        "foo() && arr?[0] > 0 || \"fooc\" ? arr?.![#this + 1] : null"
      )
      val expected = Some(
        Ternary(
          OpOr(
            OpAnd(
              FunctionReference(false, "foo", List()),
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
            StringLiteral("\"fooc\"")
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
      )
      assert(
        result == expected
      )
    }
  }
}
