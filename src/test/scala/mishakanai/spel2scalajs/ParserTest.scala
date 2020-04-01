package mishakanai.spel2scalajs
import utest._
import scala.scalajs.js

object ParseTest extends TestSuite {
  def tests = Tests {
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
