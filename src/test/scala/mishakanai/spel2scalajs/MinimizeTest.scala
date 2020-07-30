package mishakanai.spel2scalajs
import utest._
import scala.scalajs.js
import scala.collection.immutable.HashMap

object MinimizeTest extends TestSuite {
  def compile =
    (input: String) =>
      fastparse
        .parse(input, ExpressionParser.parse(_))
        .fold(
          (f: String, int: Int, extra: Any) =>
            throw new RuntimeException("failed to parse" + f),
          (s: ExpressionSymbol, int: Int) => s
        )
  def tests = Tests {
    test("basic") {
      val minimized = Minimize
        .minimize(
          HashMap[String, ExpressionSymbol](
            "foo" -> compile("a + b > 0 ? foo(c) : '' ")
          )
        )
        .map(t => (t._1, t._2._1))
      assert(
        minimized == HashMap(
          MethodReference(false, "foo", List(PropertyReference(false, "c"))).toString -> 1,
          OpPlus(PropertyReference(false, "a"), PropertyReference(false, "b")).toString -> 1,
          OpGT(
            OpPlus(
              PropertyReference(false, "a"),
              PropertyReference(false, "b")
            ),
            NumberLiteral(0)
          ).toString -> 1,
          Ternary(
            OpGT(
              OpPlus(
                PropertyReference(false, "a"),
                PropertyReference(false, "b")
              ),
              NumberLiteral(0)
            ),
            MethodReference(false, "foo", List(PropertyReference(false, "c"))),
            StringLiteral("")
          ).toString -> 1
        )
      )
    }
    test("2") {
      val minimized = Minimize
        .minimize(
          HashMap[String, ExpressionSymbol](
            "foo" -> compile("a + b > a + b ? foo(a + b) : a + b / a + b ")
          )
        )
        .map(t => (t._1, t._2._1))
      assert(
        minimized == HashMap(
          OpPlus(
            PropertyReference(false, "a"),
            OpDivide(
              PropertyReference(false, "b"),
              PropertyReference(false, "a")
            )
          ).toString -> 1,
          OpPlus(
            OpPlus(
              PropertyReference(false, "a"),
              OpDivide(
                PropertyReference(false, "b"),
                PropertyReference(false, "a")
              )
            ),
            PropertyReference(false, "b")
          ).toString -> 1,
          OpDivide(PropertyReference(false, "b"), PropertyReference(false, "a")).toString -> 1,
          OpPlus(PropertyReference(false, "a"), PropertyReference(false, "b")).toString -> 3,
          Ternary(
            OpGT(
              OpPlus(
                PropertyReference(false, "a"),
                PropertyReference(false, "b")
              ),
              OpPlus(
                PropertyReference(false, "a"),
                PropertyReference(false, "b")
              )
            ),
            MethodReference(
              false,
              "foo",
              List(
                OpPlus(
                  PropertyReference(false, "a"),
                  PropertyReference(false, "b")
                )
              )
            ),
            OpPlus(
              OpPlus(
                PropertyReference(false, "a"),
                OpDivide(
                  PropertyReference(false, "b"),
                  PropertyReference(false, "a")
                )
              ),
              PropertyReference(false, "b")
            )
          ).toString -> 1,
          OpGT(
            OpPlus(
              PropertyReference(false, "a"),
              PropertyReference(false, "b")
            ),
            OpPlus(PropertyReference(false, "a"), PropertyReference(false, "b"))
          ).toString -> 1,
          MethodReference(
            false,
            "foo",
            List(
              OpPlus(
                PropertyReference(false, "a"),
                PropertyReference(false, "b")
              )
            )
          ).toString -> 1
        )
      )
    }

    test("Test the reduction!") {
      val minimized = Minimize
        .getReplacements(
          HashMap[String, ExpressionSymbol](
            "foo" -> compile("a + b > a ? b : c"),
            "bar" -> compile("a + b "),
            "baz" -> compile("b / a")
          )
        )
      assert(
        minimized == (Map(
          "$a" -> OpPlus(
            PropertyReference(false, "a"),
            PropertyReference(false, "b")
          )
        ), HashMap(
          "bar" -> PropertyReference(false, "$a"),
          "baz" -> OpDivide(
            PropertyReference(false, "b"),
            PropertyReference(false, "a")
          ),
          "foo" -> Ternary(
            OpGT(PropertyReference(false, "$a"), PropertyReference(false, "a")),
            PropertyReference(false, "b"),
            PropertyReference(false, "c")
          )
        ))
      )
    }

    test("Test the reduction!") {
      val minimized = Minimize
        .getReplacements(
          HashMap[String, ExpressionSymbol](
            "foo" -> compile("a + b > a + b ? foo(a + b) : a + b / a + b "),
            "bar" -> compile("a + b "),
            "baz" -> compile("b / a")
          )
        )
      assert(
        minimized == (Map(
          "$a" -> OpPlus(
            PropertyReference(false, "a"),
            PropertyReference(false, "b")
          ),
          "$b" -> OpDivide(
            PropertyReference(false, "b"),
            PropertyReference(false, "a")
          )
        ), HashMap(
          "bar" -> PropertyReference(false, "$a"),
          "baz" -> PropertyReference(false, "$b"),
          "foo" -> Ternary(
            OpGT(
              PropertyReference(false, "$a"),
              PropertyReference(false, "$a")
            ),
            MethodReference(false, "foo", List(PropertyReference(false, "$a"))),
            OpPlus(
              OpPlus(
                PropertyReference(false, "a"),
                PropertyReference(false, "$b")
              ),
              PropertyReference(false, "b")
            )
          )
        ))
      )
    }
  }
}
