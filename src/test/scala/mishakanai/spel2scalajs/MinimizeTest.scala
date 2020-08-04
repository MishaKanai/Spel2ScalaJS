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
            "bar" -> compile("a + b"),
            "foo" -> compile("a + b > a + b ? foo(a + b) : a + b / a + b ")
          )
        )
        .map(t => (t._1, t._2._1))
      assert(
        minimized == HashMap(
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
          OpPlus(PropertyReference(false, "a"), PropertyReference(false, "b")).toString -> 4
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

    test("Test the reduction with lookupEntityDataDn") {
      val minimized = Minimize
        .getReplacements(
          HashMap[String, ExpressionSymbol](
            "foo" -> compile(
              "userId && #lookupEntityData('User', userId, 'email') != null "
            ),
            "bar" -> compile(
              "userId ? #lookupEntityData('User', userId, 'email') : null  "
            )
          )
        )

      val ymap = Map[String, ExpressionSymbol](
        "$a" -> FunctionReference(
          false,
          "lookupEntityData",
          StringLiteral("User") :: PropertyReference(false, "userId") :: StringLiteral(
            "email"
          ) :: Nil
        )
      )
      val xmap = Map[String, ExpressionSymbol](
        "foo" -> OpAnd(
          PropertyReference(false, "userId"),
          OpNE(
            PropertyReference(false, "$a"),
            NullLiteral()
          )
        ),
        "bar" -> Ternary(
          PropertyReference(false, "userId"),
          PropertyReference(false, "$a"),
          NullLiteral()
        )
      )
      assert(
        minimized == (ymap, xmap)
      )
    }
  }
}
