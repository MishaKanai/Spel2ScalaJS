package mishakanai.spel2scalajs
import utest._
import scala.scalajs.js
import scala.collection.immutable.Map

object MinimizeExpressionsTest extends TestSuite {
  def tests = Tests {
    test("basic") {
      val (added, changed) = Minimize.minimizeExpressions(
        Map(
          "field1" -> " a + b ? a + b : c - d",
          "field2" -> "c - d"
        )
      )
      assert(
        added == Map(
          "$a" -> "c - d",
          "$b" -> "a + b"
        )
      )
      assert(
        changed == Map(
          "field1" -> "$b ? $b : $a",
          "field2" -> "$a"
        )
      )
    }

    test("js") {
      val jsresult = SpelEval.getMinimalExpressions(
        js.eval(
            """({ "field1": " a + b ? a + b : c - d ", "field2": " c - d " })"""
          )
          .asInstanceOf[js.Dictionary[String]]
      )
      val r = jsresult.toList.map(e => e.toMap)
      assert(
        r == List(
          Map(
            "$a" -> "c - d",
            "$b" -> "a + b"
          ),
          Map(
            "field1" -> "$b ? $b : $a",
            "field2" -> "$a"
          )
        )
      )
    }
  }
}
