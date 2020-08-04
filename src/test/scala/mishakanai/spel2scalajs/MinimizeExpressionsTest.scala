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
          "$a" -> "c - d"
        )
      )
      assert(
        changed == Map(
          "field1" -> "a + b ? a + b : $a",
          "field2" -> "$a"
        )
      )
    }

    test("Don't replace if only shared top-level exps") {
      val (added, changed) = Minimize.minimizeExpressions(
        Map(
          "field1" -> " a + b ? a + b : c - d",
          "field2" -> " a + b ? a + b : c - d"
        )
      )
      assert(
        added == Map(
          "$a" -> "a + b"
        )
      )
      assert(
        changed == Map(
          "field1" -> "$a ? $a : c - d",
          "field2" -> "$a ? $a : c - d"
        )
      )
    }

    test("basic with lookupEntityData replacement success") {
      val (added, changed) = Minimize.minimizeExpressions(
        Map(
          "field1" -> "a + b ? lookupEntityData('User', foo, 'email') : c - d",
          "field2" -> "a ? lookupEntityData('User', foo, 'email') : c"
        )
      )
      assert(
        added == Map(
          "$a" -> """lookupEntityData("User", foo, "email")"""
        )
      )
      assert(
        changed == Map(
          "field1" -> """a + b ? $a : c - d""",
          "field2" -> """a ? $a : c"""
        )
      )
    }
    test("basic with lookupEntityData non-replacement (match based on args)") {
      val (added, changed) = Minimize.minimizeExpressions(
        Map(
          "field1" -> "a + b ? lookupEntityData('User', foo, 'email') : c - d",
          "field2" -> "a + b ? lookupEntityData('User', foo, 'fname') : c - d"
        )
      )
      assert(
        added == Map(
          "$a" -> "a + b"
        )
      )
      assert(
        changed == Map(
          "field1" -> """$a ? lookupEntityData("User", foo, "email") : c - d""",
          "field2" -> """$a ? lookupEntityData("User", foo, "fname") : c - d"""
        )
      )
    }
    test(
      "basic with lookupEntityData non-replacement (when there's a compound as argument)"
    ) {
      val (added, changed) = Minimize.minimizeExpressions(
        Map(
          "field1" -> " a + b ? lookupEntityData(foo.bar) : c - d",
          "field2" -> "c - d"
        )
      )
      assert(
        added == Map(
          "$a" -> "c - d"
        )
      )
      assert(
        changed == Map(
          "field1" -> "a + b ? lookupEntityData(foo.bar) : $a",
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
            "$a" -> "c - d"
          ),
          Map(
            "field1" -> "a + b ? a + b : $a",
            "field2" -> "$a"
          )
        )
      )
    }

    test("js variable 'orders of execution' are encoded in the variable names") {
      val jsresult = SpelEval.getMinimalExpressions(
        js.eval(
            """({ "field1": " a + b ? a + b : c - d - e ", "field2": " c - d - e ", "field3": "c - d" })"""
          )
          .asInstanceOf[js.Dictionary[String]]
      )
      val r = jsresult.toList.map(e => e.toMap)

      assert(
        r == List(
          Map(
            "$$b" -> "$a - e",
            "$a" -> "c - d"
          ),
          Map(
            "field1" -> "a + b ? a + b : $$b",
            "field2" -> "$$b",
            "field3" -> "$a"
          )
        )
      )
    }

    test("js") {
      val jsresult = SpelEval.getMinimalExpressions(
        js.eval(
            """({ "f": "f" })"""
          )
          .asInstanceOf[js.Dictionary[String]]
      )
      val r = jsresult.toList.map(e => e.toMap)
      assert(
        r == List(
          Map(),
          Map(
            "f" -> "f"
          )
        )
      )
    }
  }
}
