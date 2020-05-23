package mishakanai.spel2scalajs
import utest._
import scala.scalajs.js

object MetaTest extends TestSuite {
  def tests = Tests {
    test("Can use indexes into arrays") {
      assert(true == true)
    }
    test("gets a simple expansion") {
      val compiled = SpelEval
        .compileExpression(
          "foo.bar.baz"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansions")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toList
      assert(res == List("foo.bar.baz"))
    }
    test("nested properties") {
      val compiled = SpelEval
        .compileExpression(
          "p.p2 == 'p2sp1'"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansions")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toList
      assert(res == List("p.p2"))
    }
    test("regular fields and nested properties") {
      val compiled = SpelEval
        .compileExpression(
          "p1 ? p && p.p2 == 'p2sp1' || p3 : p4.p && p.p3"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansions")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(res == Set("p1", "p", "p.p2", "p.p3", "p3", "p4.p"))
    }
    test("regular fields and nested properties and various paren groupings") {
      val compiled = SpelEval
        .compileExpression(
          "((p1 ? (p && p.p2 == 'p2sp1') || (p3) : p4.p) && p.p3)"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansions")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(res == Set("p1", "p", "p.p2", "p.p3", "p3", "p4.p"))
    }
    test("only gets method arguments if they are properties") {
      val compiled = SpelEval
        .compileExpression(
          "fn1(p1) > fn2('foo')"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansions")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(res == Set("p1"))
    }
    test("gets fields from method arguments that are compound expressions") {
      val compiled = SpelEval
        .compileExpression(
          "fn1(p1 * 2 / p2) > fn2(true)"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansions")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(res == Set("p1", "p2"))
    }

    /*
        BELOW:
        getExpansionsWithAll
     */
    test("replaces a dynamic index with _ALL_") {
      val compiled = SpelEval
        .compileExpression(
          "field1[fn1(p1 * 2 / p2) > fn2(true)].field2"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(res == Set("field1._ALL_", "p1", "p2"))
    }
    test(
      "replaces a dynamic index (with bracketed expressions inside) with _ALL_"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "field1[p3[p1 * 2 / p2] > fn2(true)]"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(res == Set("field1._ALL_", "p3._ALL_", "p1", "p2"))
    }
    test(
      "replaces multiple dynamic indexes on the same depthwith _ALL_"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "field1[p3] && field2[p4]"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(res == Set("field1._ALL_", "field2._ALL_", "p3", "p4"))
    }
    test(
      "deals with a complex tree of nested indexable expressions"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "field1[field1[p3] && field2[p4]].p3 || field4[field1[p]]"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "field1._ALL_",
          "field1._ALL_",
          "p3",
          "p4",
          "field2._ALL_",
          "field4._ALL_",
          "p"
        )
      )
    }
    test(
      "deals with combinations of indexes and function calls"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "myfunc1(foo[x]) && myfunc2(a)[xd]"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "foo._ALL_",
          "x",
          "xd",
          "a"
        )
      )
    }
    test(
      "deals with combinations of indexes and function calls 2"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "myfunc1(foo[x], asdf[y]) && myfunc2(a)[xd]"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "foo._ALL_",
          "x",
          "xd",
          "asdf._ALL_",
          "y",
          "a"
        )
      )
    }
    test(
      "can make a function inside an index"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "myfield[foo(x)]"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "myfield._ALL_",
          "x"
        )
      )
    }
    test(
      "returns dot seperated properties instead of safe navigation operator"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "(currentAddress?.state?.title != null ? currentAddress?.state?.title == lastName : false)"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "currentAddress.state.title",
          "lastName"
        )
      )
    }
    test(
      "ignores closing parenthesis inside of string literals when parsing function arguments"
    ) {
      val compiled = SpelEval
        .compileExpression(
          " myfunc(\")\", x) "
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "x"
        )
      )
    }
    test(
      "can make a function call inside an index inside a function call"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "myfunc(myfield[foo(x)])"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "myfield._ALL_",
          "x"
        )
      )
    }
    test(
      "can make a function call as an argument to a function call"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "myfunc(myfunc2(x))"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "x"
        )
      )
    }
    test(
      "can make a function call with multiple arguments as an argument to a function call"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "myfunc(myfunc2(x, y))"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "x",
          "y"
        )
      )
    }
    test(
      "can make a function call with no arguments"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "myfunc()"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          )
      )
    }
    test(
      "handles filters on safe-navigation operator correctly, strips \".size()\", ignores \"this\" "
    ) {
      val compiled = SpelEval
        .compileExpression(
          "roles.?[#this == filterValue].size() != 0"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "roles",
          "filterValue"
        )
      )
    }
    test(
      "expands anything based off \"this\" in filters"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "record.organizations && record.organizations.?[foo && #this.name == #this.othername].size() != 0"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "foo",
          "record.organizations",
          "record.organizations.name",
          "record.organizations.othername"
        )
      )
    }
    test(
      "deals with nested contexts appropriately"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "foo.?[#this.list.?[#this.two == '2a'].size() > 1].size()"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "foo",
          "foo.list",
          "foo.list.two"
        )
      )
    }
    test(
      "deals with nested contexts and function calls appropriately"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "fn(foo.?[length(#this.list.bar) > 1].size())"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "foo",
          "foo.list.bar"
        )
      )
    }
    test(
      "deals with contains appropriately 1"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "foo.contains(bar) && baz"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "foo",
          "bar",
          "baz"
        )
      )
    }
    test(
      "deals with contains appropriately 2"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "foo.func(bar).res1"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "foo",
          "bar"
        )
      )
    }
    test(
      "real life example 4-20-2020"
    ) {
      val compiled = SpelEval
        .compileExpression(
          "(provider != null && provider.providerOrgTypeCode != null && miisUserRole != null && ((provider.providerOrgTypeCode== 'AGGREGATOR' || provider.providerOrgTypeCode == 'MEDICALGROUP') && matchPattern(getConceptFromCode('MIISUserRole', miisUserRoleCode).group, 'READONLY'))) ? false : true"
        )
      val t = compiled("type")
      assert(t == "parse_success")
      val getExpansionsFn =
        compiled("getExpansionsWithAll")
          .asInstanceOf[js.Function0[js.Array[String]]]
      val res = getExpansionsFn().toSet
      assert(
        res == Set(
          "miisUserRole",
          "miisUserRoleCode",
          "provider",
          "provider.providerOrgTypeCode"
        )
      )
    }
  }
}
