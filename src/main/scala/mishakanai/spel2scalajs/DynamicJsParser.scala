package mishakanai.spel2scalajs
import scala.scalajs.js
import js.JSConverters._
import js.typeOf

sealed abstract class JSContext
case class JSCString(value: String) extends JSContext
case class JSCDictionary(value: Map[String, JSContext]) extends JSContext
case class JSCArray(value: js.Array[JSContext]) extends JSContext
case class JSCFloat(value: Double) extends JSContext
case class JSCNull() extends JSContext
case class JSCBoolean(value: Boolean) extends JSContext
case class JSCFunction(function: js.Dynamic) extends JSContext
case class ScalaFunction0(function: Function0[Any]) extends JSContext
case class ScalaFunction1(function: Function1[Any, Any]) extends JSContext
case class ScalaFunction2(function: Function2[Any, Any, Any]) extends JSContext
case class ScalaFunction3(function: Function3[Any, Any, Any, Any])
    extends JSContext

object DynamicJsParser {
  def stripFunctionsExcept(
      dynamic: js.Dynamic,
      allowed: List[String]
  ): js.Dynamic = {
    if (typeOf(dynamic) == "object" & dynamic != null) {
      val asString = dynamic.asInstanceOf[scala.scalajs.js.Object].toString()
      if (asString == "[object Object]") {
        val dict = dynamic.asInstanceOf[js.Dictionary[js.Dynamic]]
        dict
          .toMap[String, js.Dynamic]
          .filter({
            case (k, v) =>
              (typeOf(dynamic) != "function" || allowed.contains(k))
          })
          .toJSDictionary
      }
    }
    return dynamic;
  }

  def isJSDict(dynamic: js.Dynamic): Boolean = {
    if (typeOf(dynamic) == "object" && dynamic != null) {
      val asString = dynamic.asInstanceOf[scala.scalajs.js.Object].toString()
      return asString == "[object Object]"
    }
    return false
  }

  def parseDynamicJs(dynamic: js.Dynamic): JSContext = {
    if (dynamic.isInstanceOf[Float]) {
      return JSCFloat(dynamic.asInstanceOf[Float]);
    }
    if (js.Array.isArray(dynamic)) {
      return JSCArray(
        dynamic.asInstanceOf[js.Array[js.Dynamic]].map(parseDynamicJs)
      )
    }
    if (typeOf(dynamic) == "object" && dynamic != null) {
      val asString = dynamic.asInstanceOf[scala.scalajs.js.Object].toString()
      if (asString == "[object Object]") {
        val dict = dynamic.asInstanceOf[js.Dictionary[js.Dynamic]]
        return JSCDictionary(
          dict
            .toMap[String, js.Dynamic]
            .transform[JSContext]((k, v) => parseDynamicJs(v))
        )
      } else if (asString == "[object String]") {
        return JSCString(dynamic.asInstanceOf[String])
      } else if (asString.matches("<function[0-9]+>")) {
        val argumentCount = asString.slice(9, asString.indexOf(">")).toInt;
        if (argumentCount == 0)
          return ScalaFunction0(dynamic.asInstanceOf[Function0[Any]])
        else if (argumentCount == 1)
          return ScalaFunction1(dynamic.asInstanceOf[Function1[Any, Any]])
        else if (argumentCount == 2)
          return ScalaFunction2(dynamic.asInstanceOf[Function2[Any, Any, Any]])
        else if (argumentCount == 3)
          return ScalaFunction3(
            dynamic.asInstanceOf[Function3[Any, Any, Any, Any]]
          )
        else
          throw new RuntimeException(
            "Arbitrarily not supported: scala function found with more than 3 arguments"
          )
      }
    }
    if (typeOf(dynamic) == "string") {
      return JSCString(dynamic.asInstanceOf[String])
    }
    if (typeOf(dynamic) == "function") {
      // javascript function can be called with any number of parameters
      return JSCFunction(dynamic)
    }
    if (typeOf(dynamic) == "boolean") {
      return JSCBoolean(dynamic.asInstanceOf[Boolean]);
    }
    return JSCNull()
  }

  def backToDynamic(context: JSContext): js.Any = {
    context match {
      case JSCString(value) => value
      case JSCDictionary(value) =>
        value
          .transform((k, v) => backToDynamic((v)))
          .toJSDictionary
      case JSCArray(value)       => value.map(backToDynamic)
      case JSCFloat(value)       => value
      case JSCNull()             => null
      case JSCBoolean(value)     => value
      case JSCFunction(function) => function
      case ScalaFunction0(function) =>
        function.asInstanceOf[js.Function0[Any]]
      case ScalaFunction1(function) =>
        function.asInstanceOf[js.Function1[Any, Any]]
      case ScalaFunction2(function) =>
        function.asInstanceOf[js.Function2[Any, Any, Any]]
      case ScalaFunction3(function) =>
        function.asInstanceOf[js.Function3[Any, Any, Any, Any]]
    }
  }
}
