package mishakanai.spel2scalajs
import scala.scalajs.js
import scala.util.parsing.combinator._
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

object DynamicJsParser {
  def parseDynamicJs(dynamic: js.Dynamic): JSContext = {
    if (dynamic.isInstanceOf[Float]) {
      return JSCFloat(dynamic.asInstanceOf[Float]);
    }
    if (js.Array.isArray(dynamic)) {
      return JSCArray(
        dynamic.asInstanceOf[js.Array[js.Dynamic]].map(parseDynamicJs)
      )
    }
    if (typeOf(dynamic) == "object") {
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
        // this is hit when we call with a scala defined function.
        // can set as Function0, Function1, etc.
        // ignore for now since we care about javascript functions only
        return JSCFunction(dynamic)
      }
    }
    if (typeOf(dynamic) == "string") {
      return JSCString(dynamic.asInstanceOf[String])
    }
    if (typeOf(dynamic) == "function") {
      // javascript function can be called with any number of parameters
      return JSCFunction(dynamic)
    }
    println(typeOf(dynamic))
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
    }
  }
}
