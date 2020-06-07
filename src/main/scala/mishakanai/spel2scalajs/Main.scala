package mishakanai.spel2scalajs
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.JSON
import scala.scalajs.js.Dictionary
import js.JSConverters._
import js.typeOf
import fastparse.parse
import boopickle.Default._
import java.nio.ByteBuffer
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

@JSExportTopLevel("Spel2ScalaJs")
object SpelEval {
  @JSExport
  def parseFast(
      input: String,
      context: js.Dynamic,
      functionsAndVariables: js.Dynamic =
        js.Dictionary().asInstanceOf[js.Dynamic]
  ): js.Dynamic = {
    val result = fastparse.parse(input, ExpressionParser.parse(_))
    null
  }
  @JSExport
  def evaluateFast(
      input: String,
      context: js.Dynamic,
      functionsAndVariables: js.Dynamic =
        js.Dictionary().asInstanceOf[js.Dynamic]
  ): js.Dynamic = {
    val result = fastparse
      .parse(input, ExpressionParser.parse(_))
      .fold((msg, i, extra) => None, (r: ExpressionSymbol, _) => Some(r))
      .map(p => {
        val ctxt = DynamicJsParser.parseDynamicJs(context)
        val fnsAndVars = DynamicJsParser.parseDynamicJs(functionsAndVariables)
        new Evaluator(ctxt, fnsAndVars).evaluate(p)
      })
    if (result.isDefined)
      return DynamicJsParser.backToDynamic(result.get).asInstanceOf[js.Dynamic]
    else {
      throw new RuntimeException("Failed to Parse Spel expression")
    }
  }
  // return a javascript object with functions to get:
  // 1. paths (e.g. data paths) (rewrite this using tests from the typescript impls)
  // 2. 'evaluate' function.
  //
  /*
    {
      type: 'success',
      evaluate(context, functionsAndVars)
    } | {
      type: 'failure',
      msg: string
      i: number,

    }
   */

  val loadAst = (r: ExpressionSymbol) => {
    val getExpansions: js.Function0[js.Array[String]] =
      () => {
        Meta.getExpansions(r).toJSArray
      }
    val getExpansionsWithAll: js.Function0[js.Array[String]] =
      () => {
        Meta.getExpansions(r, true).toJSArray
      }
    val serialize: js.Function0[ByteBuffer] = () => {
      Pickle.intoBytes(r)
    }
    val toJson: js.Function0[String] = () => {
      r.asJson.noSpaces
    }
    val methodAndFunctionNames = Meta.getMethodsAndFunctions(r).distinct
    val evaluate: js.Function2[js.Dynamic, js.Dynamic, js.Dictionary[Any]] = {
      (
          context: js.Dynamic,
          functionsAndVariables: js.Dynamic
      ) =>
        {
          val ctxt = DynamicJsParser.parseDynamicJs(context)
          val fnsAndVars =
            DynamicJsParser.parseDynamicJs(functionsAndVariables)
          try {
            val result = new Evaluator(ctxt, fnsAndVars).evaluate(r)
            val asJs = DynamicJsParser
              .backToDynamic(result)
              .asInstanceOf[js.Dynamic]
            js.Dictionary[Any](
              "type" -> "evaluation_success",
              "result" -> asJs
            )
          } catch {
            case e: RuntimeException => {
              val msg = e.getMessage()
              js.Dictionary[Any](
                "type" -> "evaluation_failure",
                "msg" -> msg
              )
            }
          }
        }
    }
    js.Dictionary[Any](
      "type" -> "parse_success",
      "evaluate" -> evaluate,
      "getExpansions" -> getExpansions,
      "getExpansionsWithAll" -> getExpansionsWithAll,
      "methodsAndFunctions" -> methodAndFunctionNames.toJSArray,
      "serialize" -> serialize,
      "toJson" -> toJson
    )
  }

  @JSExport
  def compileExpression(
      input: String
  ): js.Dictionary[Any] = {
    val c = fastparse.parse(input, ExpressionParser.parse(_))

    c.fold(
      (msg, i, extra) =>
        js.Dictionary[Any](
          "type" -> "parse_failure",
          "msg" -> msg,
          "i" -> i
        ),
      (r: ExpressionSymbol, _) => loadAst(r)
    )
  }
  @JSExport
  def deserialize(
      byteBuffer: ByteBuffer
  ): js.Dictionary[Any] = {
    try {
      val ast = Unpickle[ExpressionSymbol].fromBytes(byteBuffer)
      loadAst(ast)
    } catch {
      case e: RuntimeException => {
        js.Dictionary[Any](
          "type" -> "parse_failure",
          "msg" -> e.getMessage()
        )
      }
    }
  }
  @JSExport
  def fromJson(
      json: String
  ): js.Dictionary[Any] = {
    decode[ExpressionSymbol](json).fold(
      e =>
        js.Dictionary[Any](
          "type" -> "parse_failure",
          "msg" -> e.getMessage()
        ),
      loadAst(_)
    )
  }
}
