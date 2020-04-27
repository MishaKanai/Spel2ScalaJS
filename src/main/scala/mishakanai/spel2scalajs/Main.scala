package mishakanai.spel2scalajs
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSExport
import scala.util.parsing.combinator._
import scala.scalajs.js.JSON
import scala.scalajs.js.Dictionary
import js.JSConverters._
import js.typeOf
import fastparse._
@JSExportTopLevel("Spel2ScalaJs")
object SpelEval {
  @JSExport
  def evaluate(
      input: String,
      context: js.Dynamic,
      functionsAndVariables: js.Dynamic =
        js.Dictionary().asInstanceOf[js.Dynamic]
  ): js.Dynamic = {
    val result = SpelParser
      .apply(input)
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
  @JSExport
  def evaluateFast(
      input: String,
      context: js.Dynamic,
      functionsAndVariables: js.Dynamic =
        js.Dictionary().asInstanceOf[js.Dynamic]
  ): js.Dynamic = {
    val result = parse(input, ExpressionParser.parse(_))
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
}
