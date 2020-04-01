package mishakanai.spel2scalajs
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.parsing.combinator._
import scala.scalajs.js.JSON
import scala.scalajs.js.Dictionary
import js.JSConverters._
import js.typeOf

object SpelEval {
  @JSExportTopLevel("evaluate")
  def evaluate(input: String, context: js.Dynamic) = {
    SpelParser
      .apply(input)
      .map(
        new Evaluator(DynamicJsParser.parseDynamicJs(context)).evaluate
      )
  }
}
