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
  def evaluate(input: String, context: js.Dynamic): js.Dynamic = {
    val result = SpelParser
      .apply(input)
      .map(p => {
        val ctxt = DynamicJsParser.parseDynamicJs(context)
        new Evaluator(ctxt).evaluate(p)
      })
    if (result.isDefined)
      return DynamicJsParser.backToDynamic(result.get).asInstanceOf[js.Dynamic]
    else {
      throw new RuntimeException("Failed to Parse Spel expression")
    }
  }
}
