package mishakanai.spel2scalajs
import scala.scalajs.js
import scala.collection.immutable.Nil
import js.JSConverters._
import scala.collection.mutable
import js.typeOf

class EvaluatorToJS(
    rootContext: js.Dynamic,
    functionsAndVariables: js.Dynamic
) {
  def evaluateFunction(
      fn: js.Dynamic,
      evaluatedArguments: List[js.Any]
  ): js.Dynamic = {
    val jsResult = fn(evaluatedArguments.toSeq: _*)
    jsResult
  }

  def applyStringBinOp(
      fn: (String, String) => js.Dynamic,
      left: ExpressionSymbol,
      right: ExpressionSymbol
  ): js.Dynamic = {
    val x = evaluate(left);
    val y = evaluate(right)
    if (typeOf(x) != "string") {
      throw new RuntimeException(s" $x is not a string")
    }
    if (typeOf(y) != "string") {
      throw new RuntimeException(s" $y is not a string")
    }
    fn(x.asInstanceOf[String], y.asInstanceOf[String])
  }

  def orOperator(
      left: ExpressionSymbol,
      right: ExpressionSymbol
  ): js.Dynamic = {
    val leftValue = evaluate(left)
    if (leftValue == null || (typeOf(leftValue) == "boolean" && leftValue
          .asInstanceOf[Boolean] == false) ||
        (typeOf(leftValue) == "string" && leftValue
          .asInstanceOf[String] == "")) {
      val rightValue = evaluate(right)
      return rightValue
    }
    return leftValue;
  }
  def andOperator(
      left: ExpressionSymbol,
      right: ExpressionSymbol
  ): js.Dynamic = {
    val leftValue = evaluate(left)
    if (leftValue == null || (typeOf(leftValue) == "boolean" && leftValue
          .asInstanceOf[Boolean] == false) ||
        (typeOf(leftValue) == "string" && leftValue
          .asInstanceOf[String] == "")) {
      leftValue
    } else evaluate(right)
  }

  def applyBinFloatOp(
      fn: (Double, Double) => js.Dynamic,
      left: ExpressionSymbol,
      right: ExpressionSymbol
  ): js.Dynamic = {
    val x = evaluate(left);
    val y = evaluate(right)
    if (!x.isInstanceOf[Number]) {
      throw new RuntimeException(s" $x is not a number")
    }
    if (!y.isInstanceOf[Number]) {
      throw new RuntimeException(s" $y is not a number")
    }
    fn(x.asInstanceOf[Double], y.asInstanceOf[Double])
  }
  // we build a context stack
  var stack = new mutable.Stack[js.Dynamic]()
  stack.push(rootContext.asInstanceOf[js.Dynamic])

  def getPropertyValueInContext(variable: String): Option[js.Dynamic] = {
    stack.toList
      .foldLeft[Option[js.Dynamic]](None)((prev, curr) => {
        if (prev.isDefined) prev
        else if (variable == "this") Some(curr)
        else {
          if (DynamicJsParser.isJSDict(curr)) {
            val dict = curr.asInstanceOf[js.Dictionary[js.Dynamic]]
            return dict.get(variable)

          }
          return None
        }
      })
  }
  def getValueInProvidedFuncsAndVars(
      variableName: String
  ): Option[js.Dynamic] = {
    if (variableName == "this") Some(stack.head)
    else if (variableName == "root") Some(stack.last)
    else
      functionsAndVariables
        .asInstanceOf[js.Dictionary[js.Dynamic]]
        .get(variableName)
  }
  def find(
      jsArray: js.Array[js.Dynamic],
      expression: ExpressionSymbol,
      reverse: Boolean
  ): js.Dynamic = {
    val value = if (reverse) jsArray.reverse else jsArray;
    value.find(e => {
      stack.push(e);
      val result = evaluate(expression);
      stack.pop();
      if (typeOf(result) != "boolean") {
        throw new RuntimeException(
          "Result of selection expression is not Boolean"
        );
      }
      result.asInstanceOf[Boolean]
    }) match {
      case Some(value) => value
      case None        => null
    }
  }

  def evaluate(
      ast: ExpressionSymbol
  ): js.Dynamic = {
    ast match {
      case StringLiteral(value) => value.asInstanceOf[js.Dynamic]
      case Ternary(expression, ifTrue, ifFalse) => {
        val result = evaluate(expression)
        if (typeOf(result) == "boolean") {
          evaluate(if (result.asInstanceOf[Boolean]) ifTrue else ifFalse)
        } else if (result == null) {
          evaluate(ifFalse)
        } else
          throw new RuntimeException(
            s"Unexpected non boolean/null in Ternary conditional expression: $result"
          )
      }

      case VariableReference(variableName) => {
        val valueInFuncsAndVars = getValueInProvidedFuncsAndVars(variableName)
        if (valueInFuncsAndVars.isEmpty) {
          throw new RuntimeException(
            s"Null Pointer Exception: variable $variableName not found"
          )
        }
        return valueInFuncsAndVars.get
      }
      case SelectionFirst(nullSafeNavigation, expression) => {
        val head = stack.head;
        if (head == null && nullSafeNavigation) null
        else if (js.Array.isArray(head))
          find(
            head.asInstanceOf[js.Array[js.Dynamic]],
            expression,
            false
          )
        else {
          val astHead = DynamicJsParser.parseDynamicJs(head)
          throw new RuntimeException(
            s"Cannot run selectionfirst expression on non-array: $astHead"
          )
        }
      }
      case SelectionLast(nullSafeNavigation, expression) => {
        val head = stack.head;
        if (head == null && nullSafeNavigation) null
        else if (js.Array.isArray(head))
          find(
            head.asInstanceOf[js.Array[js.Dynamic]],
            expression,
            true
          )
        else {
          val astHead = DynamicJsParser.parseDynamicJs(head)
          throw new RuntimeException(
            s"Cannot run selectionlast expression on non-array: $astHead"
          )
        }
      }
      case SelectionAll(nullSafeNavigation, expression) => {
        val head = stack.head;
        if (head == null && nullSafeNavigation) null
        else if (js.Array.isArray(head))
          head
            .asInstanceOf[js.Array[js.Dynamic]]
            .filter(v => {
              stack.push(v);
              val result = evaluate(expression);
              stack.pop();
              if (typeOf(result) == "boolean")
                (result.asInstanceOf[Boolean] == true)
              else
                throw new RuntimeException(
                  "Result of selectionall expression is not Boolean"
                );
            })
            .asInstanceOf[js.Dynamic]
        else {
          val astHead = DynamicJsParser.parseDynamicJs(head)
          throw new RuntimeException(
            s"Cannot run selectionall expression on non-array: $astHead"
          )
        }
      }
      case PropertyReference(nullSafeNavigation, propertyName) => {
        val valueInContext: Option[js.Dynamic] = getPropertyValueInContext(
          propertyName
        );
        if (valueInContext.isEmpty) {
          if (nullSafeNavigation) {
            null
          } else {
            val astStack = stack.map(DynamicJsParser.parseDynamicJs(_))
            throw new RuntimeException(
              s"Null Pointer Exception: Property $propertyName not found in context $astStack"
            )
          }

        }
        return valueInContext.get
      }
      case Projection(nullSafeNavigation, expression) => {
        val head = stack.head;
        if (head == null && nullSafeNavigation) null
        else if (js.Array.isArray(head))
          head
            .asInstanceOf[js.Array[js.Dynamic]]
            .map(v => {
              stack.push(v);
              val result = evaluate(expression);
              stack.pop();
              result;
            })
            .asInstanceOf[js.Dynamic]
        else {
          val astHead = DynamicJsParser.parseDynamicJs(head)
          throw new RuntimeException(
            s"Cannot run projection expression on non-array: $astHead"
          )
        }
      }
      case OpPower(base, expression) =>
        applyBinFloatOp(
          (a, b) => scala.math.pow(a, b).asInstanceOf[js.Dynamic],
          base,
          expression
        )
      case OpPlus(left, right) =>
        applyBinFloatOp((a, b) => (a + b).asInstanceOf[js.Dynamic], left, right)
      case OpOr(left, right) =>
        orOperator(left, right)

      case OpNot(expression) => {
        val result = evaluate(expression)
        if (typeOf(result) == "boolean") {
          return (!result.asInstanceOf[Boolean]).asInstanceOf[js.Dynamic]
        }
        if (result == null) {
          return true.asInstanceOf[js.Dynamic];
        }
        if (typeOf(result) == "string") {
          return (result.asInstanceOf[String] != "").asInstanceOf[js.Dynamic]
        }
        throw new RuntimeException(
          s"value $expression (evaluates to $result) is not Boolean for ! operator"
        )
      }
      case OpNE(left, right) => {
        val x = evaluate(left)
        val y = evaluate(right)
        if (typeOf(x) == "boolean" && typeOf(y) == "boolean") {
          return (x.asInstanceOf[Boolean] != y.asInstanceOf[Boolean])
            .asInstanceOf[js.Dynamic]
        }
        if (x.isInstanceOf[Number] && y.isInstanceOf[Number]) {
          return (x.asInstanceOf[Number] != y.asInstanceOf[Number])
            .asInstanceOf[js.Dynamic]
        }
        if (typeOf(x) == "string" && typeOf(y) == "string") {
          return (x.asInstanceOf[String] != y.asInstanceOf[String])
            .asInstanceOf[js.Dynamic]
        }
        if (x == null && y == null) {
          return false.asInstanceOf[js.Dynamic]
        }
        return true.asInstanceOf[js.Dynamic]
      }
      case OpMultiply(left, right) =>
        applyBinFloatOp((a, b) => (a * b).asInstanceOf[js.Dynamic], left, right)
      case OpModulus(left, right) =>
        applyBinFloatOp((a, b) => (a % b).asInstanceOf[js.Dynamic], left, right)
      case OpMinus(left, right) =>
        applyBinFloatOp((a, b) => (a - b).asInstanceOf[js.Dynamic], left, right)
      case OpMatches(left, right) =>
        applyStringBinOp(
          (a, b) => a.matches(b).asInstanceOf[js.Dynamic],
          left,
          right
        )
      case OpLT(left, right) =>
        applyBinFloatOp((a, b) => (a < b).asInstanceOf[js.Dynamic], left, right)
      case OpLE(left, right) =>
        applyBinFloatOp(
          (a, b) => (a <= b).asInstanceOf[js.Dynamic],
          left,
          right
        )
      case OpGT(left, right) =>
        applyBinFloatOp((a, b) => (a > b).asInstanceOf[js.Dynamic], left, right)
      case OpGE(left, right) =>
        applyBinFloatOp(
          (a, b) => (a >= b).asInstanceOf[js.Dynamic],
          left,
          right
        )
      case OpEQ(left, right) => {
        val x = evaluate(left)
        val y = evaluate(right)
        if (typeOf(x) == "boolean" && typeOf(y) == "boolean") {
          return (x.asInstanceOf[Boolean] == y.asInstanceOf[Boolean])
            .asInstanceOf[js.Dynamic]
        }
        if (x.isInstanceOf[Number] && y.isInstanceOf[Number]) {
          return (x.asInstanceOf[Number] == y.asInstanceOf[Number])
            .asInstanceOf[js.Dynamic]
        }
        if (typeOf(x) == "string" && typeOf(y) == "string") {
          return (x.asInstanceOf[String] == y.asInstanceOf[String])
            .asInstanceOf[js.Dynamic]
        }
        if (x == null && y == null) {
          return true.asInstanceOf[js.Dynamic]
        }
        return false.asInstanceOf[js.Dynamic]
      }
      case OpDivide(left, right) =>
        applyBinFloatOp((a, b) => (a / b).asInstanceOf[js.Dynamic], left, right)
      case OpAnd(left, right) =>
        andOperator(
          left,
          right
        )
      case Negative(value) => {
        val result = evaluate(value)
        if (result.isInstanceOf[Number]) {
          return (result.asInstanceOf[Double] * -1).asInstanceOf[js.Dynamic]
        }
        throw new RuntimeException(s"unary (-) operator applied to $result")

      }
      case NumberLiteral(value) => value.asInstanceOf[js.Dynamic]
      case NullLiteral()        => null
      case MethodReference(nullSafeNavigation, methodName, args) => {
        val valueInContext: Option[js.Dynamic] = getPropertyValueInContext(
          methodName
        )
        val evaluatedArguments = args
          .map(evaluate)

        valueInContext match {
          case Some(value) => {
            if (typeOf(value) == "function") {
              return evaluateFunction(value, evaluatedArguments)
            } else if (typeOf(value) == "object" && value != null &&
                       value
                         .asInstanceOf[scala.scalajs.js.Object]
                         .toString()
                         .matches("<function0>")) {
              return value
                .asInstanceOf[Function0[Any]]()
                .asInstanceOf[js.Dynamic]
            } else {
              throw new RuntimeException(s"$value is not a function")
            }
          }
          case None => {
            if (!nullSafeNavigation) {
              throw new RuntimeException(s"Method $methodName Not found")
            }
            null
          }
        }
      }
      case FunctionReference(nullSafeNavigation, functionName, args) => {
        val maybeProvidedFunction: Option[js.Dynamic] =
          getValueInProvidedFuncsAndVars(
            functionName
          )
        val evaluatedArguments = args
          .map(evaluate)
        maybeProvidedFunction match {
          case Some(value) => {
            if (typeOf(value) == "function") {
              return evaluateFunction(value, evaluatedArguments)
            } else if (typeOf(value) == "object" && value != null &&
                       value
                         .asInstanceOf[scala.scalajs.js.Object]
                         .toString()
                         .matches("<function0>")) {
              return value
                .asInstanceOf[Function0[Any]]()
                .asInstanceOf[js.Dynamic]
            } else {
              throw new RuntimeException(s"$value is not a function")
            }
          }
          case None => {
            if (!nullSafeNavigation) {
              throw new RuntimeException(s"Method $functionName Not found")
            }
            null
          }
        }
      }
      case InlineMap(elements) => {
        elements
          .transform((k, expressionSymbol) => evaluate(expressionSymbol))
          .toJSDictionary
          .asInstanceOf[js.Dynamic]
      }
      case InlineList(elements) => {
        elements.map(evaluate).toJSArray.asInstanceOf[js.Dynamic]
      }
      case Indexer(nullSafeNavigation, indexSymbol) => {
        val head = stack.head
        if (head == null && nullSafeNavigation) {
          return null
        }
        val value = indexSymbol match {
          case NumberLiteral(n) => {
            if (typeOf(head) == "string")
              head
                .asInstanceOf[String]
                .charAt(n.toInt)
                .toString
                .asInstanceOf[js.Dynamic]
            else if (js.Array.isArray(head))
              head
                .asInstanceOf[js.Array[js.Dynamic]](n.toInt)
                .asInstanceOf[js.Dynamic]
            else {
              val astHead = DynamicJsParser.parseDynamicJs(head)
              throw new RuntimeException(
                s"Not supported: indexing into $astHead with $indexSymbol"
              );
            }
          }
          case StringLiteral(s) => {
            if (DynamicJsParser.isJSDict(head))
              head
                .asInstanceOf[js.Dictionary[js.Dynamic]]
                .get(s) match {
                case Some(value) => value
                case None => {
                  val astHead = DynamicJsParser.parseDynamicJs(head)
                  throw new RuntimeException(
                    s"key $s not found in dictionary $astHead"
                  );
                }
              }
            else {
              val astHead = DynamicJsParser.parseDynamicJs(head)
              throw new RuntimeException(
                s"Not supported: indexing into $astHead with $indexSymbol"
              );
            }
          }
          case _ => {
            val astHead = DynamicJsParser.parseDynamicJs(head)
            throw new RuntimeException(
              s"Not supported: indexing into $astHead with $indexSymbol"
            );
          }
        }
        value
      }
      case Elvis(expression, ifFalse) => {
        val expr = evaluate(expression)
        if (expr == null) evaluate(ifFalse) else expr
      }
      case CompoundExpression(expressionComponents) => {
        // TODO
        // implement safe-navigation here
        val res = expressionComponents.foldLeft(
          rootContext.asInstanceOf[js.Dynamic]
        )((currContext, expressionSymbol) => {
          val res = evaluate(expressionSymbol)
          stack.push(res);
          res;
        })
        expressionComponents.foreach((c) => {
          stack.pop
        })
        res.asInstanceOf[js.Dynamic];
      }
      case BooleanLiteral(value) => value.asInstanceOf[js.Dynamic]
    }
  }
}
