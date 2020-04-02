package mishakanai.spel2scalajs
import scala.scalajs.js
import scala.collection.immutable.Nil
import js.JSConverters._
import scala.collection.mutable

class Evaluator(rootContext: JSContext) {
  def evaluateFunction(
      fn: js.Dynamic,
      evaluatedArguments: List[js.Any]
  ): JSContext = {
    val jsResult = fn(evaluatedArguments.toSeq: _*)
    DynamicJsParser.parseDynamicJs(jsResult.asInstanceOf[js.Dynamic])
  }

  def applyStringBinOp(
      fn: (String, String) => JSContext,
      left: ExpressionSymbol,
      right: ExpressionSymbol
  ): JSContext = {
    val leftValue = evaluate(left) match {
      case JSCString(value) => Some(value)
      case _                => None
    }
    val rightValue = evaluate(right) match {
      case JSCString(value) => Some(value)
      case _                => None
    }
    (leftValue, rightValue) match {
      case (None, _) => throw new RuntimeException(s" $left is not a string")
      case (_, None) => throw new RuntimeException(s" $right is not a string")
      case (Some(x), Some(y)) => {
        fn(
          x,
          y
        )
      }
    }
  }
  def applyBinRelOp(
      fn: (Boolean, Boolean) => JSContext,
      left: ExpressionSymbol,
      right: ExpressionSymbol
  ): JSContext = {
    val leftValue = evaluate(left) match {
      case JSCBoolean(value) => Some(value)
      case _                 => None
    }
    val rightValue = evaluate(right) match {
      case JSCBoolean(value) => Some(value)
      case _                 => None
    }
    (leftValue, rightValue) match {
      case (None, _) => throw new RuntimeException(s" $left is not a boolean")
      case (_, None) => throw new RuntimeException(s" $right is not a boolean")
      case (Some(x), Some(y)) => {
        fn(
          x,
          y
        )
      }
    }
  }
  def applyBinFloatOp(
      fn: (Double, Double) => JSContext,
      left: ExpressionSymbol,
      right: ExpressionSymbol
  ): JSContext = {
    val leftValue = evaluate(left) match {
      case JSCFloat(value) => Some(value)
      case _               => None
    }
    val rightValue = evaluate(right) match {
      case JSCFloat(value) => Some(value)
      case _               => None
    }
    (leftValue, rightValue) match {
      case (None, _) => throw new RuntimeException(s" $left is not a float")
      case (_, None) => throw new RuntimeException(s" $right is not a float")
      case (Some(x), Some(y)) => {
        fn(
          x,
          y
        )
      }
    }
  }
  // we build a context stack
  var stack = new mutable.Stack[JSContext]()
  stack.push(rootContext)

  def getVariableValueInContext(variable: String): Option[JSContext] = {
    stack.toList
      .foldLeft[Option[JSContext]](None)((prev, curr) => {
        if (prev.isDefined) prev
        else if (variable == "this") Some(curr)
        else
          curr match {
            case JSCDictionary(value) => {
              return value.get(variable)
            }
            case _ => None
          }
      })
  }

  def evaluate(
      ast: ExpressionSymbol
  ): JSContext = {
    ast match {
      case StringLiteral(value) => JSCString(value)
      case Ternary(expression, ifTrue, ifFalse) =>
        evaluate(expression) match {
          case JSCBoolean(value) => evaluate(ifTrue)
          case _                 => evaluate(ifFalse)
        }
      case VariableReference(variableName) => {
        val valueInContext: Option[JSContext] = getVariableValueInContext(
          variableName
        );
        if (valueInContext.isEmpty) {
          throw new RuntimeException(
            s"Null Pointer Exception: variable $variableName not found"
          )
        }
        return valueInContext.get
      }
      case SelectionFirst(nullSafeNavigation, expression) => {
        throw new RuntimeException("SelectionFirst Not Implemented")
      }
      case SelectionLast(nullSafeNavigation, expression) => {
        throw new RuntimeException("SelectionLast Not Implemented")
      }
      case SelectionAll(nullSafeNavigation, expression) => {
        throw new RuntimeException("Selection Not Implemented")
      }
      case PropertyReference(nullSafeNavigation, propertyName) => {
        val valueInContext: Option[JSContext] = getVariableValueInContext(
          propertyName
        );
        if (valueInContext.isEmpty) {
          if (nullSafeNavigation) {
            null
          } else {
            throw new RuntimeException(
              s"Null Pointer Exception: Property $propertyName not found in context $stack"
            )
          }

        }
        return valueInContext.get
      }
      case Projection(nullSafeNavigation, expression) => {
        throw new RuntimeException("Projection Not Implemented")
      }
      case OpPower(base, expression) =>
        applyBinFloatOp(
          (a, b) => JSCFloat(scala.math.pow(a, b)),
          base,
          expression
        )
      case OpPlus(left, right) =>
        applyBinFloatOp((a, b) => JSCFloat(a + b), left, right)
      case OpOr(left, right) =>
        applyBinRelOp((a, b) => JSCBoolean(a || b), left, right)
      case OpNot(expression) =>
        evaluate(expression) match {
          case JSCBoolean(value) => JSCBoolean(!value)
          case _ => {
            throw new RuntimeException(
              s"value $expression is not Boolean for ! operator"
            )
          }
        }
      case OpNE(left, right) => {
        (evaluate(left), evaluate(right)) match {
          case (JSCBoolean(x), JSCBoolean(y)) => JSCBoolean(x != y)
          case (JSCFloat(x), JSCFloat(y))     => JSCBoolean(x != y)
          case (JSCString(x), JSCString(y))   => JSCBoolean(x != y)
          case (JSCNull(), JSCNull())         => JSCBoolean(false)
          case _                              => JSCBoolean(true)
        }
      }
      case OpMultiply(left, right) =>
        applyBinFloatOp((a, b) => JSCFloat(a * b), left, right)
      case OpModulus(left, right) =>
        applyBinFloatOp((a, b) => JSCFloat(a % b), left, right)
      case OpMinus(left, right) =>
        applyBinFloatOp((a, b) => JSCFloat(a - b), left, right)
      case OpMatches(left, right) =>
        applyStringBinOp((a, b) => JSCBoolean(a.matches(b)), left, right)
      case OpLT(left, right) =>
        applyBinFloatOp((a, b) => JSCBoolean(a < b), left, right)
      case OpLE(left, right) =>
        applyBinFloatOp((a, b) => JSCBoolean(a <= b), left, right)
      case OpGT(left, right) =>
        applyBinFloatOp((a, b) => JSCBoolean(a > b), left, right)
      case OpGE(left, right) =>
        applyBinFloatOp((a, b) => JSCBoolean(a >= b), left, right)
      case OpEQ(left, right) => {
        (evaluate(left), evaluate(right)) match {
          case (JSCBoolean(x), JSCBoolean(y)) => JSCBoolean(x == y)
          case (JSCFloat(x), JSCFloat(y))     => JSCBoolean(x == y)
          case (JSCString(x), JSCString(y))   => JSCBoolean(x == y)
          case (JSCNull(), JSCNull())         => JSCBoolean(true)
          case _                              => JSCBoolean(false)
        }
      }
      case OpDivide(left, right) =>
        applyBinFloatOp((a, b) => JSCFloat(a / b), left, right)
      case OpAnd(left, right) =>
        applyBinRelOp((a, b) => JSCBoolean(a && b), left, right)
      case Negative(value) => {
        evaluate(value) match {
          case JSCBoolean(value) => JSCBoolean(!value)
          case x => {
            throw new RuntimeException(s"Negation applied to $x")
          }

        }
      }
      case NumberLiteral(value) => JSCFloat(value)
      case NullLiteral()        => JSCNull()
      case MethodReference(nullSafeNavigation, methodName, args) => {
        throw new RuntimeException("Method Reference Not Implemented")
      }
      case FunctionReference(nullSafeNavigation, functionName, args) => {
        val valueInContext: Option[JSContext] = getVariableValueInContext(
          functionName
        )
        val evaluatedArguments = args
          .map(evaluate)
          .map(DynamicJsParser.backToDynamic)
        valueInContext match {
          case Some(value) => {
            value match {
              case JSCFunction(fun) => {
                evaluateFunction(fun, evaluatedArguments)
              }
              case ScalaFunction0(function) => {
                DynamicJsParser.parseDynamicJs(
                  function().asInstanceOf[js.Dynamic]
                )
              }
              case x => throw new RuntimeException(s"$x is not a function")
            }
          }
          case None => {
            if (!nullSafeNavigation) {
              throw new RuntimeException(s"Method $functionName Not found")
            }
            JSCNull()
          }
        }
      }
      case InlineMap(elements) => {
        JSCDictionary(
          elements.transform((k, expressionSymbol) =>
            evaluate(expressionSymbol)
          )
        )
      }
      case InlineList(elements) => {
        JSCArray(elements.map(evaluate).toJSArray)
      }
      case Indexer(nullSafeNavigation, index) => {
        throw new RuntimeException("Indexer Not Implemented")
      }
      case Elvis(expression, ifFalse) => {
        val expr = evaluate(expression)
        if (expr == JSCNull()) evaluate(ifFalse) else expr
      }
      case CompoundExpression(expressionComponents) => {
        // TODO
        // implement safe-navigation here
        val res = expressionComponents.foldLeft(rootContext)(
          (currContext, expressionSymbol) => {
            val res = evaluate(expressionSymbol)
            stack.push(res);
            res;
          }
        )
        expressionComponents.foreach((c) => {
          stack.pop
        })
        res;
      }
      case BooleanLiteral(value) => JSCBoolean(value)
    }
  }
}
