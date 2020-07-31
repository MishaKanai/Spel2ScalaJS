package mishakanai.spel2scalajs
import fastparse._
import scala.collection.mutable

object Minimize {

  def minimizeExpressions(expMap: Map[String, String]): (
      Map[String, String],
      Map[String, String]
  ) = {
    val m: Map[String, ExpressionSymbol] =
      expMap.transform[ExpressionSymbol]((k, v) => {
        val Parsed.Success(result, _) =
          fastparse.parse(v, ExpressionParser.expression(_))
        result
      })
    val (added, transformed) = getReplacements(m)
    (
      added.transform((k, ast) => AstToString.astToString(ast)),
      transformed.transform((k, ast) => AstToString.astToString(ast))
    )
  }

  def minimize(
      astMap: Map[String, ExpressionSymbol]
  ): mutable.Map[String, (Int, ExpressionSymbol)] = {
    val added: mutable.Map[String, (Int, ExpressionSymbol)] =
      mutable.HashMap[String, (Int, ExpressionSymbol)]()
    def iterate(ast: ExpressionSymbol): Unit = {
      ast match {
        case StringLiteral(value)                                => {}
        case VariableReference(variableName)                     => {}
        case PropertyReference(nullSafeNavigation, propertyName) => {}
        case NumberLiteral(value)                                => {}
        case NullLiteral()                                       => {}
        case BooleanLiteral(value)                               => {}
        case x => {
          if (added.contains(x.toString())) {
            added.update(x.toString(), (added(x.toString())._1 + 1, x))
          } else {
            added.addOne(x.toString(), (1, x))
          }
          x match {
            case Ternary(expression, ifTrue, ifFalse) => {
              iterate(expression)
              iterate(ifTrue)
              iterate(ifFalse)
            }
            case SelectionFirst(nullSafeNavigation, expression) => {
              iterate(expression)
            }
            case SelectionLast(nullSafeNavigation, expression) => {
              iterate(expression)
            }
            case SelectionAll(nullSafeNavigation, expression) => {
              iterate(expression)
            }
            case Projection(nullSafeNavigation, expression) => {
              iterate(expression)
            }
            case OpPower(base, expression) => {
              iterate(base)
              iterate(expression)
            }
            case OpPlus(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpOr(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpNot(expression) => {
              iterate(expression)
            }
            case OpNE(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpMultiply(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpModulus(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpMinus(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpMatches(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpLT(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpLE(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpGT(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpGE(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpEQ(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpDivide(left, right) => {
              iterate(left)
              iterate(right)
            }
            case OpAnd(left, right) => {
              iterate(left)
              iterate(right)
            }
            case Negative(value) => {
              iterate(value)
            }
            case MethodReference(nullSafeNavigation, methodName, args) => {
              args.foreach(iterate(_))
            }
            case FunctionReference(nullSafeNavigation, functionName, args) => {
              args.foreach(iterate(_))
            }
            case InlineMap(elements) => {
              elements.foreach(f => iterate(f._2))
            }
            case InlineList(elements) => {
              elements.foreach(iterate(_))
            }
            case Indexer(nullSafeNavigation, index) => {
              iterate(index)
            }
            case Elvis(expression, ifFalse) => {
              iterate(expression)
              iterate(ifFalse)
            }
            case CompoundExpression(expressionComponents) => {
              expressionComponents.foreach(iterate(_))
            }
          }
        }
      }
    }
    astMap.foreach(t => iterate(t._2))
    added
  }

  def replaceInAst(
      ast: ExpressionSymbol,
      replaceWith: ExpressionSymbol,
      variableName: String
  ): ExpressionSymbol = {
    if (ast == replaceWith) {
      PropertyReference(false, variableName)
    } else {
      val replaceIt = (exp: ExpressionSymbol) =>
        replaceInAst(exp, replaceWith, variableName);
      ast match {
        case Ternary(expression, ifTrue, ifFalse) =>
          Ternary(
            replaceIt(expression),
            replaceIt(ifTrue),
            replaceIt(ifFalse)
          )
        case SelectionFirst(nullSafeNavigation, expression) =>
          SelectionFirst(
            nullSafeNavigation,
            replaceIt(expression)
          )
        case SelectionLast(nullSafeNavigation, expression) =>
          SelectionLast(
            nullSafeNavigation,
            replaceIt(expression)
          )
        case SelectionAll(nullSafeNavigation, expression) =>
          SelectionAll(
            nullSafeNavigation,
            replaceIt(expression)
          )
        case Projection(nullSafeNavigation, expression) =>
          Projection(
            nullSafeNavigation,
            replaceIt(expression)
          )
        case OpPower(base, expression) =>
          OpPower(replaceIt(base), replaceIt(expression))
        case OpPlus(left, right) =>
          OpPlus(replaceIt(left), replaceIt(right))
        case OpOr(left, right) =>
          OpOr(replaceIt(left), replaceIt(right))
        case OpNot(expression) =>
          OpNot(replaceIt(expression))
        case OpNE(left, right) =>
          OpNE(replaceIt(left), replaceIt(right))
        case OpMultiply(left, right) =>
          OpMultiply(replaceIt(left), replaceIt(right))
        case OpModulus(left, right) =>
          OpModulus(replaceIt(left), replaceIt(right))
        case OpMinus(left, right) =>
          OpMinus(replaceIt(left), replaceIt(right))
        case OpMatches(left, right) =>
          OpMatches(replaceIt(left), replaceIt(right))
        case OpLT(left, right) =>
          OpLT(replaceIt(left), replaceIt(right))
        case OpLE(left, right) =>
          OpLE(replaceIt(left), replaceIt(right))
        case OpGT(left, right) =>
          OpGT(replaceIt(left), replaceIt(right))
        case OpGE(left, right) =>
          OpGE(replaceIt(left), replaceIt(right))
        case OpEQ(left, right) =>
          OpEQ(replaceIt(left), replaceIt(right))
        case OpDivide(left, right) =>
          OpDivide(replaceIt(left), replaceIt(right))
        case OpAnd(left, right) =>
          OpAnd(replaceIt(left), replaceIt(right))
        case Negative(value) =>
          Negative(replaceIt(value))
        case MethodReference(nullSafeNavigation, methodName, args) =>
          MethodReference(
            nullSafeNavigation,
            methodName,
            args.map(replaceIt(_))
          )
        case FunctionReference(nullSafeNavigation, functionName, args) =>
          FunctionReference(
            nullSafeNavigation,
            functionName,
            args.map(replaceIt(_))
          )
        case InlineMap(elements) =>
          InlineMap(elements.transform((k, v) => replaceIt(v)))
        case InlineList(elements) =>
          InlineList(elements.map(replaceIt(_)))
        case Indexer(nullSafeNavigation, index) =>
          Indexer(nullSafeNavigation, replaceIt(index))
        case Elvis(expression, ifFalse) =>
          Elvis(replaceIt(expression), replaceIt(ifFalse))
        case CompoundExpression(expressionComponents) =>
          CompoundExpression(expressionComponents.map(replaceIt(_)))
        case x => x
      }
    }
  }

  val replacementKeys =
    "abcdefghijklmnopqrstuv"
      .toCharArray()
      .concat(
        "abcdefghijklmnopqrstuv"
          .toCharArray()
          .flatMap(c => {
            "abcdefghijklmnopqrstuv".toCharArray().map(c2 => c + c2)
          })
      )
      .map(c => "$" + c)
  def getReplacements(
      expressionsMap: Map[String, ExpressionSymbol]
  ): (Map[String, ExpressionSymbol], Map[String, ExpressionSymbol]) = {
    val addedVariables = mutable.HashMap[String, ExpressionSymbol]()
    def replaceIfPossible(
        _exprMap: Map[String, ExpressionSymbol],
        i: Int = 0
    ): Map[String, ExpressionSymbol] = {
      // subAsts by count.
      // lets find the one with the highest count, consider it our next replacement,
      // replace all instances, and recurse (or continue iteration.)
      val added = Minimize.minimize(_exprMap)
      val (maxAstStr, (maxCount, astToReplace)) =
        added.reduce((prev, curr) =>
          if (curr._2._1 > prev._2._1) curr else prev
        )
      if (maxCount == 1) {
        _exprMap;
      } else {
        val outputAst = _exprMap.transform((k, ast) =>
          replaceInAst(ast, astToReplace, replacementKeys(i))
        )
        addedVariables.addOne((replacementKeys(i), astToReplace))
        replaceIfPossible(outputAst, i + 1)
      }
    }

    val resultExpressions = replaceIfPossible(expressionsMap)
    return (
      addedVariables.toMap,
      resultExpressions
    )
  }
}
