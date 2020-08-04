package mishakanai.spel2scalajs
import fastparse._
import scala.collection.mutable

object Minimize {
  def depth(ast: ExpressionSymbol, curr: Int = 0): Int = {
    ast match {
      case StringLiteral(value) => curr
      case Ternary(expression, ifTrue, ifFalse) =>
        List(
          depth(expression, curr + 1),
          depth(ifTrue, curr + 1),
          depth(ifFalse, curr + 1)
        ).reduce(_ max _)
      case VariableReference(variableName) =>
        curr
      case SelectionFirst(nullSafeNavigation, expression) =>
        depth(expression, curr + 1)
      case SelectionLast(nullSafeNavigation, expression) =>
        depth(expression, curr + 1)
      case SelectionAll(nullSafeNavigation, expression) =>
        depth(expression, curr + 1)
      case PropertyReference(nullSafeNavigation, propertyName) => curr
      case Projection(nullSafeNavigation, expression) =>
        depth(expression, curr + 1)
      case OpPower(base, expression) =>
        depth(base, curr + 1) max depth(expression, curr + 1)
      case OpPlus(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpOr(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpNot(expression) =>
        depth(expression, curr + 1)
      case OpNE(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpMultiply(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpModulus(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpMinus(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpMatches(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpLT(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpLE(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpGT(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpGE(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpEQ(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpDivide(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case OpAnd(left, right) =>
        depth(left, curr + 1) max depth(right, curr + 1)
      case Negative(value) =>
        depth(value, curr + 1)
      case NumberLiteral(value) =>
        curr
      case NullLiteral() =>
        curr
      case MethodReference(nullSafeNavigation, methodName, args) =>
        args.map(a => depth(a, curr + 1)).reduce(_ max _)
      case FunctionReference(nullSafeNavigation, functionName, args) =>
        args.map(a => depth(a, curr + 1)).reduce(_ max _)
      case InlineMap(elements) =>
        elements.values.map(a => depth(a, curr + 1)).reduce(_ max _)
      case InlineList(elements) =>
        elements.map(a => depth(a, curr + 1)).reduce(_ max _)
      case Indexer(nullSafeNavigation, index) =>
        depth(index, curr + 1)
      case Elvis(expression, ifFalse) =>
        depth(expression, curr + 1) max depth(ifFalse, curr + 1)
      case CompoundExpression(expressionComponents) =>
        expressionComponents.map(a => depth(a, curr + 1)).reduce(_ max _)
      case BooleanLiteral(value) =>
        curr
    }
  }
  def reduceVarReplacements[T](
      ast: ExpressionSymbol,
      reducer: (T, String) => T,
      prev: T
  ): T = {
    ast match {
      case StringLiteral(value) => prev
      case Ternary(expression, ifTrue, ifFalse) =>
        reduceVarReplacements(
          ifFalse,
          reducer,
          reduceVarReplacements(
            ifTrue,
            reducer,
            reduceVarReplacements(expression, reducer, prev)
          )
        )
      case VariableReference(variableName) =>
        prev
      case SelectionFirst(nullSafeNavigation, expression) =>
        reduceVarReplacements(expression, reducer, prev)
      case SelectionLast(nullSafeNavigation, expression) =>
        reduceVarReplacements(expression, reducer, prev)
      case SelectionAll(nullSafeNavigation, expression) =>
        reduceVarReplacements(expression, reducer, prev)
      case PropertyReference(nullSafeNavigation, propertyName) =>
        if (propertyName.startsWith("$"))
          reducer(prev, propertyName)
        else prev
      case Projection(nullSafeNavigation, expression) =>
        reduceVarReplacements(expression, reducer, prev)
      case OpPower(base, expression) =>
        reduceVarReplacements(
          base,
          reducer,
          reduceVarReplacements(expression, reducer, prev)
        )
      case OpPlus(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpOr(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpNot(expression) => reduceVarReplacements(expression, reducer, prev)
      case OpNE(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpMultiply(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpModulus(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpMinus(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpMatches(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpLT(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpLE(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpGT(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpGE(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpEQ(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpDivide(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case OpAnd(left, right) =>
        reduceVarReplacements(
          right,
          reducer,
          reduceVarReplacements(left, reducer, prev)
        )
      case Negative(value) =>
        reduceVarReplacements(value, reducer, prev)
      case NumberLiteral(value) =>
        prev
      case NullLiteral() =>
        prev
      case MethodReference(nullSafeNavigation, methodName, args) =>
        args.foldLeft(prev)((_prev, curr) =>
          reduceVarReplacements(curr, reducer, _prev)
        )
      case FunctionReference(nullSafeNavigation, functionName, args) =>
        args.foldLeft(prev)((_prev, curr) =>
          reduceVarReplacements(curr, reducer, _prev)
        )
      case InlineMap(elements) =>
        elements.values.foldLeft(prev)((_prev, curr) =>
          reduceVarReplacements(curr, reducer, _prev)
        )
      case InlineList(elements) =>
        elements.foldLeft(prev)((_prev, curr) =>
          reduceVarReplacements(curr, reducer, _prev)
        )
      case Indexer(nullSafeNavigation, index) =>
        reduceVarReplacements(index, reducer, prev)
      case Elvis(expression, ifFalse) =>
        reduceVarReplacements(
          ifFalse,
          reducer,
          reduceVarReplacements(expression, reducer, prev)
        )
      case CompoundExpression(expressionComponents) =>
        expressionComponents.foldLeft(prev)((_prev, curr) =>
          reduceVarReplacements(curr, reducer, _prev)
        )
      case BooleanLiteral(value) => prev
    }
  }

  def minimizeExpressions(expMap: Map[String, String], max: Int = 100): (
      Map[String, String],
      Map[String, String]
  ) = {
    val m: Map[String, ExpressionSymbol] =
      expMap.transform[ExpressionSymbol]((k, v) => {
        val Parsed.Success(result, _) =
          fastparse.parse(v, ExpressionParser.expression(_))
        result
      })
    val (added, transformed) = getReplacements(m, max)
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

    def add(ast: ExpressionSymbol) = {
      if (added.contains(ast.toString())) {
        added.update(ast.toString(), (added(ast.toString())._1 + 1, ast))
      } else {
        added.addOne(ast.toString(), (1, ast))
      }
    }
    def iterate(
        ast: ExpressionSymbol,
        isTop: Boolean = false,
        isTopMost: Boolean = false
    ): Unit = {
      ast match {
        case StringLiteral(value)                                => {}
        case VariableReference(variableName)                     => {}
        case PropertyReference(nullSafeNavigation, propertyName) => {}
        case NumberLiteral(value)                                => {}
        case NullLiteral()                                       => {}
        case BooleanLiteral(value)                               => {}
        case x => {
          /*
            We want only 'fully' complete subexpressions - so no taking 'foo.bar' out of 'foo && foo.bar'
            (because foo.bar alone short-circuits)
            Therefore, we only extract + replace (we can add on to this list):
            1. subexpressions that are also used as complete expressions in our astMap
            2. function calls we know are short circuited.
              Right now I'm hardcoding my use case...
           */
          val isLookupWithNoCompound = x match {
            case FunctionReference(
                nullSafeNavigation,
                "lookupEntityData",
                entityName :: PropertyReference(nsn, pn) :: pathn
                ) =>
              true
            case MethodReference(
                nullSafeNavigation,
                "lookupEntityData",
                entityName :: PropertyReference(nsn, pn) :: pathn
                ) =>
              true
            case _ => false
          }
          /* TODO
          Include: Anything that's the beginning of a top-level ternary
          Anything that's the beginning of a top-level elvis
          Anything that's the beginning of a top-level && or top-level ||
           */

          /*
            TODO add an 'is REALLY top to distinguish'
            We want to prevent adding expressions that are ONLY top level expressions (in other words, if it exists in astMap,
            add it only if we are not at the 'real' top.)
           */

          /*
            Add to our count if:
            - it's the top-level expression (only count these ONCE though - no repeats)
            - we are sub-top-level on a valid subpath we ccan add on
            - we are not top-level, but we found our ast in a top-level


           */
          if ((isTopMost && astMap
                .filter(t => t._2 == x)
                .size == 1) || (isTop && !isTopMost) || (!isTopMost && astMap
                .exists(t => t._2 == x)) || isLookupWithNoCompound) {
            add(x)
          }
          x match {
            case Ternary(expression, ifTrue, ifFalse) => {
              iterate(expression, isTop)
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
              iterate(left, isTop)
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
              iterate(left, isTop)
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
    astMap.foreach(t => iterate(t._2, true, true))
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

  val alphabet = "abcdefghijklmnopqrstuvwxyz"
    .toCharArray();
  val replacementKeys =
    alphabet
      .concat(
        1.to(100).map(i => i).flatMap(i => alphabet.map(a => a + s"$i"))
      )
      .map(c => "$" + c)
  def getReplacements(
      expressionsMap: Map[String, ExpressionSymbol],
      max: Int = 100
  ): (Map[String, ExpressionSymbol], Map[String, ExpressionSymbol]) = {
    val addedVariables = mutable.HashMap[String, ExpressionSymbol]()

    // we will iterate until empty-
    // iterate until added (without filters) is empty
    //  filter no replacements
    //    iterate over possibilities
    //  filter by only replacements in previous step
    //    iterate over possibilities
    // then
    var localCopyOfVariablesBeforeIteration = addedVariables.clone();
    var variableRefDepth = 0
    def replace2(
        _exprMap: Map[String, ExpressionSymbol],
        i: Int = 0
    ): Map[String, ExpressionSymbol] = {
      if (i > max) {
        return _exprMap
      }
      val added = Minimize.minimize(_exprMap).filter(t => t._2._1 > 1)
      if (added.isEmpty) {
        _exprMap
      } else {
        val localToAdd = added.filter(t =>
          reduceVarReplacements(
            t._2._2,
            (prev: Boolean, curr: String) =>
              localCopyOfVariablesBeforeIteration.contains(
                curr
              ), //except when it's a varReplacement that was added after our current iteration began
            true // by default include expressions
          )
        )
        if (localToAdd.isEmpty) {
          localCopyOfVariablesBeforeIteration = addedVariables.clone();
          variableRefDepth += 1;
          replace2(_exprMap, i)
        } else {
          // lets iterate/recurse down until no more to add, then continue the larger recursion.
          val (
            maxAstStr,
            (maxCount, astToReplace)
          ) = // get the deepest ast that can be replaced
            localToAdd.reduce((prev, curr) =>
              // deeper is better
              if (depth(curr._2._2) > depth(prev._2._2)) curr
              else if (curr._2._1 > prev._2._1) curr
              else prev
            )
          val newVariableKey =
            ("$" * variableRefDepth) + replacementKeys(i)
          val outputAst = _exprMap.transform((k, ast) =>
            replaceInAst(
              ast,
              astToReplace,
              newVariableKey
            )
          )
          addedVariables.addOne((newVariableKey, astToReplace))
          replace2(outputAst, i + 1)
        }
      }
    }

    val resultExpressions = replace2(expressionsMap)
    return (
      addedVariables.toMap,
      resultExpressions
    )
  }
}
