package mishakanai.spel2scalajs

object AstToString {
  def opElvisShouldParen =
    (exp: ExpressionSymbol) =>
      exp match {
        case Ternary(expression, ifTrue, ifFalse) => true
        case Elvis(expression, ifFalse)           => true
        case _                                    => false
      }
  def opPowerShouldParen =
    (exp: ExpressionSymbol) =>
      exp match {
        case Ternary(expression, ifTrue, ifFalse) => true
        case Elvis(expression, ifFalse)           => true
        case OpAnd(left, right)                   => true
        case OpOr(left, right)                    => true
        case OpNE(left, right)                    => true
        case OpEQ(left, right)                    => true
        case OpGT(left, right)                    => true
        case OpGE(left, right)                    => true
        case OpLT(left, right)                    => true
        case OpLE(left, right)                    => true
        case OpMinus(left, right)                 => true
        case OpPlus(left, right)                  => true
        case OpMultiply(left, right)              => true
        case OpDivide(left, right)                => true
        case OpModulus(left, right)               => true
        case _                                    => false
      }
  def opAddShouldParen =
    (exp: ExpressionSymbol) =>
      exp match {
        case Ternary(expression, ifTrue, ifFalse) => true
        case Elvis(expression, ifFalse)           => true
        case OpAnd(left, right)                   => true
        case OpOr(left, right)                    => true
        case OpNE(left, right)                    => true
        case OpEQ(left, right)                    => true
        case OpGT(left, right)                    => true
        case OpGE(left, right)                    => true
        case OpLT(left, right)                    => true
        case OpLE(left, right)                    => true
        case OpMinus(left, right)                 => true
        case _                                    => false
      }
  def opMinusShouldParen =
    (exp: ExpressionSymbol) =>
      exp match {
        case Ternary(expression, ifTrue, ifFalse) => true
        case Elvis(expression, ifFalse)           => true
        case OpAnd(left, right)                   => true
        case OpOr(left, right)                    => true
        case OpNE(left, right)                    => true
        case OpEQ(left, right)                    => true
        case OpGT(left, right)                    => true
        case OpGE(left, right)                    => true
        case OpLT(left, right)                    => true
        case OpLE(left, right)                    => true
        case OpMinus(left, right)                 => true
        case OpPlus(left, right)                  => true
        case _                                    => false
      }
  def opOrShouldParen =
    (exp: ExpressionSymbol) =>
      exp match {
        case Ternary(expression, ifTrue, ifFalse) => true
        case Elvis(expression, ifFalse)           => true
        case _                                    => false
      }
  def opAndShouldParen =
    (exp: ExpressionSymbol) =>
      exp match {
        case Ternary(expression, ifTrue, ifFalse) => true
        case Elvis(expression, ifFalse)           => true
        case OpOr(left, right)                    => true
        case _                                    => false
      }
  def unaryShouldParen =
    (exp: ExpressionSymbol) =>
      exp match {
        case Ternary(expression, ifTrue, ifFalse) => true
        case OpPower(base, expression)            => true
        case OpPlus(left, right)                  => true
        case OpOr(left, right)                    => true
        case OpNE(left, right)                    => true
        case OpMultiply(left, right)              => true
        case OpModulus(left, right)               => true
        case OpMinus(left, right)                 => true
        case OpLT(left, right)                    => true
        case OpLE(left, right)                    => true
        case OpGT(left, right)                    => true
        case OpGE(left, right)                    => true
        case OpEQ(left, right)                    => true
        case OpDivide(left, right)                => true
        case OpAnd(left, right)                   => true
        case Negative(value)                      => true
        case Elvis(expression, ifFalse) =>
          true
        case _ => false
      }
  def opRelationalShouldParen =
    (exp: ExpressionSymbol) =>
      exp match {
        case Ternary(expression, ifTrue, ifFalse) => true
        case Elvis(expression, ifFalse)           => true
        case OpAnd(left, right)                   => true
        case OpOr(left, right)                    => true
        case _                                    => false
      }
  def opProductShouldParen =
    (exp: ExpressionSymbol) =>
      exp match {
        case Ternary(expression, ifTrue, ifFalse) => true
        case Elvis(expression, ifFalse)           => true
        case OpAnd(left, right)                   => true
        case OpOr(left, right)                    => true
        case OpNE(left, right)                    => true
        case OpEQ(left, right)                    => true
        case OpGT(left, right)                    => true
        case OpGE(left, right)                    => true
        case OpLT(left, right)                    => true
        case OpLE(left, right)                    => true
        case OpMinus(left, right)                 => true
        case OpPlus(left, right)                  => true
        case OpDivide(left, right)                => true
        case _                                    => false
      }
  def opDivideShouldParen =
    (exp: ExpressionSymbol) =>
      exp match {
        case Ternary(expression, ifTrue, ifFalse) => true
        case Elvis(expression, ifFalse)           => true
        case OpAnd(left, right)                   => true
        case OpOr(left, right)                    => true
        case OpNE(left, right)                    => true
        case OpEQ(left, right)                    => true
        case OpGT(left, right)                    => true
        case OpGE(left, right)                    => true
        case OpLT(left, right)                    => true
        case OpLE(left, right)                    => true
        case OpMinus(left, right)                 => true
        case OpPlus(left, right)                  => true
        case OpDivide(left, right)                => true
        case OpMultiply(left, right)              => true
        case _                                    => false
      }
  def astToString(
      ast: ExpressionSymbol,
      shouldParen: (ExpressionSymbol) => Boolean = (exp: ExpressionSymbol) =>
        false
  ): String = {
    val result = ast match {
      case StringLiteral(value) => Quote._quote('"')(value)
      case Ternary(expression, ifTrue, ifFalse) =>
        astToString(
          expression,
          exp =>
            exp match {
              case Ternary(expression, ifTrue, ifFalse) => true
              case _                                    => false
            }
        ) + " ? " + astToString(ifTrue) + " : " + astToString(ifFalse)
      case VariableReference(variableName) => s"#$variableName"
      case SelectionFirst(nullSafeNavigation, expression) =>
        (if (nullSafeNavigation) "?." else "") + "^[" + astToString(expression) + "]"
      case SelectionLast(nullSafeNavigation, expression) =>
        (if (nullSafeNavigation) "?." else "") + "$[" + astToString(expression) + "]"
      case SelectionAll(nullSafeNavigation, expression) =>
        (if (nullSafeNavigation) "?." else "") + "?[" + astToString(expression) + "]"
      case PropertyReference(nullSafeNavigation, propertyName) =>
        (if (nullSafeNavigation) "?." else "") + propertyName
      case Projection(nullSafeNavigation, expression) =>
        (if (nullSafeNavigation) "?." else "") + "![" + astToString(expression) + "]"
      case OpPower(base, expression) =>
        astToString(
          base,
          opPowerShouldParen
        ) + " ** " + astToString(expression, opPowerShouldParen)
      case OpPlus(left, right) =>
        astToString(left, opAddShouldParen) + " + " + astToString(
          right,
          opAddShouldParen
        )
      case OpOr(left, right) =>
        astToString(left, opOrShouldParen) + " || " + astToString(
          right,
          opOrShouldParen
        )
      case OpNot(expression) =>
        "!" + astToString(expression, unaryShouldParen)
      case OpNE(left, right) =>
        astToString(left, opRelationalShouldParen) + " != " + astToString(
          right,
          opRelationalShouldParen
        )
      case OpMultiply(left, right) =>
        astToString(left, opProductShouldParen) + " * " + astToString(
          right,
          opProductShouldParen
        )
      case OpModulus(left, right) =>
        astToString(left, opProductShouldParen) + " % " + astToString(
          right,
          opProductShouldParen
        )
      case OpMinus(left, right) =>
        astToString(left, opMinusShouldParen) + " - " + astToString(
          right,
          opMinusShouldParen
        )
      // Is this a thing?? It was added to our ast, and we have an evaluation for it,
      // but we don't currently look for it while parsing...
      // case OpMatches(left, right)                                    =>

      case OpLT(left, right) =>
        astToString(left, opRelationalShouldParen) + " < " + astToString(
          right,
          opRelationalShouldParen
        )
      case OpLE(left, right) =>
        astToString(left, opRelationalShouldParen) + " <= " + astToString(
          right,
          opRelationalShouldParen
        )
      case OpGT(left, right) =>
        astToString(left, opRelationalShouldParen) + " > " + astToString(
          right,
          opRelationalShouldParen
        )
      case OpGE(left, right) =>
        astToString(left, opRelationalShouldParen) + " >= " + astToString(
          right,
          opRelationalShouldParen
        )
      case OpEQ(left, right) =>
        astToString(left, opRelationalShouldParen) + " == " + astToString(
          right,
          opRelationalShouldParen
        )
      case OpDivide(left, right) =>
        astToString(left, opDivideShouldParen) + " / " + astToString(
          right,
          opDivideShouldParen
        )
      case OpAnd(left, right) =>
        astToString(left, opAndShouldParen) + " && " + astToString(
          right,
          opAndShouldParen
        )
      case Negative(value) =>
        " -" + astToString(value, unaryShouldParen)
      case NumberLiteral(value) =>
        value.toString()
      case NullLiteral() =>
        "null"
      case MethodReference(nullSafeNavigation, methodName, args) =>
        methodName + "(" + args.map(astToString(_)).mkString(", ") + ")"
      case FunctionReference(nullSafeNavigation, functionName, args) =>
        "#" + functionName + "(" + args.map(astToString(_)).mkString(", ") + ")"
      case InlineMap(elements) =>
        "{" + elements.toList.map(t => (t._1, astToString(t._2))) + "}"
      case InlineList(elements) =>
        "{" + elements.map(astToString(_)) + "}"
      case Indexer(nullSafeNavigation, index) =>
        (if (nullSafeNavigation) "?." else "") + "[" + astToString(index) + "]"
      case Elvis(expression, ifFalse) =>
        astToString(expression, opElvisShouldParen) + "?: " + astToString(
          ifFalse
        )
      case CompoundExpression(expressionComponents) =>
        expressionComponents
          .map(astToString(_))
          .reduce((prev, curr) => {
            if (curr.startsWith("?."))
              prev + curr
            else
              prev + "." + curr

          })
      case BooleanLiteral(value) =>
        if (value) "true" else "false"
    }
    if (shouldParen(ast)) "(" + result + ")" else result
  }
}
