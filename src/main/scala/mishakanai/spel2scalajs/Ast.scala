package mishakanai.spel2scalajs

sealed abstract class ExpressionSymbol
case class StringLiteral(value: String) extends ExpressionSymbol
case class Ternary(
    expression: ExpressionSymbol,
    ifTrue: ExpressionSymbol,
    ifFalse: ExpressionSymbol
) extends ExpressionSymbol
case class VariableReference(variableName: String) extends ExpressionSymbol
case class SelectionFirst(
    nullSafeNavigation: Boolean,
    expression: ExpressionSymbol
) extends ExpressionSymbol
case class SelectionLast(
    nullSafeNavigation: Boolean,
    expression: ExpressionSymbol
) extends ExpressionSymbol
case class SelectionAll(
    nullSafeNavigation: Boolean,
    expression: ExpressionSymbol
) extends ExpressionSymbol
case class PropertyReference(nullSafeNavigation: Boolean, propertyName: String)
    extends ExpressionSymbol
case class Projection(
    nullSafeNavigation: Boolean,
    expression: ExpressionSymbol
) extends ExpressionSymbol

case class OpPower(base: ExpressionSymbol, expression: ExpressionSymbol)
    extends ExpressionSymbol
case class OpPlus(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpOr(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpNot(expression: ExpressionSymbol) extends ExpressionSymbol
case class OpNE(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpMultiply(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpModulus(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpMinus(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpMatches(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpLT(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpLE(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpGT(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpGE(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpEQ(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
case class OpDivide(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol
// case class OpGT(left: ExpressionSymbol, right: ExpressionSymbol)
//     extends ExpressionSymbol
case class OpAnd(left: ExpressionSymbol, right: ExpressionSymbol)
    extends ExpressionSymbol

case class Negative(value: ExpressionSymbol) extends ExpressionSymbol
case class NumberLiteral(value: Float) extends ExpressionSymbol
case class NullLiteral() extends ExpressionSymbol

case class MethodReference(
    nullSafeNavigation: Boolean,
    methodName: String,
    args: List[ExpressionSymbol]
) extends ExpressionSymbol
// See Code for description of FunctionReference
case class FunctionReference(
    nullSafeNavigation: Boolean,
    functionName: String,
    args: List[ExpressionSymbol]
) extends ExpressionSymbol
case class InlineMap(elements: Map[String, ExpressionSymbol])
    extends ExpressionSymbol // not sure if elements should be property-refernces...
case class InlineList(elements: List[ExpressionSymbol]) extends ExpressionSymbol
case class Indexer(nullSafeNavigation: Boolean, index: ExpressionSymbol)
    extends ExpressionSymbol
case class Elvis(expression: ExpressionSymbol, ifFalse: ExpressionSymbol)
    extends ExpressionSymbol

// Represents a DOT separated expression sequence, such as 'property1.property2.methodOne()'
case class CompoundExpression(expressionComponents: List[ExpressionSymbol])
    extends ExpressionSymbol // TODO: refine type argument
case class BooleanLiteral(value: Boolean) extends ExpressionSymbol

/*
    MAYBE ADD CONSTRUCTORS
 */
