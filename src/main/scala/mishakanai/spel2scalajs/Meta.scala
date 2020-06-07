package mishakanai.spel2scalajs
import scala.collection.immutable.Nil
import scala.collection.mutable

object Meta {
  def getMethodsAndFunctions(
    outerAst: ExpressionSymbol,
  ): List[String] = {
    outerAst match {
      case StringLiteral(value) => List()
      case Ternary(expression, ifTrue, ifFalse) => getMethodsAndFunctions(expression) ::: getMethodsAndFunctions(ifTrue) ::: getMethodsAndFunctions(ifFalse)
      case VariableReference(variableName) => List()
      case SelectionFirst(nullSafeNavigation, expression) => getMethodsAndFunctions(expression)
      case SelectionLast(nullSafeNavigation, expression) => getMethodsAndFunctions(expression)
      case SelectionAll(nullSafeNavigation, expression) => getMethodsAndFunctions(expression)
      case PropertyReference(nullSafeNavigation, propertyName) => List()
      case Projection(nullSafeNavigation, expression) => getMethodsAndFunctions(expression)
      case OpPower(base, expression) => getMethodsAndFunctions(expression)
      case OpPlus(left, right) => getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpOr(left, right) =>  getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpNot(expression) => getMethodsAndFunctions(expression)
      case OpNE(left, right) =>  getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpMultiply(left, right) => getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpModulus(left, right) => getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpMinus(left, right) => getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpMatches(left, right) => getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpLT(left, right) => getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpLE(left, right) =>getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpGT(left, right) => getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpGE(left, right) => getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpEQ(left, right) => getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpDivide(left, right) => getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case OpAnd(left, right) => getMethodsAndFunctions(left) ::: getMethodsAndFunctions(right)
      case Negative(value) => getMethodsAndFunctions(value)
      case NumberLiteral(value) => List()
      case NullLiteral() => List()
      case MethodReference(nullSafeNavigation, methodName, args) => methodName :: args.flatMap(getMethodsAndFunctions(_))
      case FunctionReference(nullSafeNavigation, functionName, args) => functionName :: args.flatMap(getMethodsAndFunctions(_))
      case InlineMap(elements) => elements.toList.flatMap(t =>
            t match {
              case (key, ast) => getMethodsAndFunctions(ast)
            }
          )
      case InlineList(elements) => elements.flatMap(getMethodsAndFunctions(_))
      case Indexer(nullSafeNavigation, index) => getMethodsAndFunctions(index)
      case Elvis(expression, ifFalse) => getMethodsAndFunctions(expression) ::: getMethodsAndFunctions(ifFalse)
      case CompoundExpression(expressionComponents) => expressionComponents.flatMap(getMethodsAndFunctions(_))
      case BooleanLiteral(value) => List()
    }
  }
  def getExpansions(
      outerAst: ExpressionSymbol,
      includeAllExpansion: Boolean = false
  ): List[String] = {
    def _getExpands(
        ast: ExpressionSymbol,
        currentPathToThis: Option[String] = None
    ): List[String] = {
      def ge = (_ast: ExpressionSymbol) => _getExpands(_ast, currentPathToThis)
      ast match {
        case StringLiteral(value) => List()
        case Ternary(expression, ifTrue, ifFalse) =>
          ge(expression) ::: ge(
            ifTrue
          ) ::: ge(
            ifFalse
          )
        case VariableReference(variableName) => List()
        case SelectionFirst(nullSafeNavigation, expression) =>
          ge(expression)
        case SelectionLast(nullSafeNavigation, expression) =>
          ge(expression)
        case SelectionAll(nullSafeNavigation, expression) =>
          ge(expression)
        case PropertyReference(nullSafeNavigation, propertyName) =>
          List(propertyName)
        case Projection(nullSafeNavigation, expression) =>
          ge(expression)
        case OpPower(base, expression) =>
          ge(base) ::: ge(
            expression
          )
        case OpPlus(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpOr(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpNot(expression) =>
          ge(expression)
        case OpNE(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpMultiply(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpModulus(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpMinus(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpMatches(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpLT(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpLE(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpGT(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpGE(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpEQ(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpDivide(left, right) =>
          ge(left) ::: ge(
            right
          )
        case OpAnd(left, right) =>
          ge(left) ::: ge(
            right
          )
        case Negative(value) =>
          ge(value)
        case NumberLiteral(value) => List()
        case NullLiteral()        => List()
        case MethodReference(nullSafeNavigation, methodName, args) =>
          args.flatMap(x => ge(x))
        case FunctionReference(nullSafeNavigation, functionName, args) =>
          args.flatMap(x => ge(x))
        case InlineMap(elements) =>
          elements.toList.flatMap(t =>
            t match {
              case (key, ast) => ge(ast)
            }
          )
        case InlineList(elements) =>
          elements.flatMap(e => ge(e))
        case Indexer(nullSafeNavigation, index) =>
          ge(index)
        case Elvis(expression, ifFalse) =>
          ge(expression) ::: ge(ifFalse)
        case CompoundExpression(expressionComponents) => {
          var terminalReached = false;
          // prev is going to be a list of paths, with the rightmost being the 'deepest until this point'
          expressionComponents.foldLeft(List[String]())((prev, curr) => {
            curr match {
              case VariableReference(variableName) =>
                if (variableName == "this" && currentPathToThis.isDefined)
                  List(currentPathToThis.get)
                else {
                  terminalReached = true;
                  List()
                }
              case SelectionFirst(nullSafeNavigation, expression) => {
                if (terminalReached) prev ::: _getExpands(expression, None)
                else {
                  terminalReached = true;
                  prev ::: _getExpands(expression, prev.lastOption)
                }
              }
              case SelectionLast(nullSafeNavigation, expression) => {
                if (terminalReached) prev ::: _getExpands(expression, None)
                else {
                  terminalReached = true;
                  prev ::: _getExpands(expression, prev.lastOption)
                }
              }
              case SelectionAll(nullSafeNavigation, expression) => {
                if (terminalReached) prev ::: _getExpands(expression, None)
                else {
                  terminalReached = true;
                  prev ::: _getExpands(expression, prev.lastOption)
                }
              }
              case PropertyReference(nullSafeNavigation, propertyName) =>
                if (terminalReached) prev
                else
                  (prev.lastOption
                    .map(last => last :: propertyName :: Nil)
                    .getOrElse(List(propertyName)))
                    .mkString(".") :: Nil
              case Projection(nullSafeNavigation, expression) => {
                if (terminalReached) prev ::: _getExpands(expression, None)
                else {
                  terminalReached = true;
                  prev ::: _getExpands(expression, prev.lastOption)
                }
              }
              case Indexer(nullSafeNavigation, expression) => {
                if (terminalReached) prev ::: _getExpands(expression, None)
                else {
                  terminalReached = true;
                  val expansions = _getExpands(expression)
                  if (includeAllExpansion)
                    prev.lastOption.map(last => s"$last._ALL_") match {
                      case Some(last) => last :: expansions
                      case None       => expansions
                    }
                  else prev ::: expansions
                }
              }
              case other => {
                terminalReached = true;
                prev ::: _getExpands(other)
              }
            }
          })
        }
        case BooleanLiteral(value) => List()
      }
    }
    _getExpands(outerAst)
  }
}
