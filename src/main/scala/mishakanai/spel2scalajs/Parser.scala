package mishakanai.spel2scalajs
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.parsing.combinator._

object SpelParser extends JavaTokenParsers {
  /*
    Start: EXPRESSION
   */
  def expression: Parser[ExpressionSymbol] =
    (ternary | elvis | logicalOrExpression)
  def ternary: Parser[ExpressionSymbol] =
    logicalOrExpression ~ "?" ~ expression ~ ":" ~ expression ^^ {
      case condition ~ "?" ~ ifTrue ~ ":" ~ ifFalse =>
        Ternary(condition, ifTrue, ifFalse)
    }
  def elvis: Parser[ExpressionSymbol] =
    logicalOrExpression ~ "?:" ~ expression ^^ {
      case expression ~ "?:" ~ ifFalse => Elvis(expression, ifFalse)
    }
  def logicalOrExpression: Parser[ExpressionSymbol] =
    logicalAndExpression ~ rep(
      "||" ~ logicalAndExpression | "OR" ~ logicalOrExpression
    ) ^^ {
      case op ~ list =>
        list.foldLeft(op) {
          case (x, "||" ~ y) => OpOr(x, y)
          case (x, "OR" ~ y) => OpOr(x, y)
        }
    }
  /*
    END: EXPRESSION
   */
  def logicalAndExpression: Parser[ExpressionSymbol] =
    relationalExpression ~ rep(
      "&&" ~ relationalExpression | "AND" ~ relationalExpression
    ) ^^ {
      case op ~ list =>
        list.foldLeft(op) {
          case (x, "&&" ~ y)  => OpAnd(x, y)
          case (x, "AND" ~ y) => OpAnd(x, y)
        }
    }
  // -----------------------------------------------------------------
  /*
    START: RELATIONAL EXPRESSION
   */
  def relationalExpression: Parser[ExpressionSymbol] =
    sumExpression ~ opt("<=|>=|==|>|<".r ~ sumExpression) ^^ {
      case left ~ Some(operator ~ right) => {
        operator match {
          case ">"  => OpGT(left, right)
          case "<"  => OpLT(left, right)
          case "<=" => OpLE(left, right)
          case ">=" => OpGE(left, right)
          case "==" => OpEQ(left, right)
        }
      }
      case left ~ None => left
    }
  /*
    END: RELATIONAL EXPRESSION
   */
  // -----------------------------------------------------------------
  def sumExpression: Parser[ExpressionSymbol] =
    productExpression ~ rep("+" ~ productExpression | "-" ~ productExpression) ^^ {
      case op ~ list =>
        list.foldLeft(op) {
          case (x, "+" ~ y) => OpPlus(x, y)
          case (x, "-" ~ y) => OpMinus(x, y)
        }
    }

  def productExpression: Parser[ExpressionSymbol] =
    powerExpression ~ rep(
      "*" ~ powerExpression | "/" ~ powerExpression | "%" ~ powerExpression
    ) ^^ {
      case op ~ list =>
        list.foldLeft(op) {
          case (x, "*" ~ y) => OpMultiply(x, y)
          case (x, "/" ~ y) => OpDivide(x, y)
          case (x, "%" ~ y) => OpModulus(x, y)
        }
    }
  def powerExpression: Parser[ExpressionSymbol] =
    unaryExpression ~ opt("**" ~ unaryExpression) ^^ {
      case op ~ list =>
        list.foldLeft(op) {
          case (x, "**" ~ y) => OpPower(x, y)
        }
    }
  def unaryExpression: Parser[ExpressionSymbol] =
    negative | not | primaryExpression
  def negative: Parser[ExpressionSymbol] = ("-" ~ unaryExpression) ^^ {
    case symbol ~ exp => Negative(exp)
  }
  def not: Parser[ExpressionSymbol] = ("!" ~ unaryExpression) ^^ {
    case symbol ~ exp => OpNot(exp)
  }

  def primaryExpression: Parser[ExpressionSymbol] = startNode ~ rep(node) ^^ {
    // value: ExpressionSymbol ~ List[ExpressionSymbol]
    case sn ~ list =>
      if (list.length > 0) CompoundExpression(sn :: list) else sn
  }

  def startNode: Parser[ExpressionSymbol] =
    // I don't get why 'indexer' counts as a possible start node...
    // TODO add inline list/map
    parenExpr | literal | notNullSafeMethodOrProperty | functionOrVar // | projection | selection //  | indexer // | constructor

  //----------------------------------------------------------------------------

  /* START: NODE */

  def node: Parser[ExpressionSymbol] =
    navProperty | index | functionOrVar | projection | selection

  def navProperty =
    ("." ~> notNullSafeMethodOrProperty) | ("?." ~> nullSafeMethodOrProperty)
  def index = nullSafeIndex | notNullSafeIndex
  def nullSafeIndex = "?[" ~> expression <~ "]" ^^ { exp => Indexer(true, exp) }
  def notNullSafeIndex = "[" ~> expression <~ "]" ^^ { exp =>
    Indexer(false, exp)
  }

  def nullSafeMethodOrProperty =
    ident ~ opt("(" ~> repsep(expression, ",") <~ ")") ^^ {
      // ExpressionSymbol ~ Option[List[ExpressionSymbol]]
      case id ~ Some(list) => {
        FunctionReference(true, id, list)
      }
      case id ~ None => {
        PropertyReference(true, id)
      }
    }
  def notNullSafeMethodOrProperty: Parser[ExpressionSymbol] =
    ident ~ opt("(" ~> repsep(expression, ",") <~ ")") ^^ {
      // ExpressionSymbol ~ Option[List[ExpressionSymbol]]
      case id ~ Some(list) => {
        FunctionReference(false, id, list)
      }
      case id ~ None => {
        PropertyReference(false, id)
      }
    }
  /*
    END: NODE
   */

  //----------------------------------------------------------------------------

  /*
  START: FUNCTION OR VAR
   */

  def functionOrVar: Parser[ExpressionSymbol] = function | variable
  def function: Parser[ExpressionSymbol] =
    "#" ~ ident ~ "(" ~ repsep(expression, ",") ~ ")" ^^ {
      case "#" ~ id ~ "(" ~ list ~ ")" => FunctionReference(false, id, list)
    }
  def variable: Parser[ExpressionSymbol] = "#" ~> ident ^^ { id =>
    VariableReference(id)
  }
  /*
  END: FUNCTION OR VAR
   */

  //----------------------------------------------------------------------------
  /*
  START: SELECTION
   */
  def selection: Parser[ExpressionSymbol] =
    nullSafeSelection | notNullSafeSelection
  def notNullSafeSelection: Parser[ExpressionSymbol] =
    "." ~> (notNullSafeSelectionAll | notNullSafeSelectionFirst | notNullSafeSelectionLast)
  def nullSafeSelection: Parser[ExpressionSymbol] =
    "?." ~> (nullSafeSelectionAll | nullSafeSelectionFirst | nullSafeSelectionLast)
  def nullSafeSelectionAll: Parser[ExpressionSymbol] =
    "?[" ~> expression <~ "]" ^^ { expr => SelectionAll(true, expr) }
  def notNullSafeSelectionAll: Parser[ExpressionSymbol] =
    "?[" ~> expression <~ "]" ^^ { expr => SelectionAll(false, expr) }
  def nullSafeSelectionFirst: Parser[ExpressionSymbol] =
    "^[" ~> expression <~ "]" ^^ { expr => SelectionAll(true, expr) }
  def notNullSafeSelectionFirst: Parser[ExpressionSymbol] =
    "^[" ~> expression <~ "]" ^^ { expr => SelectionAll(false, expr) }
  def nullSafeSelectionLast: Parser[ExpressionSymbol] =
    "$[" ~> expression <~ "]" ^^ { expr => SelectionAll(true, expr) }
  def notNullSafeSelectionLast: Parser[ExpressionSymbol] =
    "$[" ~> expression <~ "]" ^^ { expr => SelectionAll(false, expr) }
  /*
  END: SELECTION
   */
  //----------------------------------------------------------------------------
  /*
  START: PROJECTION
   */
  def projection: Parser[ExpressionSymbol] =
    nullSafeProjection | notNullSafeProjection
  def nullSafeProjection: Parser[ExpressionSymbol] =
    "?.![" ~> expression <~ "]" ^^ { expr => Projection(true, expr) }
  def notNullSafeProjection: Parser[ExpressionSymbol] =
    ".![" ~> expression <~ "]" ^^ { expr => Projection(false, expr) }

  /*
  END: PROJECTION
   */
  //----------------------------------------------------------------------------
  /*
    START: Literal
   */
  def literal: Parser[ExpressionSymbol] =
    string | integer | float | boolean | nullLiteral
  def string = stringLiteral ^^ { str => StringLiteral(str) };
  // def literal
  def integer = wholeNumber ^^ { n => NumberLiteral(n.toFloat) }
  def float = floatingPointNumber ^^ { n => NumberLiteral(n.toFloat) }
  def boolean: Parser[BooleanLiteral] = ("true" | "false") ^^ {
    case "true"  => BooleanLiteral(true)
    case "false" => BooleanLiteral(false)
  }
  def nullLiteral = "null" ^^ { _ => NullLiteral() }

  /*
    END: Literal
   */

  //----------------------------------------------------------------------------
  /*
    START: INLINE LIST OR MAP
   */
  // TODO
  /*
    END: INLINE LIST OR MAP
   */
  //----------------------------------------------------------------------------

  def parenExpr: Parser[ExpressionSymbol] = "(" ~ expression ~ ")" ^^ {
    case "(" ~ expr ~ ")" => expr
  }

  //----------------------------------------------------------------------------
  // def constructor: Parser[ExpressionSymbol] = string

  def apply(input: String): Option[ExpressionSymbol] =
    parseAll(expression, input) match {
      case Success(result, _) => Some(result)
      case Error(msg, next) => {
        println(s"ERROR: $msg")
        None
      }
      case Failure(msg, next) => {
        println(s"FAILURE: $msg")
        None
      }
    }
}
