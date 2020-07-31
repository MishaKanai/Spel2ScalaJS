package mishakanai.spel2scalajs
import fastparse._

object Quote {
  def _quote(from: Char): (String) => String = {
    def quote(s: String) =
      s.map {
          case '"'  => if (from == '"') "\\\"" else '"'
          case '\'' => if (from == '\'') "\\\'" else '\''
          case '\\' => "\\\\"
          case '/'  => "\\/"
          case '\b' => "\\b"
          case '\f' => "\\f"
          case '\n' => "\\n"
          case '\r' => "\\r"
          case '\t' => "\\t"
          case c
              if ((c >= '\u0000' && c <= '\u001f') || (c >= '\u007f' && c <= '\u009f')) =>
            "\\u%04x".format(c: Int)
          case c => c
        }
        .mkString(s"$from", "", s"$from")
    quote(_)
  }
  def _unquote(from: Char) = {
    def unquote(s: String): String =
      if (s.isEmpty) s
      else
        s(0) match {
          case '"' =>
            if (from == '"') unquote(s.tail) else '"' + unquote(s.tail)
          case '\'' =>
            if (from == '\'') unquote(s.tail) else '\'' + unquote(s.tail)
          case '\\' =>
            s(1) match {
              case 'b' => '\b' + unquote(s.drop(2))
              case 'f' => '\f' + unquote(s.drop(2))
              case 'n' => '\n' + unquote(s.drop(2))
              case 'r' => '\r' + unquote(s.drop(2))
              case 't' => '\t' + unquote(s.drop(2))
              case '"' => '"' + unquote(s.drop(2))
              case 'u' =>
                Integer.parseInt(s.drop(2).take(4), 16).toChar + unquote(
                  s.drop(6)
                )
              case c => c + unquote(s.drop(2))
            }
          case c => c + unquote(s.tail)
        }
    unquote(_)
  }

}

object LiteralsParser {
  import fastparse._, NoWhitespace._
  def space[_: P] = P(CharsWhileIn(" \r\n", 0))
  def digits[_: P] = P(CharsWhileIn("0-9"))
  def exponent[_: P] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional[_: P] = P("." ~ digits)
  def integral[_: P] = P("0" | CharIn("1-9") ~ digits.?)

  def number[_: P] =
    P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(x =>
      NumberLiteral(x.toFloat)
    )

  def `null`[_: P] = P("null").map(_ => NullLiteral())
  def `false`[_: P] = P("false").map(_ => BooleanLiteral(false))
  def `true`[_: P] = P("true").map(_ => BooleanLiteral(true))

  def escapeseq[_: P]: P[Unit] = P("\\" ~ AnyChar)
  def string[_: P]: P[ExpressionSymbol] =
    P(shortstring0("'") | shortstring0("\""))
      .map(StringLiteral)
  def shortstring0[_: P](delimiter: String) =
    P(delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
      .map(Quote._unquote(delimiter(0)))
  def shortstringitem[_: P](quote: String): P[Unit] =
    P(shortstringchar(quote) | escapeseq)
  def shortstringchar[_: P](quote: String): P[Unit] =
    P(CharsWhile(!s"\\\n${quote(0)}".contains(_)))

  def letter[_: P] = P(lowercase | uppercase)
  def lowercase[_: P] = P(CharIn("a-z"))
  def uppercase[_: P] = P(CharIn("A-Z"))

  def digit[_: P] = P(CharIn("0-9"))

  def identifier[_: P] = P((letter | "_") ~ (letter | digit | "_").rep)

}

object ExpressionParser {
  import fastparse._, JavaWhitespace._

  def expression[_: P]: P[ExpressionSymbol] =
    P(
      LiteralsParser.space.? ~ logicalOrExpresion ~ (("?" ~ expression ~ ":" ~ expression) | ("?:" ~ expression)).? ~ LiteralsParser.space.?
    ).map(x => {
      x match {
        case (condition, maybeRest) => {
          maybeRest match {
            case Some(r) =>
              r match {
                case (ifTrue: ExpressionSymbol, ifFalse: ExpressionSymbol) =>
                  Ternary(
                    condition,
                    ifTrue,
                    ifFalse
                  )
                case (ifFalse: ExpressionSymbol) =>
                  Elvis(condition, ifFalse)
              }
            case None => condition
          }
        }
      }
    })
  def logicalOrExpresion[_: P]: P[ExpressionSymbol] =
    P(
      LiteralsParser.space.? ~
        logicalAndExpression ~
        ("||" ~ logicalAndExpression).rep.? ~ LiteralsParser.space.?
    ).map(x => {
      x match {
        case (op, list) =>
          list match {
            case Some(value) =>
              value.foldLeft(op) {
                case (x, y) => OpOr(x, y)
              }
            case None => op
          }

      }
    })

  def logicalAndExpression[_: P]: P[ExpressionSymbol] =
    P(
      relationalExpression ~ ("&&" ~ relationalExpression).rep.?
    ).map(x => {
      x match {
        case (op, maybeList) =>
          maybeList match {
            case Some(value) =>
              value.foldLeft(op) {
                case (x, y) => OpAnd(x, y)
              }
            case None => op
          }
      }
    })

  def relationalExpression[_: P]: P[ExpressionSymbol] =
    P(
      sumExpression ~
        (StringIn("<=", ">=", "==", "!=", "<", ">").! ~ sumExpression).?
    ).map(x =>
      x match {
        case (left, Some((operator, right))) =>
          operator match {
            case ">"  => OpGT(left, right)
            case "<"  => OpLT(left, right)
            case "<=" => OpLE(left, right)
            case ">=" => OpGE(left, right)
            case "==" => OpEQ(left, right)
            case "!=" => OpNE(left, right)
          }
        case (left, None) => left
      }
    )

  def sumExpression[_: P]: P[ExpressionSymbol] =
    P(
      productExpression ~ (CharIn("+\\-").! ~ productExpression).rep.?
    ).map(x => {
      x match {
        case (op, maybeList) => {
          maybeList match {
            case Some(list) =>
              list.foldLeft(op) {
                case (x, ("+", y)) => OpPlus(x, y)
                case (x, ("-", y)) => OpMinus(x, y)
              }
            case None => op
          }
        }
      }
    })

  def productExpression[_: P]: P[ExpressionSymbol] =
    P(
      powerExpression ~ (
        (CharIn("*/%").! ~ powerExpression)
      ).rep.?
    ).map(x =>
      x match {
        case (op, maybeList) =>
          maybeList match {
            case Some(list) =>
              list.foldLeft(op) {
                case (x, ("*", y)) => OpMultiply(x, y)
                case (x, ("/", y)) => OpDivide(x, y)
                case (x, ("%", y)) => OpModulus(x, y)
              }
            case None => op
          }

      }
    )

  def powerExpression[_: P]: P[ExpressionSymbol] =
    P(
      unaryExpression ~ ("**" ~ unaryExpression).?
    ).map(x =>
      x match {
        case (left, Some(right)) => OpPower(left, right)
        case (left, None)        => left
      }
    )

  def unaryExpression[_: P]: P[ExpressionSymbol] =
    P(negative | not | primaryExpression)

  def negative[_: P]: P[ExpressionSymbol] =
    P(
      "-" ~/ unaryExpression
    ).map(Negative)

  def not[_: P]: P[ExpressionSymbol] = P("!" ~/ unaryExpression).map(OpNot)

  def primaryExpression[_: P]: P[ExpressionSymbol] =
    P(startNode ~ node.rep)
      .map(x =>
        x match {
          case (sn, continuation) => {
            if (continuation.length > 0)
              CompoundExpression(sn :: continuation.toList)
            else sn
          }
        }
      )

  def startNode[_: P]: P[ExpressionSymbol] =
    P(
      parenExpr | literal | functionOrVar | notNullSafeMethodOrProperty | inlineList | inlineMap
    )
  def node[_: P]: P[ExpressionSymbol] =
    P(projection | selection | navProperty | index | functionOrVar)
  def navProperty[_: P]: P[ExpressionSymbol] =
    P(
      ("." ~ notNullSafeMethodOrProperty) | ("?." ~ nullSafeMethodOrProperty)
    )

  def index[_: P]: P[ExpressionSymbol] = P(nullSafeIndex | notNullSafeIndex)

  def nullSafeIndex[_: P]: P[ExpressionSymbol] =
    P(
      "?[" ~ expression ~ "]"
    ).map(exp => Indexer(true, exp))

  def notNullSafeIndex[_: P]: P[ExpressionSymbol] =
    P(
      "[" ~ expression ~ "]"
    ).map(exp => Indexer(false, exp))

  def nullSafeMethodOrProperty[_: P]: P[ExpressionSymbol] =
    P(
      ident.! ~ ("(" ~ expression.rep(sep = ",") ~ ")").?
    ).map(x =>
      x match {
        case (id, Some(list)) => MethodReference(true, id, list.toList)
        case (id, None)       => PropertyReference(true, id)
      }
    )

  def notNullSafeMethodOrProperty[_: P]: P[ExpressionSymbol] =
    P(
      ident.! ~ ("(" ~ expression.rep(sep = ",") ~ ")").?
    ).map(x =>
      x match {
        case (id, Some(list)) => MethodReference(false, id, list.toList)
        case (id, None)       => PropertyReference(false, id)
      }
    )

// function or var
  def functionOrVar[_: P]: P[ExpressionSymbol] = P(function | variable)
  def function[_: P]: P[ExpressionSymbol] =
    P(
      "#" ~ ident.! ~ "(" ~ expression.rep(sep = ",") ~ ")"
    ).map(x =>
      x match {
        case (id, list) => FunctionReference(false, id, list.toList)
      }
    )

  def variable[_: P]: P[ExpressionSymbol] =
    P("#" ~ ident.!).map(VariableReference)

  def selection[_: P]: P[ExpressionSymbol] =
    P(
      StringIn("?.", ".").! ~ StringIn("?[", "^[", "$[").! ~ expression ~ "]"
    ).map(x => {
      x match {
        case ("?.", opener, expr) =>
          opener match {
            case "?[" => SelectionAll(true, expr)
            case "^[" => SelectionFirst(true, expr)
            case "$[" => SelectionLast(true, expr)
          }
        case (".", opener, expr) =>
          opener match {
            case "?[" => SelectionAll(false, expr)
            case "^[" => SelectionFirst(false, expr)
            case "$[" => SelectionLast(false, expr)
          }
      }
    })

  def projection[_: P]: P[ExpressionSymbol] =
    P(
      StringIn("?.", ".").! ~ StringIn("![") ~ expression ~ "]"
    ).map(x => {
      x match {
        case ("?.", expr) =>
          Projection(true, expr)
        case (".", expr) =>
          Projection(false, expr)
      }
    })

  // literals
  def literal[_: P]: P[ExpressionSymbol] =
    P(
      (LiteralsParser.string | LiteralsParser.number | LiteralsParser.`true` | LiteralsParser.`false` | LiteralsParser.`null`)
    )
  def parenExpr[_: P]: P[ExpressionSymbol] =
    P(
      "(" ~/ expression ~ ")"
    )

  def inlineList[_: P]: P[ExpressionSymbol] =
    P(
      "{" ~ expression
        .rep(sep = ",") ~ "}"
    ).map(x => InlineList(x.toList))

  def inlineMap[_: P]: P[ExpressionSymbol] =
    P(
      "{" ~ (ident.! ~ ":" ~ expression)
        .rep(sep = ",") ~ "}"
    ).map(list => InlineMap(list.toMap))

  def ident[_: P] = LiteralsParser.identifier

  def parse[_: P] = P(Start ~ expression ~ End)

}
