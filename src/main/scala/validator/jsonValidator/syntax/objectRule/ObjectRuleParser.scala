package validator.jsonValidator.syntax.objectRule

import validator.utils.StringUtil.parseWith

import fastparse.NoWhitespace._
import fastparse._

import ObjectNumericTree._
import ObjectBoolTree._

object ObjectRuleParser {

  def parse(s: String): Either[String, BoolOps] = parseWith(numericBoolExpression(_))(s)

  def parseNumeric(s: String): Either[String, NumericOps] = parseWith(numericExpression(_))(s)

  ////////////////////////////////////////////////////////////////////////////////
  // NumExpr Parser
  ////////////////////////////////////////////////////////////////////////////////

  private def numericExpression[$: P]: P[NumericOps] = P(append ~ sp ~ End)
  private def numericBoolExpression[$: P]: P[BoolOps] = P(or ~ sp ~ End)

  private def sp[$: P] = P(CharsWhileIn(" \r\n\t").rep(max = 80))
  private def `(`[$: P] = P(sp ~ "(" ~ sp)
  private def `)`[$: P] = P(sp ~ ")" ~ sp)
  private def cl[$: P] = P(CharPred(c => c != '}'))
  private def el[$: P] = P("\\}") // note:: if want to use "}" in key name, escape by "\\"

  ////////////////////////////////////////////////////////////////////////////////
  private def number[$: P]: P[Number] = P(sp ~ CharIn("0-9").rep(1).! ~ sp).map(s => Number(s.toInt))

  private def term[$: P]: P[Term] = P(sp ~ "${" ~ (el.! | cl.!).rep(1) ~ "}" ~ sp).map { s =>
    val neo = s.map(c => if (c == "\\}") "}" else c).mkString
    Term(neo)
  }

  private def factor[$: P]: P[NumericExpression] = P(number | term | parens)

  private def parens[$: P]: P[NumericOps] = P(`(` ~/ append ~ `)`)

  private def times[$: P]: P[NumericOps] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(s => toNumOps(s))

  private def append[$: P]: P[NumericOps] = P(times ~ (CharIn("+\\-").! ~/ times).rep).map(s => toNumOps(s))

  private def toNumOps(tree: (NumericExpression, Seq[(String, NumericExpression)])): NumericOps = {
    val (base, ops) = tree
    val op = ops.map {
      case ("+", m) => (Plus, m)
      case ("-", m) => (Minus, m)
      case ("*", m) => (Multiply, m)
      case ("/", m) => (Divide, m)
    }.toList

    NumericOps(base, op)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Boolean & Compare Rule Parser
  ////////////////////////////////////////////////////////////////////////////////

  private def cmp[$: P]: P[BoolExpr] = P(append ~ StringIn("<", "<=", ">", ">=", "==", "!=").! ~ append).map {
    case (lhs, "<", rhs) => NumComp(lhs, rhs, Lt)
    case (lhs, "<=", rhs) => NumComp(lhs, rhs, Lte)
    case (lhs, ">", rhs) => NumComp(lhs, rhs, Gt)
    case (lhs, ">=", rhs) => NumComp(lhs, rhs, Gte)
    case (lhs, "==", rhs) => NumComp(lhs, rhs, Eq)
    case (lhs, "!=", rhs) => NumComp(lhs, rhs, NEq)
  }

  private def factorBool[$: P]: P[BoolExpr] = P(paransBool | cmp)

  private def paransBool[$: P]: P[BoolOps] = P(`(` ~/ or ~ `)`)

  private def and[$: P]: P[BoolOps] = P(factorBool ~ ("&&".! ~ factorBool).rep).map(s => toBoolOps(s))

  private def or[$: P]: P[BoolOps] = P(and ~ sp ~ ("||".! ~ and).rep).map(s => toBoolOps(s))

  private def toBoolOps(tree: (BoolExpr, Seq[(String, BoolExpr)])): BoolOps = {

    val (base, ops) = tree
    val op = ops.map {
      case ("&&", m) => (And, m)
      case ("||", m) => (Or, m)
    }.toList

    BoolOps(base, op)
  }
}

