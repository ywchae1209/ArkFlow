package validator.jsonValidator.syntaxValue.rule

import fastparse.NoWhitespace._
import fastparse._
import validator.jsonValidator.InvalidSyntaxValueRule
import validator.jsonValidator.syntaxValue.rule.ValueRuleAst._
import validator.utils.StringUtil.parseWith

/**
 * SyntaxValue Parser
 */
object ValueRuleParser {

  def parse(s: String): Either[InvalidSyntaxValueRule, VPredicates]
  = parseWith(syntaxValueRule(_))(s).left.map( InvalidSyntaxValueRule)

  ////////////////////////////////////////////////////////////////////////////////
  private def syntaxValueRule[$: P]: P[VPredicates] = P(sp ~ or ~ End)

  private def sp[$: P] = P(CharsWhileIn(" \r\n\t").rep(max = 80))
  private def `(`[$: P] = P(sp ~ "(" ~ sp)
  private def `)`[$: P] = P(sp ~ ")" ~ sp)
  private def `,`[$: P] = P(sp ~ "," ~ sp)
  private def `||`[$: P] = P(sp ~ "||" ~ sp)
  private def `&&`[$: P] = P(sp ~ "&&" ~ sp)
  private def lower[$: P] = P(CharIn("a-z"))
  private def upper[$: P] = P(CharIn("A-Z"))
  private def digit[$: P] = P(CharIn("0-9"))
  private def underscore[$: P] = P("_")

  private def fn[$: P] = P((lower | upper | digit | underscore).rep(1).!)
  private def nq[$: P] = P(CharPred(_ != '\'')) // not qute
  private def eq[$: P] = P("\\'") // escaped quote

  private def quoted[$: P]: P[String] = P("'" ~ (eq.! | nq.!).rep(1) ~ "'").map { s =>
    val neo = s.map { c => if ("\\'" == c) "'" else c }.mkString
    neo
  }

  private def normal[$: P]: P[String] = P(CharPred(c => !" \r\n\t',()".contains(c)).rep(1).!)
  private def arg[$: P] = P(quoted | normal)
  private def args[$: P] = P(sp ~ arg.rep(0, `,`, 12))      // argument-count-limit : 12
  private def call0[$: P] = P(sp ~ fn ~ sp).map { f => VPredicate(f, Nil) }
  private def call[$: P] = P(sp ~ fn ~ `(` ~/ args ~ `)`).map { case (f, ps) => VPredicate(f, ps) }
  private def factor[$: P]: P[ValueCondition] = P(call | call0 | parans)
  private def parans[$: P]: P[VPredicates] = P(`(` ~/ or ~ `)`)
  private def and[$: P]: P[VPredicates] = P(factor.rep(1, `&&`)).map(s => toFunctionOps(s, And))
  private def or[$: P]: P[VPredicates] = P(and.rep(1, `||`)).map(s => toFunctionOps(s, Or))

  private def toFunctionOps(s: Seq[ValueCondition], op: FunctionOperator) =
    VPredicates(s.head, s.tail.map(op -> _).toList)
}

