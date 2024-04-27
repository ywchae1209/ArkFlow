package transform.jsonValidator.syntaxValue

import org.json4s.JValue
import transform.jsonValidator.syntaxValue.rule.PredicateMaker.UserFunctionTable
import transform.jsonValidator.syntaxValue.rule.ValueRuleAst.ValueCondition
import transform.jsonValidator.syntaxValue.rule.ValueRuleParser
import transform.jsonValidator.{EvaluationError, SyntaxError}

case class SyntaxValue(expr: ValueCondition) {

  override def toString: String = expr.toString

  val evaluate: JValue => Either[EvaluationError, Boolean]
  = jv => expr.evaluateWith(UserFunctionTable)(jv)
}

object SyntaxValue {

  def apply(expr: String): Either[SyntaxError, SyntaxValue] = {
      ValueRuleParser.compile(expr)
        .flatMap{ f =>
          f.checkSyntaxWith(UserFunctionTable)  // syntax-error check.
            .map(e => Left(e))
            .getOrElse(Right( SyntaxValue(f)))

        }
  }
}



