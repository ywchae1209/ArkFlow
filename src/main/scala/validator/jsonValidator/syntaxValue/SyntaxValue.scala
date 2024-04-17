package validator.jsonValidator.syntaxValue

import org.json4s.JValue
import validator.jsonValidator.syntaxValue.rule.PredicateMaker.UserFunctionTable
import validator.jsonValidator.syntaxValue.rule.ValueRuleAst.ValueCondition
import validator.jsonValidator.syntaxValue.rule.ValueRuleParser
import validator.jsonValidator.{EvaluationError, SyntaxError}

case class SyntaxValue(expr: ValueCondition) {

  override def toString: String = expr.toString

  val evaluate: JValue => Either[EvaluationError, Boolean]
  = jv => expr.evaluateWith(UserFunctionTable)(jv)
}

object SyntaxValue {

  def apply(expr: String): Either[SyntaxError, SyntaxValue] = {
      ValueRuleParser.parse(expr)
        .flatMap{ f =>
          f.checkSyntaxWith(UserFunctionTable)  // syntax-error check.
            .map(e => Left(e))
            .getOrElse(Right( SyntaxValue(f)))

        }
  }
}



