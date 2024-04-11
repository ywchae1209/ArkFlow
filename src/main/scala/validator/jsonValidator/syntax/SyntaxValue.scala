package validator.jsonValidator.syntax

import org.json4s.JValue
import validator.jsonValidator.syntax.valueRule.PredicateMaker.UserFunctionTable
import validator.jsonValidator.syntax.valueRule.ValueBoolTree.FunctionExpr
import validator.jsonValidator.syntax.valueRule.ValueRuleParser
import validator.jsonValidator.{EvaluationError, ExprError, SyntaxError}

case class SyntaxValue(expr: FunctionExpr) {

  override def toString: String = expr.toString

  val evaluate: JValue => Either[EvaluationError, Boolean]
  = jv => expr.evaluateWith(UserFunctionTable)(jv)
}

object SyntaxValue {

  def apply(expr: String): Either[SyntaxError, SyntaxValue] = {
      ValueRuleParser.parse(expr)
        .left.map( ExprError(_))
        .flatMap{ f =>
          f.checkSyntaxWith(UserFunctionTable)
            .map(e => Left(e))
            .getOrElse(Right( SyntaxValue(f)))

        }
  }
}



