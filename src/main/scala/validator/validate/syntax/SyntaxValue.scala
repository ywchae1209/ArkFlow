package validator.validate.syntax

import org.json4s.JValue
import validator.validate.syntax.valueRule.PredicateMaker.UserFunctionTable
import validator.validate.syntax.valueRule.ValueBoolTree.FunctionExpr
import validator.validate.syntax.valueRule.ValueRuleParser
import validator.validate.{EvaluationError, ExprError}

case class SyntaxValue(expr: FunctionExpr) {

  override def toString: String = expr.toString

  val evaluate: JValue => Either[EvaluationError, Boolean]
  = jv => expr.evaluateWith(UserFunctionTable)(jv)
}

object SyntaxValue {

  def apply(expr: String): Either[ExprError, SyntaxValue] = {
      ValueRuleParser.parse(expr)
        .left.map( ExprError(_))
        .map( SyntaxValue(_))
  }
}



