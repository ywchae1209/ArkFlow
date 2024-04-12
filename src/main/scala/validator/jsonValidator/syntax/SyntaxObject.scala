package validator.jsonValidator.syntax

import org.json4s.JValue
import validator.utils.JsonUtil.JValueWithPower
import validator.jsonValidator.{EvalErrors, ExprError, ExprErrors}
import validator.jsonValidator.syntax.objectRule.ObjectBoolTree.BoolOps
import validator.jsonValidator.syntax.objectRule.ObjectRuleParser

case class SyntaxObject( syntaxBool: List[(BoolOps, String)]) {

  override def toString: String = syntaxBool.map(_._1.toString).mkString(",")

  def checkWith(jv: JValue, asFloat: Boolean = false): Either[EvalErrors, Boolean] = {

    val routeSep = "\\."    // todo :: may need other sep.
    if( asFloat)
      eval(s => {
        jv.getJValue0(s.split(routeSep)).toDouble})    // String or Numeric to Double
    else
      eval(s => {
        jv.getJValue0(s.split(routeSep)).toLong        // String or Numeric to Long
      })
  }

  private def eval[T: Numeric](f: String => Either[String, T]) = {
    val (lefts, rights) = syntaxBool.map(_._1.evaluateWith(f)).partitionMap(identity)

    if(lefts.isEmpty)
      Right( rights.forall(identity) )
    else {
      Left(EvalErrors(lefts))
    }

  }
}

object SyntaxObject{
  def apply( expr: String*): Either[ExprErrors, SyntaxObject] = {

    val (lefts, rights) =
      expr.map( e =>
        ObjectRuleParser.parse(e).fold(
          s => Left(ExprError(s"$e -> $s")),
          o => Right(o -> e) )
      ).partitionMap(identity)


    if(lefts.nonEmpty)
      Left(ExprErrors(lefts.toList))
    else
      Right(SyntaxObject( rights.toList))
  }
}


