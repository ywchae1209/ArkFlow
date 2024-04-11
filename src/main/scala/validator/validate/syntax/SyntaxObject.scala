package validator.validate.syntax

import org.json4s.JValue
import validator.utils.JsonUtil.JValueWithPower
import validator.validate.{EvalErrors, ExprError, ExprErrors}
import validator.validate.syntax.objectRule.ObjectBoolTree.BoolOps
import validator.validate.syntax.objectRule.ObjectRuleParser

case class SyntaxObject( syntaxBool: List[(BoolOps, String)]) {

  override def toString: String = syntaxBool.map(_._1.toString).mkString(",")

  def checkWith(jv: JValue, asFloat: Boolean = false): Either[EvalErrors, Boolean] = {
    if( asFloat)
      eval(s => (jv \ s).toDouble)
    else
      eval(s => (jv \ s).toLong)
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


