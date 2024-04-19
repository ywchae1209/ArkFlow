package validator.jsonValidator.syntaxObject

import org.json4s.JValue
import validator.jsonValidator.syntaxObject.rule.ObjectRuleAST.BoolOps
import validator.jsonValidator.syntaxObject.rule.ObjectRuleParser
import validator.jsonValidator.{EvalErrors, InvalidSyntaxObjectRules, SyntaxError}
import validator.utils.JsonUtil.JValueWithPower

case class SyntaxObject( syntaxBool: List[(BoolOps, String)]) {

  val routeSep = "\\."    // todo :: may need escape.

  override def toString: String = syntaxBool.map(_._1.toString).mkString(",")

  def checkWith(jv: JValue, asFloat: Boolean = false): Either[EvalErrors, Boolean] = {
    if( asFloat)
      eval(s => { jv.getJValue0(s.split(routeSep)).toDouble()})    // String or Numeric to Double
    else
      eval(s => { jv.getJValue0(s.split(routeSep)).toLong()})      // String or Numeric to Long
  }

  def expressionWith[T]( jv: JValue, asFloat: Boolean = false)
  : List[String] = {

    if( asFloat) {
      val f = (s:String) => jv.getJValue0(s.split(routeSep)).toDouble()    // String or Numeric to Double
      syntaxBool.map( _._1.expressionWith(f))
    } else {
      val f = (s:String) => jv.getJValue0(s.split(routeSep)).toLong()    // String or Numeric to Double
      syntaxBool.map( _._1.expressionWith(f))
    }
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
  def apply( expr: String*): Either[SyntaxError, SyntaxObject] = {

    val (lefts, rights) =
      expr
        .map( e =>
          ObjectRuleParser.compile(e).map( _ -> e) )
        .partitionMap(identity)

    if(lefts.nonEmpty)
      Left( InvalidSyntaxObjectRules( lefts.toList) )
    else
      Right(SyntaxObject( rights.toList))
  }
}


