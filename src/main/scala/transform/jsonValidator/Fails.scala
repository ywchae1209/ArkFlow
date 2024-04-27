package transform.jsonValidator

import org.json4s.JsonAST.JObject
import org.json4s.{JArray, JString, JValue}
import transform.traits.ToJson
import transform.utils.StringUtil

////////////////////////////////////////////////////////////////////////////////
sealed trait EvaluationError extends ToJson

case class EvalErrors( es: List[EvaluationError]) extends EvaluationError{
  override def toJson: JValue = JArray( es.map(_.toJson))
}

case class EvalError(e: String) extends EvaluationError {
  override def toJson: JValue = JString(e)
}

case class EvalSyntaxErr(syntaxError: SyntaxError ) extends EvaluationError {
  override def toJson: JValue = syntaxError.toJson
}

object EvalSyntaxErr {
  def apply[T](syntaxError: SyntaxError)
  : Either[EvalSyntaxErr, T] = Left(new EvalSyntaxErr(syntaxError))
}

object EvalErrors {
  def apply[T](se: EvaluationError*)
  : Either[EvalErrors, T] = Left( new EvalErrors(se.toList))
}

object EvalError {
  def apply[T](s: String)
  : Either[EvalError, T] = Left( new EvalError(s))

  implicit class StringWithSyntaxPower(s: String) {
    def evalError = new EvalError(s)
  }
}

////////////////////////////////////////////////////////////////////////////////
// Fails: check json Format and Syntax
////////////////////////////////////////////////////////////////////////////////
sealed trait Fails extends ToJson

case class EvalFalse(value: String, e: String) extends Fails {
  override def toJson: JValue = JObject(
    "type" -> JString("Evaluated Result is false"),
    "value" -> JString(value),
    "result" -> JString(e) )
}

case class KeyNotExist( key: String) extends Fails {
  override def toJson: JValue = JObject(
    "type" -> JString("Key does not exist"),
    "error" -> JString(s"Key($key) must exist.")
  )
}

case class TypeError(value: JValue, e: String) extends Fails{
  override def toJson: JValue = JObject(
    "type" -> JString("Type Mismatch"),
    "value" -> value,
    "error" -> JString(e)
  )
}

case class FieldError(k: String, fails: Fails) extends Fails {
  override def toJson: JValue = fails.toJson  // should not be called directly.
}

case class ValueError( es: List[EvaluationError]) extends Fails {
  override def toJson = JObject(
    "type" -> JString("Evaluation Error"),
    "error" -> JArray( es.map( s => JString(s.toString) ))
  )
}

case class ObjectSyntaxFalse( e: String, str: List[String]) extends Fails {
  override def toJson = JObject(
    "type" -> JString("Object-Evaluation result is false"),
    "expression" -> JString(e),
    "interim" -> JArray( str.map( JString)),
    "result" -> JString("false")
  )
}

// todo
case class ObjectSyntaxError( es: EvaluationError) extends Fails {
  override def toJson = JObject(
    "type" -> JString("Object-Evaluation Error"),
    "error" -> es.toJson,
  )
}

case class ObjectError( es: List[Fails]) extends Fails {
  override def toString: String = es.mkString( "{ ", ",\n", " }")

  private val warning = "This must not happen!!!\t"

  def toJson: JValue = {
    val (lefts, rights) = {
      es.map {
        case FieldError(k, f) => Right(k -> f.toJson)
        case e@ObjectSyntaxError(_) => Right( "$$$$" -> e.toJson )
        case o@ObjectSyntaxFalse(_, _) => Right( "$$$$" -> o.toJson )
        case o =>
          StringUtil.show(s"ObjectError ::: $warning : $o")
          Left(o.toJson)
      }.partitionMap(identity)
    }

    if(lefts.isEmpty)
      JObject(rights)
    else {
      JObject( List("Invalid" -> JArray(lefts)) )
    }
  }
}

case class ArrayError( es: List[Fails]) extends Fails {
  override def toString: String = es.mkString( "[ ", ",\n", " ]")
  override def toJson: JValue = JArray( es.map( _.toJson))
}

////////////////////////////////////////////////////////////////////////////////
object TypeError {
  def apply[T](j: JValue, s: String): Left[TypeError, T] = Left( new TypeError(j, s))
}

object KeyNotExist {
  def apply[T](s: String): Left[KeyNotExist, T] = Left( new KeyNotExist(s))
}

object FieldError {
  def apply[T](s: (String, Fails)): Left[FieldError, T] = Left( FieldError( s._1, s._2))
}

object ValueError {
  def apply[T](s: EvaluationError*) : Either[ValueError, T] = Left( ValueError(s.toList))
}

object ObjectError {
  def apply[T](s: Fails*) : Either[ObjectError, T] = Left( ObjectError(s.toList))
}

object ArrayError {
  def apply[T](s: Fails*) : Either[ArrayError, T] = Left( ArrayError(s.toList))
}

