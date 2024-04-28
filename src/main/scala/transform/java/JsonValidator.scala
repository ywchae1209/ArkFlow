package transform.java

import org.json4s.JValue
import transform.jsonValidator.{FormatJson, SyntaxError}
import transform.utils.JsonUtil.{JValueWithPower, StringWithJsonPower}

case class JsonValidator(vr: Either[SyntaxError, FormatJson]) extends ShowStatus {

  override def isFail(): Boolean = vr.isLeft
  override def isSuccess(): Boolean = vr.isRight
  override def getFailReason(): String = vr.fold( _.pretty, _ => "")
  override def show(): String = vr.fold( _.pretty, _.pretty)

  def validate(json: String) = {
    val ret =
      for {
        r <- vr.left.map( _.pretty)
        j <- json.toJValueOr().left.map( _ => "not valid json")
        o <- r.evaluate(j).left.map( _.pretty)
      } yield o

    ValidateResult(ret)
  }
}

object JsonValidator {
  def apply(ruleString: String): JsonValidator = {
    val r = FormatJson(ruleString)
    JsonValidator(r)
  }
}

case class ValidateResult(r: Either[String, JValue]) extends ShowStatus {
  override def isFail(): Boolean = r.isLeft
  override def isSuccess(): Boolean = r.isRight
  override def getFailReason(): String = r.fold( identity, _ => "")
  override def show(): String = r.fold( identity, _.pretty)

  def getJson() = r.fold(_ => "", _.pretty)
}

