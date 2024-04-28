package transform.java

import org.json4s.JValue
import transform.jsonConverter.Fails.not_valid_json
import transform.jsonConverter.{Fails, TurnJson, TurnRoot}
import transform.utils.JsonUtil.{JValueWithPower, StringWithJsonPower}

case class JsonConverter(er: Either[Fails.RuleSyntaxError, TurnRoot]) extends ShowStatus {

  override def isFail(): Boolean = er.isLeft
  override def isSuccess(): Boolean = er.isRight
  override def getFailReason(): String = er.fold( _.pretty, _ => "")
  override def show(): String = er.fold( _.pretty, _.pretty)

  def convert(json: String) = {
    val ret =
      for {
        r <- er.left.map( _.pretty)
        j <- json.toJValueOr.left.map( _ => not_valid_json.pretty)
        o <- r.convert(j, j).left.map( _.pretty)
      } yield o

    ConvertResult(ret)
  }
}

case class ConvertResult(r: Either[String, JValue]) extends ShowStatus {

  override def isFail(): Boolean = r.isLeft
  override def isSuccess(): Boolean = r.isRight
  override def getFailReason(): String = r.fold( identity, _ => "")
  override def show(): String = r.fold(identity, _.pretty)

  def getJson(): String = r.fold(_ => "", _.pretty)
}

object JsonConverter {

  def apply(ruleString: String): JsonConverter = {
    val r = ruleString.toJValueOr.left.map(_ => not_valid_json).flatMap(j => TurnJson(j))
    JsonConverter(r)
  }
}
