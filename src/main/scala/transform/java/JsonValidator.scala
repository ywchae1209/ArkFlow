package transform.java

import org.json4s.jackson.Serialization.writePretty
import org.json4s.{DefaultFormats, JArray, JObject, JValue}
import transform.jsonValidator.{Fails, FormatJson, SyntaxError}
import transform.utils.JsonUtil.StringWithJsonPower
import transform.utils.StringUtil
import transform.utils.StringUtil.show

import scala.collection.immutable.HashMap

case class JsonValidator( rules: Map[String, FormatJson],
                          syntaxError: Map[String, SyntaxError] ) {

  implicit val formats: DefaultFormats.type = DefaultFormats

  def evaluate(id: String, json: String): ValidateResult = {

    rules.get(id).map{ fj =>
      val ret = json.toJValueOr().map(j => fj.evaluate(j) )
      ValidateResult(ret)
    }.getOrElse( ValidateResult(Left(s"rule($id) not exist")))

  }

  def showErrors(): String = {
    val js = JArray(
        syntaxError
          .map(kv => JObject(kv._1 -> kv._2.toJson))
          .toList )

    writePretty(js)
  }

  def showRules( id: String): String = {
    val js = JArray(
      rules
        .map(kv => JObject(kv._1 -> kv._2.toJson))
        .toList )

    writePretty(js)
  }

  def findCandidate(json: String): List[String] = {

    json.toJValueOr().map{ j =>
      rules.flatMap{ case ( id, fj) => fj.evaluate(j).toOption.map( _ => id) }.toList
    }.fold(
      e => { show(e); Nil },
      l => l
    )
  }
}

/**
 * wrapper for Java Compatibility
 */
case class ValidateResult( private val ret: Either[String, Either[Fails, JValue]] ) {

  def isFail(): Boolean = !isSuccess()
  def isSuccess(): Boolean = ret.fold( _ => false, _.isRight)

  def getFailReason(): String = ret.fold(
    e => s"invalid json : $e",
    r => r.fold( _.show(), _ => ""))

  implicit val formats: DefaultFormats.type = DefaultFormats
  def getSuccess(): String = ret.fold(
    e => "",
    r => r.fold( _ => "", writePretty(_) )
  )

  def show() = {

    val r0 = s"\nisFail() == ${this.isFail()}\n" + s"isSuccess() == ${this.isSuccess()}\n"
    val r1 = if(isSuccess()) "\n<<< extracted >>>\n" + getSuccess() else "\n<<< Fail reason >>>\n" + getFailReason()

    StringUtil.show( r0 )
    StringUtil.show( r1 )
  }


}

object JsonValidator {

  def apply(rules:(String, String)*): JsonValidator = {

    val (lefts, rights) = rules.map { case( id, jstr) =>
        FormatJson(jstr)
          .map( id -> _)
          .left.map( id -> _)
    }.partitionMap( identity)

    val rs = HashMap( rights:_*)
    val se = HashMap( lefts:_*)

    if(se.nonEmpty) se.foreach{ case (id, e) => e.show( s"syntax error in rule($id)") }

    JsonValidator(rs, se)
  }

  def apply(id: String, rule: String): JsonValidator = JsonValidator( id -> rule)

}


