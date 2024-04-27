package transform.jsonConverter

import org.json4s.{JArray, JObject, JString, JValue}
import transform.traits.ToJson

object Fails {

  ////////////////////////////////////////////////////////////////////////////////
  // rule-syntax-error
  ////////////////////////////////////////////////////////////////////////////////
  trait RuleSyntaxError extends ToJson {
    override def toJson: JValue = this match {
      case RSE(s) => JString(s)
      case RSEs(s) => JArray(s.map(_.toJson))
      case RSEo(s) => JObject(s.map(kv => kv._1 -> kv._2.toJson))
    }

  }
  case class RSE(s: String) extends RuleSyntaxError
  case class RSEs(s: List[RuleSyntaxError]) extends RuleSyntaxError
  case class RSEo(s: List[(String, RuleSyntaxError)]) extends RuleSyntaxError

  ////////////////////////////////////////////////////////////////////////////////
  // run-time fail
  ////////////////////////////////////////////////////////////////////////////////
  sealed trait ConvertError extends ToJson{
    def toJson = this match {
      case CVE(s)     => JString(s)
      case CVEs(s)    => JArray(s.map(_.toJson).toList)
      case CVEo(s)    => JObject(s.map(kv => kv._1 -> kv._2.toJson).toList)
    }
  }
  case class CVE(s: String) extends ConvertError
  case class CVEs(s: Seq[ConvertError]) extends ConvertError
  case class CVEo(s: Seq[(String, ConvertError)]) extends ConvertError

  object CVE2 {
    def CVE20[T](p: String, s: String, l: Seq[T]) = if(l.isEmpty) List(p -> CVE(s)) else Nil
    def CVE2s(p: String, s: Seq[ConvertError]) = if(s.nonEmpty) List( p -> CVEs(s)) else Nil
  }
}
