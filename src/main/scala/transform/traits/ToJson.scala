package transform.traits

import org.json4s.{DefaultFormats, JValue}
import org.json4s.jackson.Serialization.writePretty
import transform.utils.StringUtil

trait ToJson{

  implicit val formats: DefaultFormats.type = DefaultFormats

  def toJson: JValue
  def pretty: String = writePretty(toJson)
  def show(header: String= ""): String =  {
    StringUtil.show(header + "\n" + writePretty(toJson))
  }
}

