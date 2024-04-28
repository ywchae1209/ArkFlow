package transform.jsonConverter

import org.json4s.{JArray, JObject, JString, JValue}
import transform.common.ToJson

object Fails {

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

  // not confirm reason
  ////////////////////////////////////////////////////////////////////////////////
  sealed trait CauseOfDenial extends ToJson {
    def toJson = this match {
      case COD(s)     => JString(s)
      case CODs(s)    => JArray(s.map(_.toJson).toList)
      case CODo(s)    => JObject(s.map(kv => kv._1 -> kv._2.toJson).toList)
    }
  }
  case class COD(s: String) extends CauseOfDenial
  case class CODs(s: Seq[CauseOfDenial]) extends CauseOfDenial
  case class CODo(s: Seq[(String, CauseOfDenial)]) extends CauseOfDenial

  // some utils
  ////////////////////////////////////////////////////////////////////////////////
  def RSE2s_lefts[A]( es: Either[(String, RuleSyntaxError), A]* ) =
    es.partitionMap(identity)._1.toList

  ////////////////////////////////////////////////////////////////////////////////
  def COD2(t: (String, String)): (String, COD)
  = t._1 -> COD(t._2)

  def COD2s_ifNone[A](o: Option[A])(ifNone: => (String, String)): Seq[(String, COD)]
  = o.map( _ => Seq.empty).getOrElse(Seq( COD2(ifNone)))

  def COD2s_ifSome[A](o: Option[A])(ifSome: => (String, String)): Seq[(String, COD)]
  = o.map( _ => Seq(COD2(ifSome))).getOrElse(Seq.empty)

  def COD2s_cond(b: Boolean)(cod2: (String,String)*): Seq[(String, COD)]
  = if(b) cod2.map(t => t._1 -> COD(t._2)) else Nil

  def COD2s_Nel(p: String, s: Seq[CauseOfDenial]): Seq[(String, CODs)]
  = if(s.nonEmpty) List( p -> CODs(s)) else Nil

}
