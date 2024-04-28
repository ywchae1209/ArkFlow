package transform.jsonValidator

import org.json4s.JsonAST.JObject
import org.json4s.{JArray, JString, JValue}
import transform.common.ToJson

////////////////////////////////////////////////////////////////////////////////
// SyntaxError : check syntax for field Value
////////////////////////////////////////////////////////////////////////////////
sealed trait SyntaxError extends ToJson

////////////////////////////////////////////////////////////////////////////////
// SyntaxError-Tree Holder
case class SyntaxErrorObjectNode(err: List[(String, SyntaxError)]) extends SyntaxError {
  override def toJson: JValue = JObject( err.map(kv => kv._1 -> kv._2.toJson))
}

case class SyntaxErrorArrayNode(err: List[SyntaxError]) extends SyntaxError {
  override def toJson: JValue = JArray( err.map( _.toJson))
}


////////////////////////////////////////////////////////////////////////////////
// Rule-Json Structure Error
case class InvalidRuleJsonError(j: String) extends SyntaxError {
  override def toJson: JValue = JString(j)
}

case class InvalidRuleNodeError(e: String) extends SyntaxError {
  override def toJson: JValue = JString(e)
}

////////////////////////////////////////////////////////////////////////////////
/// SyntaxErrorValue
sealed trait SyntaxErrorValue extends SyntaxError {
  def flatten(sv: SyntaxErrorValue): List[SyntaxErrorValue] = {

    // todo :: tailrec
    sv match {
      case SyntaxErrorValues(e, _) => e.flatMap( flatten)
      case o => List(o)
    }
  }
}

// multiple-error holder
case class SyntaxErrorValues( e: List[SyntaxErrorValue], exp: String) extends SyntaxErrorValue {
  override def toJson: JValue = {
    JObject(
      "type" -> JString("Value Syntax error"),
      "value" -> JString(exp),
      "result" -> JArray( flatten(this).map(_.toJson)) )
  }
}

// parse fail
case class InvalidSyntaxValueRule( e: String) extends SyntaxErrorValue {
  override def toJson: JValue = JString(e)
}

case class UnknownFunction(name: String, args: Seq[String]) extends SyntaxErrorValue {
  val st =
    if (args.isEmpty) s"unknown-function:: $name"
    else args.mkString(s"unknown-function:: $name(", ",", ")" )

  def toJson = JString(st)
}

case class ArgumentCountError(name: String, args: Seq[String], n: Int) extends SyntaxErrorValue {

  private def functionString(name: String, l: Seq[String])
  = if(l.isEmpty) name else l.mkString( s"$name(", ",", ")")

  val st = s"arg-count must $n :: ${functionString(name, args)}"

  def toJson = JString(st)
}

case class ArgumentTypeError(name: String, args: Seq[String], msg: String) extends SyntaxErrorValue {

  private def functionString(name: String, l: Seq[String])
  = if(l.isEmpty) name else l.mkString( s"$name(", ",", ")")

  val st = s"type error($msg):: ${functionString(name, args)}"

  def toJson = JString(st)
}

////////////////////////////////////////////////////////////////////////////////
sealed trait SyntaxErrorObject extends SyntaxError {

  def flatten(sv: SyntaxErrorObject): List[SyntaxErrorObject] = {

    // todo :: tailrec
    sv match {
      case SyntaxErrorObjects(e, _) => e.flatMap( flatten)
      case o => List(o)
    }
  }
}


case class SyntaxErrorObjects(e: List[SyntaxErrorObject], exp: String) extends SyntaxErrorObject {
  override def toJson: JValue = {
    JObject(
      "type" -> JString("Object Syntax error"),
      "value" -> JString(exp),
      "result" -> JArray( flatten(this).map(_.toJson)) )
  }
}

// parse fail
case class InvalidSyntaxObjectRules( e: List[InvalidSyntaxObjectRule]) extends SyntaxErrorObject {
  override def toJson: JValue = JArray( e.map(_.toJson))
}

case class InvalidSyntaxObjectRule( e: String) extends SyntaxErrorObject {
  override def toJson: JValue = JString( e)
}

////////////////////////////////////////////////////////////////////////////////
