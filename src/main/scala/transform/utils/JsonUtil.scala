package transform.utils

import org.json4s.jackson.JsonMethods
import org.json4s.{DefaultFormats, JArray, JBool, JDecimal, JDouble, JInt, JLong, JNothing, JNull, JNumber, JObject, JSet, JString, JValue, JsonAST}
import May.{mayOr, maybe}
import org.json4s.jackson.Serialization.writePretty

import scala.annotation.tailrec
import scala.util.matching.Regex

object JsonUtil {

  ////////////////////////////////////////////////////////////////////////////////
  def toJValue(str: String): Either[String, JValue] = {
    mayOr( JsonMethods.parse(str) )("Invalid JSON string")
  }

  ////////////////////////////////////////////////////////////////////////////////
  @tailrec
  def getJValue0(j: JValue, route: Seq[String]): JValue = {
    if (route.isEmpty) j else getJValue0(j \ route.head, route.tail)
  }

  ////////////////////////////////////////////////////////////////////////////////
  def getJValue(j: JValue, route: Seq[String])
  : Either[String, JValue] = getJValue0(j, route) match {
    case JNothing => Left(s"not exist: $route in $j")
    case jv => Right(jv)
  }

  ////////////////////////////////////////////////////////////////////////////////
  def toDouble(j: JValue): Either[String, Double]
  = j match {
    case JString(s) => s.toDoubleOption.toRight(s"string($s) to Double failed")
    case JDouble(n) => Right(n)
    case JDecimal(n) => maybe(n.toDouble).toRight(s"BigDecimal($n) convert fail")
    case JLong(n) => maybe(n.toDouble).toRight(s"long($n) convert fail")
    case JInt(n) => maybe(n.toDouble).toRight(s"BigInt($n) convert fail")
    case o => Left(s"not numeric type : $o")
  }

  def toBigDecimal(j: JValue): Either[String, BigDecimal]
  = j match {
    case JString(s) => maybe(BigDecimal(s)).toRight(s"string($s) to BigDecimal failed")
    case JDouble(n) => maybe(BigDecimal(n)).toRight(s"Double($n) to BigDecimal failed")
    case JDecimal(n) => Right(n)
    case JLong(n) => maybe(BigDecimal(n)).toRight(s"long($n) to BigDecimal failed")
    case JInt(n) => maybe(BigDecimal(n)).toRight(s"BigInt($n) to BigDecimal failed")
    case o => Left(s"not numeric type : $o")
  }

  def toLong(j: JValue): Either[String, Long]
  = j match {
    case JString(s) => s.toLongOption.toRight(s"String($s) can't be Long")
    case JDouble(n) => maybe(n.toLong).toRight(s"Double($n) can't be Long")
    case JDecimal(n) => maybe(n.toLong).toRight(s"BigDecimal($n) can't be Long")
    case JLong(n) => Right(n)
    case JInt(n) => maybe(n.toLong).toRight(s"BigInt($n) can't be Long")
    case o => Left(s"not numeric type : $o")
  }

  def toBigInt(j: JValue): Either[String, BigInt]
  = j match {
    case JString(s) => maybe(BigInt(s)).toRight(s"String($s) can't be BigInt")
    case JDouble(n) => maybe( BigInt(n.toLong)).toRight(s"Double($n) can't be BigInt")
    case JDecimal(n) => maybe( n.toBigInt).toRight(s"BigDecimal($n) can't be BigInt")
    case JLong(n) => maybe(BigInt(n)).toRight(s"long($n) can't be BigInt")
    case JInt(n) => Right(n)
    case o => Left(s"not numeric type : $o")
  }

  ////////////////////////////////////////////////////////////////////////////////
  def asObject0(j: JValue) : Option[JObject]
  = j match {
    case o@JObject(_) => Some(o)
    case _ => None
  }

  def asObjectValues0(j: JValue): Option[List[(String, JValue)]]
  = asObject0(j).map( _.obj)

  def asArray0(j: JValue): Option[JArray]
  = j match {
    case a@JArray(_) => Some(a)
    case o => None
  }

  def asArrayValues0(j: JValue): Option[List[JValue]]
  = asArray0(j).map( _.arr)

  def asDouble0(j: JValue): Option[ Double]
  = j match {
    case JDouble(d) => Some(d)
    case o => None
  }

  def asBigDecimal0(j: JValue): Option[BigDecimal]
  = j match {
    case JDecimal(d) => Some(d)
    case _ => None
  }

  def asBigInt0(j: JValue): Option[BigInt]
  = j match {
    case JInt(i) => Some(i)
    case _ => None
  }

  def asLong0(j: JValue): Option[Long]
  = j match {
    case JLong(l) => Some(l)
    case _ => None
  }

  def asNumeric0(j: JValue): Option[JValue]
  = j match {
    case JDouble(_)
         | JDecimal(_)
         | JInt(_)
         | JLong(_) => Some(j)
    case _ => None
  }

  def asBoolean0(j: JValue ): Option[Boolean]
  = j match {
    case JBool(b) => Some(b)
    case _ => None
  }

  def asString0(j: JValue): Option[String]
  = j match {
    case JString(s) => Some(s)
    case _ => None
  }

  def asObject(j: JValue): Either[String, JObject]
  = asObject0(j).toRight( s"not JObject : $j")

  def asObjectValues(j: JValue): Either[String, List[(String, JValue)]]
  = asObject(j).map( _.obj)

  def asArray(j: JValue): Either[String, JArray]
  = asArray0(j).toRight( s"not JArray : $j")

  def asArrayValues(j: JValue) : Either[String, List[JValue]]
  = asArray(j).map( _.arr)

  def asDouble(j: JValue) : Either[String, Double]
  = asDouble0(j).toRight( s"not BigDecimal : $j")

  def asBigDecimal(j: JValue) : Either[String, BigDecimal]
  = asBigDecimal0(j).toRight(s"not BigDecimal : $j")

  def asBigInt(j: JValue) : Either[String, BigInt]
  = asBigInt0(j).toRight(s"not BigInt : $j")

  def asLong(j: JValue) : Either[String, Long]
  = asLong0(j).toRight(s"not Long : $j")

  def asNumeric(j: JValue) : Either[String, JValue]
  = asNumeric0(j).toRight(s"not Numeric : $j")

  def asBoolean(j: JValue ) : Either[String, Boolean]
  = asBoolean0(j).toRight(s"not Bool : $j")

  def asString(j: JValue) : Either[String, String]
  = asString0(j).toRight( s"not String: $j")

  def isNothing(j: JValue) : Boolean
  = j match {
    case JNothing => true
    case _ => false
  }

  ////////////////////////////////////////////////////////////////////////////////
  // implicits for dev.
  ////////////////////////////////////////////////////////////////////////////////
  implicit class StringWithJsonPower(str: String) {

    def toJValue()
    : JValue = JsonUtil.toJValue(str).getOrElse(JNothing)

    def toJValueOr(): Either[String, JValue] = JsonUtil.toJValue(str)

  }

  implicit class JValueWithPower(j: JValue) {

    implicit val formats: DefaultFormats.type = DefaultFormats
    def pretty: String = writePretty(j)

    def nonEmpty = j match {
      case JNothing => None
      case _ => Some(j)
    }

    // todo
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    def asList(): List[JValue] = j match {
      case JArray(l) => l
      case o => List(o)
    }

    def =~ (r: Regex) = j match {
      case JString(s) =>
        val found = r.findFirstIn(s).isDefined
        if(!found) println( s"not matched =~ regex: ${j}")
        found
      case _ =>
        println( s"not String =~ regex: ${j}")
        false
    }


    def \~ (name: String*): LazyList[JValue] = {

      def find (pred: String => Boolean): Option[JValue] = j match {
        case JObject(l) => l.find( kv => pred( kv._1)).map( _._2)
        case o => None
      }

      name.to(LazyList).flatMap( k => find(_ == k) )
    }

    def \~~ : LazyList[JValue] = j.children.to(LazyList)

    def \\? ( pred: String => Boolean): LazyList[JValue] = {

      def find(json: JValue): LazyList[JValue] = json match {
        case JObject(l) =>
          l.to(LazyList)
            .foldLeft(LazyList[JValue]()) { case (b, (name, value)) =>
              b.lazyAppendedAll(
                if ( pred(name) ) value +: find(value)
                else find(value)
              )
            }
        case JArray(l) =>
          l.to(LazyList)
            .foldLeft(LazyList[JValue]())((b, json) => b.lazyAppendedAll( find(json)) )

        case _ =>
          LazyList.empty
      }

      find(j)

    }

    def \\~ (name: String): LazyList[JValue] = \\?( _ == name)

    def \\~~ : LazyList[JValue] = {

      def all(json: JValue): LazyList[JValue] = {
        val ar = json.\~~
        ar.lazyAppendedAll(
          ar.foldLeft(LazyList[JValue]())((b, json) => b.lazyAppendedAll( all(json)) )
        )
      }

      all(j)
    }

    def slice( start: Option[Int], end: Option[Int], step: Int)
    : LazyList[JValue] = j match {

        case JArray(l) =>
          val len = l.length
          val s0 = start.map( i => if(i < 0) len + i else i ).getOrElse(0)
          val e0   = end.getOrElse(len)
          val (from, to) = (s0 min e0, s0 max e0)
          val is = from until to  by step
//          println( is.mkString("Range( ", ",", ")"))

          is.to(LazyList).flatMap( l.lift )

        case _ => println(s"not array: $j")
          LazyList.empty
      }

    def random( index: Seq[Int])
    : LazyList[JValue] = j match {
      case JArray(l) => index.to(LazyList).flatMap( l.lift)
      case _         => LazyList.empty
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    def getJValue0(route: String*)
    : JValue = JsonUtil.getJValue0(j, route)

    def getObjectValues()
    : Either[String, List[(String, JValue)]] = JsonUtil.asObjectValues(j)

    def getArrayValues()
    : Either[String, List[JValue]] = JsonUtil.asArrayValues(j)

    //////////////////////////////////////////////////

    def toLong() : Either[String, Long] = JsonUtil.toLong(j)
    def toBigInt() : Either[String, BigInt] = JsonUtil.toBigInt(j)
    def toDouble() : Either[String, Double] = JsonUtil.toDouble(j)
    def toBigDecimal() : Either[String, BigDecimal] = JsonUtil.toBigDecimal(j)

    //////////////////////////////////////////////////////////////////
    def asObject0(): Option[JObject] = JsonUtil.asObject0(j)
    def asObjectValues0(): Option[List[(String, JValue)]] = JsonUtil.asObjectValues0(j)
    def asArray0(): Option[JArray] = JsonUtil.asArray0(j)
    def asArrayValues0(): Option[List[JValue]] = JsonUtil.asArrayValues0(j)
    def asDouble0(): Option[Double] = JsonUtil.asDouble0(j)
    def asBigDecimal0(): Option[BigDecimal] = JsonUtil.asBigDecimal0(j)
    def asBigInt0(): Option[BigInt] = JsonUtil.asBigInt0(j)
    def asLong0(): Option[Long] = JsonUtil.asLong0(j)
    def asNumeric0(): Option[JValue] = JsonUtil.asNumeric0(j)
    def asBoolean0(): Option[Boolean] = JsonUtil.asBoolean0(j)
    def asString0(): Option[String] = JsonUtil.asString0(j)

    def asObject(): Either[String, JObject] = JsonUtil.asObject(j)
    def asObjectValues(): Either[String, List[(String, JValue)]] = JsonUtil.asObjectValues(j)
    def asArray(): Either[String, JArray] = JsonUtil.asArray(j)
    def asArrayValues() : Either[String, List[JValue]] = JsonUtil.asArrayValues(j)
    def asDouble() : Either[String, Double] = JsonUtil.asDouble(j)
    def asBigDecimal() : Either[String, BigDecimal] = JsonUtil.asBigDecimal(j)
    def asBigInt() : Either[String, BigInt] = JsonUtil.asBigInt(j)
    def asLong() : Either[String, Long] = JsonUtil.asLong(j)
    def asNumeric() : Either[String, JValue] = JsonUtil.asNumeric(j)
    def asBoolean() : Either[String, Boolean] = JsonUtil.asBoolean(j)
    def asString() : Either[String, String] = JsonUtil.asString(j)

    //////////////////////////////////////////////////////////////////
    def isNothing(): Boolean = JsonUtil.isNothing(j)
    def notNothing(): Boolean = !isNothing()

    def toOption0 = if(isNothing()) None else Some(j)
  }

}
