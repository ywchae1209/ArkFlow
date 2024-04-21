package validator.utils

import org.json4s.jackson.JsonMethods
import org.json4s.{JArray, JBool, JDecimal, JDouble, JInt, JLong, JNothing, JNull, JObject, JSet, JString, JValue}
import May.{mayOr, maybe}
import org.json4s.JsonAST.JField

import scala.annotation.tailrec

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

  def getObjectValues(j: JValue)
  : Either[String, List[(String, JValue)]] = j match {
    case JObject(obj) => Right(obj)
    case o => Left(s"not JObject : $o")
  }

  def getArrayValues(j: JValue)
  : Either[String, List[JValue]] = j match {
    case JArray(arr) => Right(arr)
    case o => Left(s"not JArray : $o")
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
    case JString(s) => s.toLongOption.toRight(s"string($s) to Long failed")
    case JDouble(n) => maybe(n.toLong).toRight(s"Double($n) to Long failed")
    case JDecimal(n) => maybe(n.toLong).toRight(s"BigDecimal($n) to Long failed")
    case JLong(n) => Right(n)
    case JInt(n) => maybe(n.toLong).toRight(s"BigInt($n) to Long failed")
    case o => Left(s"not numeric type : $o")
  }

  def toBigInt(j: JValue): Either[String, BigInt]
  = j match {
    case JString(s) => maybe(BigInt(s)).toRight(s"string($s) to BigInt failed")
    case JDouble(n) => maybe( BigInt(n.toLong)).toRight(s"Double($n) to BigInt failed")
    case JDecimal(n) => maybe( n.toBigInt).toRight(s"BigDecimal($n) to BigInt failed")
    case JLong(n) => maybe(BigInt(n)).toRight(s"long($n) to BigInt failed")
    case JInt(n) => Right(n)
    case o => Left(s"not numeric type : $o")
  }


  ////////////////////////////////////////////////////////////////////////////////
  def getDouble(j: JValue): Either[String, Double]
  = j match {
    case JDouble(d) => Right(d)
    case o => Left(s"not BigDecimal : $o")
  }

  def getBigDecimal(j: JValue): Either[String, BigDecimal]
  = j match {
    case JDecimal(d) => Right(d)
    case o => Left(s"not BigDecimal : $o")
  }

  def getBigInt(j: JValue): Either[String, BigInt]
  = j match {
    case JInt(i) => Right(i)
    case o => Left(s"not BigInt : $o")
  }


  def getLong(j: JValue): Either[String, Long]
  = j match {
    case JLong(l) => Right(l)
    case o => Left(s"not Long : $o")
  }

  def getNumeric(j: JValue): Either[String, JValue]
  = j match {
    case JDouble(_)
         | JDecimal(_)
         | JInt(_)
         | JLong(_) => Right(j)
    case o => Left(s"not Numeric : $o")
  }


  def getBoolean(j: JValue ): Either[String, Boolean]
  = j match {
    case JBool(b) => Right(b)
    case _ => Left("not Bool")
  }

  def getString(j: JValue): Either[String, String]
  = j match {
    case JString(s) => Right(s)
    case _ => Left("not String")
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


    // todo
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    def \~ (name: String*): LazyList[JValue] = {
      name.to(LazyList).map( j \ _)
    }
    def \~~ : LazyList[JValue] = j.children.to(LazyList)
    def \\? ( pred: String => Boolean): LazyList[JValue] = {

      def find(json: JValue): LazyList[JValue] = json match {
        case JObject(l) =>
          l.to(LazyList)
            .foldLeft(LazyList[JValue]()) { case (b, (name, value)) =>
              b.lazyAppendedAll(
                if (pred(name)) value +: find(value)
                else find(value)
              )
            }
        case JArray(l) =>
          l.to(LazyList)
            .foldLeft(LazyList[JValue]())((b, json) =>
              b.lazyAppendedAll( find(json)) )
        case _ =>
          LazyList.empty
      }

      find(j)

    }
    def \\~ (name: String): LazyList[JValue] = \\?( _ == name)

    def \\~~ : LazyList[JValue] = \\? ( _ => true)

    def slice( start: Option[Int], end: Option[Int], step: Int)
    : LazyList[JValue] = j match {
        case JArray(l) =>
          // todo :: spec
          val len = l.length
          val s0 = start.map( i => if(i < 0) len + i else i ).getOrElse(0)
          val e0   = end.getOrElse(len)
          val (from, to) = (s0 min e0, s0 max e0)
          val is = from until to  by step

          is.to(LazyList).flatMap( l.lift )

        case _ => LazyList.empty
      }

    def random( index: Seq[Int])
    : LazyList[JValue] = j match {
      case JArray(l) => index.to(LazyList).flatMap( l.lift)
      case _         => LazyList.empty
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////



    def getJValue0(route: Seq[String])
    : JValue = JsonUtil.getJValue0(j, route)

    def getObjectValues()
    : Either[String, List[(String, JValue)]] = JsonUtil.getObjectValues(j)

    def getArrayValues()
    : Either[String, List[JValue]] = JsonUtil.getArrayValues(j)

    //////////////////////////////////////////////////
    def getLong()
    : Either[String, Long] = JsonUtil.getLong(j)

    def getLong(route: Seq[String])
    : Either[String, Long] = getJValue0(route).toLong()

    def getBigInt(route: Seq[String])
    : Either[String, BigInt] = getJValue0(route).toBigInt()

    def getDouble(route: Seq[String])
    : Either[String, Double] = getJValue0(route).toDouble()

    def getBigDecimal(route: Seq[String])
    : Either[String, BigDecimal] = getJValue0(route).toBigDecimal()

    def toLong()
    : Either[String, Long] = JsonUtil.toLong(j)

    def toBigInt()
    : Either[String, BigInt] = JsonUtil.toBigInt(j)

    def toDouble()
    : Either[String, Double] = JsonUtil.toDouble(j)

    def toBigDecimal()
    : Either[String, BigDecimal] = JsonUtil.toBigDecimal(j)

    def getDouble()
    : Either[String, Double] = JsonUtil.getDouble(j)

    def getBigDecimal()
    : Either[String, BigDecimal] = JsonUtil.getBigDecimal(j)

    def getBigInt()
    : Either[String, BigInt] = JsonUtil.getBigInt(j)

    def getBoolean()
    : Either[String, Boolean] = JsonUtil.getBoolean(j)

    def getString()
    : Either[String, String] = JsonUtil.getString(j)

    def getNumeric()
    : Either[String, JValue] = JsonUtil.getNumeric(j)
  }

}
