package transform.jsonConverter.syntaxJsonpath.rule

import org.json4s.JsonAST.{JDouble, JString}
import org.json4s.{JArray, JBool, JDecimal, JInt, JLong, JNothing, JNull, JObject, JSet, JValue}
import transform.jsonConverter.Fails.COD
import transform.jsonConverter.syntaxJsonpath.rule.JsonPathAST.Literal
import transform.utils.JsonUtil.JValueWithPower
import transform.utils.May.mayOr

import scala.util.matching.Regex

case class UserFunctions( ufs: Seq[UserFunction] ) {

  val (lefts, rights) = ufs.map(_.f).partitionMap(identity)
  val fs =
    Either.cond(
      lefts.isEmpty,
      rights.reduceLeft((a, b) => (j:JValue) => a(j).flatMap(b)),   // fold functions
      lefts.mkString("function syntax error = ", ",", "")
    )

  val functionSyntaxError = fs.left.toOption

  def executeWith(jv: JValue): Either[COD, JValue]
  = fs.fold(
      rse => Left( COD(rse)),   // may not happen in run-time.
      f => f(jv).left.map( COD).map{ r =>
        println(jv, r)    // todo :: g3nie :: temp test
        r
      }
    )
}

case class UserFunction( f: Either[String, JValue => Either[String, JValue]],
                         signature: (String, Seq[Literal])) {
  override def toString: String = signature._2.mkString( s"${signature._1}(", ",", ")" )
}

object UserFunction {

  def apply(signature: (String, Seq[Literal])): UserFunction = {
    val name = signature._1
    val args = signature._2

    val f =
      userFunctionMaker
        .get(name).toRight( s"unknown-function( $name)")
        .map( _(args)).joinRight

    UserFunction(f, signature)
  }

  ////////////////////////////////////////////////////////////////////////////////
  val asString: JValue => Either[String, JString] = {
    case JObject(_)   => Left("JObject :: not Value")
    case JArray(_)    => Left("JArray :: not Value")
    case JSet(_)      => Left("JSet :: not Value")
    case JNothing     => Right("").map(JString)
    case JNull        => Right("").map(JString)
    case j@JString(_) => Right(j)
    case other        => Right(other.values.toString).map(JString)
  }

  val asString0: JValue => Either[String, JString] = {
    case JObject(_)   => Left("JObject :: not Value")
    case JArray(_)    => Left("JArray :: not Value")
    case JSet(_)      => Left("JSet :: not Value")
    case JNothing     => Left("JNothing :: not valid")
    case JNull        => Left("JNull :: not valid")
    case j@JString(_) => Right(j)
    case other        => Right(other.values.toString).map(JString)
  }

  private def getOr[L,R](default: R): Either[L, R] => R = _.getOrElse(default)

  val _trim   : JValue => Either[String, JString] = _.asString().map( _.trim).map( JString)
  val _toLower: JValue => Either[String, JString] = _.asString().map( _.toLowerCase()).map(JString)
  val _toUpper: JValue => Either[String, JString] = _.asString().map( _.toUpperCase()).map(JString)

  val _toLong : JValue => Either[String, JLong]
  = _.asString().map( _.toLongOption.toRight("fail to convert Long")).joinRight.map(JLong)

  val _toDouble: JValue => Either[String, JDouble]
  = _.asString().map(_.toDoubleOption.toRight("fail to convert Double")).joinRight.map(JDouble)

  ////////////////////////////////////////////////////////////////////////////////
  // 1-arg
  def _toStringOr(default: Literal)
  : Either[String, JValue => Either[String, JString]]
  = {
    val f = default.jv.asString0().toRight("not valid input")   // check syntax error
    f.map( d => (jv: JValue) => asString0(jv).orElse( Right(d)) )
  }

  def _toLongOr(default: Literal)
  : Either[String, JValue => Either[String, JLong]]
  = {
    val f = default.jv.toLong().map( JLong)   // check syntax error
    f.map( d => (jv: JValue) => _toLong(jv).orElse( Right(d)) )
  }

  def _toDoubleOr(default: Literal)
  = {
    val f = default.jv.toDouble().map(JDouble) // check syntax error
    f.map( d => (jv: JValue) => _toDouble(jv).orElse(Right(d)) )
  }

  private def toChar1( sep: Literal): Either[String, Char]
  = sep.jv.asString.flatMap(s => if (s.length == 1) Right(s.head) else Left(s"not-single-char($s)"))

  private def extractNumber(sep: Char)(s: String)
  = {
    val ps = s"^-?\\d{1,3}(?:\\$sep\\d{3})*(\\.\\d+)?$$"
    val pat = new Regex(ps)

    pat
      .findFirstIn(s)
      .map( matched => matched.replace(sep.toString, "") )
      .toRight(s"invalid-format($s is not 3-digit-sep with $sep)")
  }

  private def extractNumericString(sep: Literal)
  : Either[String, JValue => Either[String, String]]
  = {
    // check syntax error
    val c = toChar1(sep)
    val f = c.map( extractNumber )

    // check run-time exception
    val ret = f
      .map( ex => (jv:JValue) => jv.asString().flatMap(ex) )
    ret
  }

  def _toLongSep( sep: Literal)
  : Either[String, JValue => Either[String, JLong]]
  = {
    val ext = extractNumericString(sep)
    val ret = ext.map( _ andThen( _.flatMap( _.toLongOption.map(JLong).toRight("can't be Long")) ))
    ret
  }


  def _toDoubleSep(sep: Literal)
  = {
    val ext = extractNumericString(sep)
    val ret = ext.map( _ andThen( _.flatMap( _.toDoubleOption.map(JDouble(_)).toRight("can't be Double")) ))
    ret
  }

  ////////////////////////////////////////////////////////////////////////////////
  // 2-arg
  def _toLongSepOr(sep: Literal, default: Literal)
  = {
    for {
      rep <- default.jv.asLong().map(JLong)
      ext <- extractNumericString(sep)
      ret = ext.andThen( _.flatMap( _.toLongOption.map(JLong).orElse(Some(rep)).toRight("can't be Long")) )
    } yield ret
  }

  def _toDoubleSepOr(sep: Literal, default: Literal)
  = {
    for {
      rep <- default.jv.asLong().map(JDouble(_))
      ext <- extractNumericString(sep)
      ret = ext.andThen( _.flatMap( _.toDoubleOption.map(JDouble).orElse(Some(rep)).toRight("can't be Long")) )
    } yield ret
  }

  implicit case class StringWithPower(str: String) {

    def sliceBetween(prefix: String, suffix: String, ifFail: String): String = {
      val start = str.indexOf(prefix)
      if (start >= 0) {
        val from = start + prefix.length
        val end = str.indexOf(suffix, from)
        if (end >= 0) str.substring(from, end) else ifFail
      } else ifFail
    }


    def sliceString(s: Int, e: Int) = {
      val len = str.length
      val s0 = if (s < 0) s + len else s
      val e0 = if (e <= 0) e + len else e
      str.substring(s0, e0)
    }
  }

    def _subString(start: Literal, end: Literal)
  : Either[String, JValue => Either[String, JString]]
  = {
    // check syntax error
    val args = for {
      s <- start.jv.toLong().map(_.toInt)
      e <- end.jv.toLong().map(_.toInt)
    } yield(s,e)

    // check run-time error
    val ret = args.map{ case (s, e) =>
      (jv: JValue) => for{
        j <- jv.asString()
        r <- mayOr(j.sliceString(s, e))("")       // modified
      } yield JString(r)
    }
    ret
  }

  def _replace(tar: Literal, rep: Literal)
  : Either[String, JValue => Either[String, JString]]
  = {
    // check syntax error
    val args = for {
      s <- tar.jv.asString()
      e <- rep.jv.asString()
    } yield (s, e)

    // check run-time error
    val ret = args.map{ case(s, e) =>
      (jv: JValue) => {
        for{
        j <- jv.asString()
        r <- mayOr( j.replace(s, e))("")
      } yield JString(r)
      }
    }
    ret
  }

  def _regexReplace(tar: Literal, rep: Literal)
  : Either[String, JValue => Either[String, JString]]
  = {
    // check syntax error
    val args = for {
      s <- tar.jv.asString().flatMap( r => mayOr(r.r)("compile regex"))
      e <- rep.jv.asString()
    } yield (s, e)

    // check run-time error
    val ret = args.map{ case(s, e) =>

      (jv: JValue) => for{
        j <- jv.asString()
        r <- mayOr(s.replaceAllIn(j, e))("replaceAllIn")
      } yield JString(r)
    }
    ret
  }

  ////////////////////////////////////////////////////////////////////////////////
  // 3-arg
  def _replaceIf(exact: Literal, ifTrue: Literal, ifFalse: Literal)
  = {

    val args = for {
      e <- exact.jv.asString()
      t <- ifTrue.jv.asString()
      f <- ifFalse.jv.asString()
    } yield ( e, t, f)

    val ret = args.map{ case(e,t,f) =>
      (jv: JValue) => for{
        j <- jv.asString()
        r = if(j == e) t else f
      } yield JString(r)
    }

    ret
  }

  def _subStringBetween(prefix: Literal, suffix: Literal, ifFail: Literal)
  = {

    val args = for {
      p <- prefix.jv.asString()
      s <- suffix.jv.asString()
      f <- ifFail.jv.asString()
    } yield (p, s, f)

    val ret = args.map{ case(p, s, f) =>
      (jv: JValue) => for{
        j <- jv.asString()
        r = j.sliceBetween(p, s, f)
      } yield JString(r)
    }

    ret
  }

  //  def _regexReplace( pattern: String, rep: String): JValue => Either[String, JString] = ???

  val abs: JValue => Either[String, JValue] = {
    case JDouble(num) => Right( JDouble(num.abs))
    case JDecimal(num)=> Right( JDecimal(num.abs))
    case JLong(num)   => Right( JLong(num.abs))
    case JInt(num)    => Right( JInt(num.abs))
    case JNothing     => Left("JNothing is not numeric")
    case JNull        => Left("JNull is not numeric")
    case JString(s)   => Left( s"String($s) is not numeric")
    case JBool(_)     => Left( "JBool is not numeric")
    case JObject(_)   => Left( "JObject is not numeric")
    case JArray(_)    => Left( "JArray is not numeric")
    case JSet(_)      => Left( "JSet is not numeric")
  }

  private def arg0[L,R,A](right: R, left: L)(as: Seq[A])
  : Either[L, R]
  = Either.cond( as.isEmpty, right, left)

  private def arg1[L,R,A,B](right: A => Either[L, B => Either[L,R]], left: L)(as: Seq[A])
  : Either[L, B => Either[L, R]]
  = Either.cond( as.length == 1, right(as.head), left).joinRight

  private def arg2[L,R,A,B](right: (A, A) => Either[L, B => Either[L,R]], left: L)(as: Seq[A])
  : Either[L, B => Either[L, R]]
  = Either.cond( as.length == 2, right(as.head, as(1)), left).joinRight

  private def arg3[L,R,A,B](right: (A, A, A) => Either[L, B => Either[L,R]], left: L)(as: Seq[A])
  : Either[L, B => Either[L, R]]
  = Either.cond( as.length == 3, right(as.head, as(1), as(2)), left).joinRight

  val userFunctionMaker
  : Map[String, Seq[Literal] => Either[String, JValue => Either[String, JValue]]]
  = Map (
    "trim"       -> arg0(_trim,     "trim() arg not empty"),
    "_trim"      -> arg0(_trim,     "_trim() arg not empty"),

    "toLower"    -> arg0(_toLower,  "toLower() arg not empty"),
    "_toLower"   -> arg0(_toLower,  "_toLower() arg not empty"),

    "toUpper"    -> arg0(_toUpper,  "toUppder() arg not empty"),
    "_toUpper"   -> arg0(_toUpper,  "_toUppder() arg not empty"),

    "toLong"     -> arg0(_toLong,   "toLong() arg not empty"),
    "_toLong"    -> arg0(_toLong,   "_toLong() arg not empty"),

    "toDouble"   -> arg0(_toDouble, "toDouble() arg not empty"),
    "_toDouble"  -> arg0(_toDouble, "_toDouble() arg not empty"),

    "toLongOr"   -> arg1( _toLongOr, "toLongOr(..) #arg not 1"),
    "_toLongOr"  -> arg1( _toLongOr, "_toLongOr(..) #arg not 1"),

    "toDoubleOr" -> arg1( _toDoubleOr, "toDoubleOr(..) #arg not 1"),
    "_toDoubleOr"-> arg1( _toDoubleOr, "_toDoubleOr(..) #arg not 1"),

    "toLongSep"   -> arg1( _toLongSep, "toLongSep(..) #arg not 1"),
    "_toLongSep"  -> arg1( _toLongSep, "_toLongSep(..) #arg not 1"),

    "toDoubleSep" -> arg1( _toDoubleSep, "toDoubleSep(..) #arg not 1"),
    "_toDoubleSep"-> arg1( _toDoubleSep, "_toDoubleSep(..) #arg not 1"),

    "toLongSepOr"   -> arg2( _toLongSepOr, "toLongSepOr(..) #arg not 2"),
    "_toLongSepOr"  -> arg2( _toLongSepOr, "_toLongSepOr(..) #arg not 2"),

    "toDoubleSepOr" -> arg2( _toDoubleSepOr, "toDoubleSepOr(..) #arg not 2"),
    "_toDoubleSepOr"-> arg2( _toDoubleSepOr, "_toDoubleSepOr(..) #arg not 2"),

    "subString"  -> arg2( _subString, "subString(..) #arg not 2"),
    "_subString" -> arg2( _subString, "_subString(..) #arg not 2"),

    "replace"    -> arg2( _replace, "replace(..) #arg not 2"),
    "_replace"   -> arg2( _replace, "_replace(..) #arg not 2"),

    "regex"      -> arg2( _regexReplace, "regex(..) #arg not 2"),
    "_regex"     -> arg2( _regexReplace, "_regex(..) #arg not 2"),
    "regexReplace"-> arg2( _regexReplace, "regexReplace(..) #arg not 2"),
    "_regexReplace"-> arg2( _regexReplace, "_regexReplace(..) #arg not 2"),
    "replaceAllIn"-> arg2( _regexReplace, "replaceAllIn(..) #arg not 2"),
    "_replaceAllIn"-> arg2( _regexReplace, "_replaceAllIn(..) #arg not 2"),

    "_replaceIf"-> arg3( _replaceIf, "_replaceIf(..) #arg not 3"),      // added
    "replaceIf"-> arg3( _replaceIf, "_replaceIf(..) #arg not 3"),      // added

    "_subStringBetween"-> arg3( _subStringBetween, "_subStringBetween(..) #arg not 3"),      // added
    "subStringBetween"-> arg3( _subStringBetween, "_subStringBetween(..) #arg not 3"),      // added

    "abs"        -> arg0( abs,      "abs() arg not empty"),

    "toString"   -> arg0( asString, "toString() arg not empty"),
    "asString"   -> arg0( asString, "asString() arg not empty"),

    "toStringOr"   -> arg1( _toStringOr, "toStringOr(..) #arg not 1"),    //added
    "_toStringOr"  -> arg1( _toStringOr, "_toStringOr(..) #arg not 1"),   //added
  )

}

