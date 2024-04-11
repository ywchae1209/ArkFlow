package validator.jsonValidator

import org.json4s.{JArray, JField, JNull, JObject, JString, JValue}
import validator.utils.JsonUtil.{JValueWithPower, StringWithJsonPower}
import validator.utils.StringUtil.{show, wordy}
import validator.jsonValidator.ExprError.StringWithExprErrorPower
import validator.jsonValidator.FormatObject.{objectSyntaxKey, optionDefault}
import validator.jsonValidator.syntax.{SyntaxObject, SyntaxValue}

////////////////////////////////////////////////////////////////////////////////
sealed trait FormatJson extends ToJson {
  def toJson: JValue
  def evaluate(j: JValue): Either[Fails, JValue]
  def ifNotExist(key: String) : Either[Fails, JValue] = KeyNotExist(key)
}

object FormatJson {

  def apply(s: String,
            sepLine: String = "\n",
            lineComment: String = "###")
  : Either[SyntaxError, FormatJson] = {

    val lines = s.split(sepLine).flatMap(_.split(lineComment).headOption)

    val jstr = lines.mkString(sepLine)

    wordy( "===== un-commented rule =====\n" + jstr + "\n=============================\n" )

    val jv = jstr.toJValueOr.left.map( ExprError(_))

    jv.flatMap(toFormatJson)
  }

  def toFormatJson( jv: JValue) : Either[SyntaxError, FormatJson] = {
    jv match {
      case jo@JObject(_) => FormatObject(jo)
      case ja@JArray(_) => FormatArray(ja)
      case other => Left( ExprError(s"not allowed format :: $other"))
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
case class FormatValue( syntax: List[SyntaxValue]= Nil ) extends FormatJson {

  override def toString: String = syntax.mkString("[$ ", ", ", " $]")

  override def evaluate(j: JValue): Either[Fails, JValue] = {
    if (syntax.isEmpty)
      Right(j)
    else {
      j match {
        case JArray(_) => TypeError(j, "must be Value, but Array")
        case JObject(_) => TypeError(j, "must be Value, but Object")
        case _ =>
          val (lefts, rights) = syntax.map(_.evaluate(j)).partitionMap(identity)

          if(rights.isEmpty)
            ValueError(lefts:_*)    // todo
          else if(rights.contains(true))
            Right(j)
          else
            Left( EvalFalse( s"$j", syntax.mkString(s"(", ",", ") == false") ))
      }
    }
  }

  override def toJson: JValue = JObject(
    "FormatValue" -> JArray( syntax.map(s => JString(s.toString)))
  )
}

object FormatValue {

  def apply( expr: String): Either[SyntaxError, FormatValue] = {
    SyntaxValue(expr).map(s => FormatValue(List(s)))
  }

}

////////////////////////////////////////////////////////////////////////////////
case class FormatArray(format: FormatJson,
                       syntaxArray: Option[SyntaxArray] = None ) extends FormatJson {

  override def toString: String = s"[\n$format\n]"    // todo :: syntax Array

  override def evaluate(j: JValue)
  : Either[Fails, JValue] = {

    val ret = j.getArrayValues().fold(
      s => TypeError[JValue](j, s"must be JArray"),
      l => {
        val (lefts, rights) = l.map( item => format.evaluate(item)).partitionMap(identity)
        if(lefts.nonEmpty)
          ArrayError( lefts:_*)
        else
          Right( JArray( rights))
      })
    ret
  }

  override def toJson: JValue = JArray( List(format.toJson))

}

object FormatArray {

  def apply(jv: JArray): Either[SyntaxError, FormatArray] = {

    val arr = jv.children

    if(arr.length != 1) {
      s"syntax error :: array must have a single-element :: 1 != ${arr.length}".exprError
    } else {

      val h = arr.head

      FormatJson.toFormatJson(h)
        .left.map( e => SyntaxErrorArray(List(e)))
        .map( f => FormatArray(f, None))
    }
  }

}

////////////////////////////////////////////////////////////////////////////////
case class FormatObject(fields: Map[String, FormatJson],
                        syntax: Option[(SyntaxObject, Boolean)] = None) extends FormatJson {

  private val syntaxObject = syntax.map(_._1)
  private val syntaxObjectAsFloat = syntax.exists(_._2)

  private val syntaxString = syntaxObject.map( _.toString).getOrElse("")
  private val objectSyntaxFalse = ObjectSyntaxFalse( syntaxString)

  override def toString: String = fields.mkString("{\n", ",\n", "}\n")

  private def evaluateSynaxObject(j: JValue)
  : Either[ObjectSyntaxError, Boolean] = {
    syntaxObject.map( _.checkWith(j, syntaxObjectAsFloat)
      .left.map( e => ObjectSyntaxError(e))).getOrElse(Right(true))
  }

  private def extractFields( l: List[(String, JValue)])
  : Either[ObjectError, List[JField]] = {

    val checks = fields.map { case (k, f) =>

      val r =
        l.find(_._1 == k)
          .map(kv => f.evaluate(kv._2))          // extract
          .getOrElse( f.ifNotExist(k) )         // for Optional Field
          .fold(
            fs => FieldError(k -> fs),
            jv => Right( k -> jv)
          )
      r
    }

    val (lefts, rights) = checks.partitionMap(identity)

    if(lefts.nonEmpty)
      ObjectError( lefts.toList:_*)
    else
      Right( rights.toList)
  }


  override def evaluate(j: JValue)
  : Either[Fails, JObject] = {

    val ret = j.getObjectValues() match {
      case Left(_)  => TypeError[JObject](j, s"must be JObject")

      case Right(l) =>

        val sf = evaluateSynaxObject(j)
        val ef = extractFields(l)

        (sf, ef) match {
          case (Left(a), Left(b))      => Left( new ObjectError( a+:b.es))
          case (Left(a), _)            => Left( new ObjectError( List(a)))
          case (Right(false), Left(b)) => Left( new ObjectError( objectSyntaxFalse +: b.es ))
          case (Right(false), _)       => Left( new ObjectError( List(objectSyntaxFalse) ))
          case (Right(true), Left(b))  => Left(b)
          case (Right(true), Right(b)) => Right(JObject(b))
        }
    }
    ret
  }

  override def toJson: JValue = JObject(
    syntaxObject.map( s => objectSyntaxKey -> JObject("FormatObject" -> JString(s.toString))).toList ++
      fields.view.mapValues( _.toJson)
  )
}

object FormatObject {

  val empty: JObject = JObject()

  val objectSyntaxKey = "$$$$"

  private val optionPrefix = "_?_:"
  private val optionPrefixLen = optionPrefix.length

  val optionDefault: JNull.type = JNull

  private def mayOptionalField(k: String, f: FormatJson)
  : (String, FormatJson) = {
    if( k.startsWith(optionPrefix))
      k.drop(optionPrefixLen) -> FormatOptional(f)
    else
      k -> f
  }

  def apply(jv: JObject): Either[SyntaxError, FormatObject] = {

    val (err, syntax) = (jv \ objectSyntaxKey)    // todo :: need to ba array ?
      .getString()
      .toOption
      .map{ s =>
        val r = SyntaxObject(s)
        r.left.foreach(show)
        r
      }.partitionMap(identity)

    val err0 = err.map(objectSyntaxKey ->_).toList

    val (lefts, rights) = jv.obj
      .filterNot(kv => kv._1 == objectSyntaxKey)
      .map{
        case (k, JString(exp)) => FormatValue(exp).map( f => mayOptionalField(k, f)).left.map( k -> _)
        case (k, jo@JObject(_)) => FormatObject(jo).map( f => mayOptionalField(k, f)).left.map( k -> _)
        case (k, ja@JArray(_)) => FormatArray(ja).map(  f => mayOptionalField(k, f)).left.map( k -> _)
        case (k, o) => Left(ExprError(s"not allowed in object : $o")).left.map( k -> _)
      }.partitionMap(identity)

    if( lefts.nonEmpty) Left(SyntaxErrorObject(err0 ++ lefts))
    else
      Right(new FormatObject(rights.toMap, syntax.headOption.map(_ -> false)))
  }
}

////////////////////////////////////////////////////////////////////////////////
case class FormatPred(syntax: JValue => Either[Fails, JValue]) extends FormatJson {
  override def evaluate(j: JValue): Either[Fails, JValue] = syntax(j)

  override def toJson: JValue = JString(this.toString)
}

////////////////////////////////////////////////////////////////////////////////
case class FormatOptional( format: FormatJson) extends FormatJson {
  override def evaluate(j: JValue): Either[Fails, JValue] = {
    format.evaluate(j).orElse(Right(j))
  }

  override def ifNotExist(key: String): Either[Fails, JValue] = {
    wordy( s"INFO [ Optional ] key-not-exist-but-ok( $key )")
    Right(optionDefault)
  }
  override def toJson: JValue = JObject( "Optional" -> format.toJson )
}

////////////////////////////////////////////////////////////////////////////////
// syntax for object
trait SyntaxArray
