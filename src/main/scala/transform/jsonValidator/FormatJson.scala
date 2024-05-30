package transform.jsonValidator

import org.json4s.{JArray, JField, JNull, JObject, JString, JValue}
import transform.jsonValidator.FormatObject.{objectSyntaxKey, optionDefault}
import transform.jsonValidator.syntaxObject.SyntaxObject
import transform.jsonValidator.syntaxValue.SyntaxValue
import transform.common.ToJson
import transform.utils.JsonUtil.{JValueWithPower, StringWithJsonPower}
import transform.utils.StringUtil.{show, wordy}

////////////////////////////////////////////////////////////////////////////////

/**
 * {{{
 * Json Validation Common Interface
 *
 * == Overview ==
 *
 * FormatJson has (almost) same structure of target-json.
 *
 * == Usage ==
 *
 * 1. create
 *    val fj: FormatJson = FormatJson.apply( ruleString)
 *
 * 2. validate Json
 *    val result: Either[Fails, JValue] = fj.evaluate(targetJson)
 *
 * == sub-classes ==
 *
 * [ Main sub-class ]
 *  FormatObject  :: Inspect a Json Object.
 *  FormatValue   :: Inspect a Json Value
 *  FormatArray   :: Inspect a Json Array
 *
 * [ Supplementary sub-class]
 *  FormatOptional :: helper for optional Json Field
 *  }}}
 *  [[FormatObject]] [[FormatValue]]  [[FormatArray]] [[FormatOptional]]
 */
sealed trait FormatJson extends ToJson {

  /**
   * {{{
   * (recursively) EvaluateJson with correspond rule.
   * note1) FormatJson has same (recursive-tree) structure with target Json.
   * note2) FormatObject has extra rule ( SyntaxObject )
   * }}}
   * @param [[JValue]]
   * @return [[Fails]]
   */
  def evaluate(j: JValue): Either[Fails, JValue]

  /**
   * {{{
   * used for Optional Json field.
   * If not FormatJson, return KeyNotExist( kind of Fails)
   * }}}
   *
   * [[KeyNotExist]] [[FormatJson]]
   */
  def ifNotExist(key: String) : Either[Fails, JValue] = KeyNotExist(key)
}

object FormatJson {

  /**
   *
   * @param json :: Rule string written in Json.
   * @param sepLine :: Line-separator of Json
   * @param lineComment :: End-line comment marker. (default : ###)
   * @return [[SyntaxError]] [[FormatJson]]
   */
  def apply(json: String,
            sepLine: String = "\n",
            lineComment: String = "###")
  : Either[SyntaxError, FormatJson] = {

    val lines = json.split(sepLine).flatMap(_.split(lineComment).headOption)

    val jstr = lines.mkString(sepLine)

    wordy( "===== un-commented rule =====\n" + jstr + "\n=============================\n" )

    val jv = jstr.toJValueOr.left.map( InvalidRuleJsonError(_) )

    jv.flatMap(toFormatJson)
  }

  /** {{{
   * Map rule(json node) to FormatJson
   * This function is called at json-root-node, or json-array-node.
   * }}}
   * @param jv Validation rule-node. Children must be JObject or JArray.
   * @return [[SyntaxError]] [[FormatJson]]
   */
  def toFormatJson( jv: JValue) : Either[SyntaxError, FormatJson] = {
    jv match {

      case jo@JObject(_) => FormatObject(jo)
      case ja@JArray(_) => FormatArray(ja)
      case other => Left( InvalidRuleNodeError( s"not Object/Array: $other"))
    }
  }
}

/**
 * Json-value inspection rule holder .
 * @param syntax : List of [[SyntaxValue]]. Target json must satisfy at least one of these syntax.
 */
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

  /**
   * create FormatJson with given rule-string (internal function)
   * @param expr
   * @return
   */
  def apply( expr: String): Either[SyntaxError, FormatValue] = {
    SyntaxValue(expr).map(s => FormatValue(List(s)))
  }

}

/**
 * {{{
 * Json-Array inspection rule holder .
 * note1: FormatArray assume that each element of target json has same structure(FormatJson).
 * note2: So, FormatArray must have exactly 1-element. (zero or above 1 elements are not allowed)
 * }}}
 * @param format FormatJson which each element must confirm.
 * @param syntaxArray not used currently.
 */
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
      Left(InvalidRuleNodeError( s"Array must have a single-element :: 1 != ${arr.length}"))

    } else {

      val h = arr.head

      FormatJson.toFormatJson(h)
        .left.map( e => SyntaxErrorArrayNode(List(e)))
        .map( f => FormatArray(f, None))
    }
  }

}
////////////////////////////////////////////////////////////////////////////////
/**
 * @param fields : JObject's Field-condition must confirm
 * @param syntax : inter-field relationship for JObject must confirm.
 *               see: [[SyntaxObject]]
 */
case class FormatObject(fields: Map[String, FormatJson],
                        syntax: Option[(SyntaxObject, Boolean)] = None) extends FormatJson {

  private val syntaxObject = syntax.map(_._1)
  private val syntaxObjectAsFloat = syntax.exists(_._2)

  private val syntaxString = syntaxObject.map( _.toString).getOrElse("")

  private def objectSyntaxFalse(str: List[String]): ObjectSyntaxFalse = ObjectSyntaxFalse( syntaxString, str)

  override def toString: String = fields.mkString("{\n", ",\n", "}\n")

  private def evaluateSyntaxObject(j: JValue)
  : Either[Fails, Boolean] = {
    val ret =
      syntaxObject.map( so =>
        so.checkWith(j, syntaxObjectAsFloat)
          .left.map( e => ObjectSyntaxError(e))
          .flatMap( b => if(b) Right(b) else Left( objectSyntaxFalse( so.expressionWith(j))))
      )
      .getOrElse(Right(true))
    ret
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

        val sf = evaluateSyntaxObject(j)
        val ef = extractFields(l)

        (sf, ef) match {
          case (Left(a), Left(b))      => Left( new ObjectError( a+:b.es))
          case (Left(a), _)            => Left( new ObjectError( List(a)))
          case (Right(true), Left(b))  => Left(b)
          case (Right(true), Right(b)) => Right(JObject(b))
          case (Right(false), _)       =>
            // Right(false) must replaced with Left(ObjectSyntaxFalse)
            throw new Exception(s"this must not happen :: evaluate :: ${toString}")
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

  /// todo ::::
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
      .asString()
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
        case (k, o) => Left( InvalidRuleNodeError(s"not String/Object/Array:: $o")).left.map( k -> _)
      }.partitionMap(identity)

    if( lefts.nonEmpty || err0.nonEmpty) Left( SyntaxErrorObjectNode(err0 ++ lefts))
    else
      Right(new FormatObject(rights.toMap, syntax.headOption.map(_ -> false)))
  }
}

////////////////////////////////////////////////////////////////////////////////
case class FormatOptional( format: FormatJson) extends FormatJson {

  override def evaluate(j: JValue): Either[Fails, JValue] = {
    format.evaluate(j).orElse(Right(j))
  }

  override def ifNotExist(key: String): Either[Fails, JValue] = {
//    wordy( s"INFO [ Optional ] key-not-exist-but-ok( $key )")
    Right( optionDefault)
  }

  override def toJson: JValue = JObject( "Optional" -> format.toJson )
}

////////////////////////////////////////////////////////////////////////////////
// syntax for array
trait SyntaxArray
