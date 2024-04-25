package validator.jsonConverter

import org.json4s.{JArray, JField, JNothing, JNull, JObject, JSet, JString, JValue}
import validator.jsonConverter.CVE2.{CVE2s, CVE20}
import validator.jsonConverter.TurnJson._
import validator.jsonConverter.syntaxJsonpath.rule.JsonPathAST.Query
import validator.jsonConverter.syntaxJsonpath.rule.JsonPathParser
import validator.jsonValidator.ToJson
import validator.utils.JsonUtil.{JValueWithPower, StringWithJsonPower}

import scala.io.Source.fromResource

////////////////////////////////////////
case class JPath(p: Query, s: String) extends ToJson {
  override def toJson: JValue = JString("JPath == " + s)

  def apply(root: JValue, jv: JValue)
  : Seq[JValue] = p.query(root)(jv)
}

object JPath {
  def apply(p: String) : Either[TurnSyntaxError, JPath] = {
    if( p.startsWith("$") || p.startsWith("@"))
      JsonPathParser.compile(p)
        .left.map(TSE)
        .map( JPath(_, p))
    else
      Left(TSE("not startwith @ or $"))
  }

}

////////////////////////////////////////
trait TurnSyntaxError
case class TSE(s: String) extends TurnSyntaxError
case class TSEs(s: List[TurnSyntaxError]) extends TurnSyntaxError

sealed trait ConvertError extends ToJson{
  def toJson = this match {
    case CVE(s)     => JString(s)
    case CVEa(s)    => JArray(s.map(_.toJson).toList)
    case CVEo(s)    => JObject(s.map(kv => kv._1 -> kv._2.toJson).toList)
  }
}
case class CVE(s: String) extends ConvertError
case class CVEa(s: Seq[ConvertError]) extends ConvertError
case class CVEo(s: Seq[(String, ConvertError)]) extends ConvertError

object CVE2 {
  def CVE20[T](p: String, s: String, l: Seq[T]) = if(l.isEmpty) List(p -> CVE(s)) else Nil
  def CVE2s(p: String, s: Seq[ConvertError]) = if(s.nonEmpty) List( p -> CVEa(s)) else Nil
}


case class IgnoreException(b: Boolean) {
  def apply() = b
}

////////////////////////////////////////
trait Convert {
  def convert(root: JValue, current: JValue)
             (implicit ignore: IgnoreException): Either[ConvertError, JValue]
}

sealed trait TurnRoot extends ToJson with Convert {

  override def toJson: JValue = this match {
    case RootArray(path, toComposite) => JObject(
      `[$$]` -> path.toJson,
      `[$>]` -> toComposite.toJson
    )
    case RootObject(path, toComposite) => JObject(
      `{$$}` -> path.toJson,
      `{$>}` -> toComposite.toJson
    )
  }

  override def convert(root: JValue, current: JValue)
                      (implicit ignore: IgnoreException)
  : Either[ConvertError, JValue] = this match {

    case RootArray(p, toComposite) =>
      val sub = p.apply(root, current)
      val (e0, l) = sub.map(s => toComposite.convert(s, s)).partitionMap(identity)

      val err0 = CVE2s(`[$>]`, e0)
      val err1 = CVE20(`[$$]`, s"${p.s} : ${sub.length} -> empty", l)
      val err = err1 ++ err0

      if( err.isEmpty || ignore())
        Right( JArray(l.toList))
      else
        Left( CVEo(err))

    case RootObject(path, toComposite) =>
      val sub = path.apply(root, current)

      sub.length match {
        case 1              => toComposite.convert(sub.head, sub.head)
        case n if !ignore() => Left(CVE(s"${`{$$}`} : too many($n)"))
        case _              =>
          val (_, l) = sub.map(s => toComposite.convert(s, s)).partitionMap(identity)
          Right( JArray(l.toList))

      }
  }
}

final case class RootArray( path: JPath, toComposite: ToComposite) extends TurnRoot
final case class RootObject( path: JPath, toComposite: ToComposite) extends TurnRoot

sealed trait MapTo extends ToJson with Convert {

  val `{}` = JObject(Nil)

  override def toJson: JValue = this match {
    case ToValue(valueOr) => valueOr.fold( _.toJson, identity)
    case composite: ToComposite => composite match {
      case ToArray(s, to) => JObject(
        `[::]` -> s.toJson,
        `[=>]` -> to.toJson
      )
      case ToObject(fields, selectedFields) =>

        val fs = fields.map( kv => kv._1 -> kv._2.toJson)
        val ss = selectedFields.map( kv => List(
          `{::}` -> kv._1.toJson,
          `{=>}` -> kv._2.toJson
        )).getOrElse(Nil) ++ fs

        JObject(ss)

    }
  }

  override def convert(root: JValue, current: JValue)
                      (implicit ignore: IgnoreException)
  : Either[ConvertError, JValue] = this match {

    case ToValue(valueOr) =>
      valueOr.fold(
        p => {
          val js = p.apply(root, current)
          js.length match {
            case 0 if !ignore() => Left( CVE( s"${p.s} : empty" ) )
            case 0 if ignore()  => Right( JNothing )
            case 1              => Right( js.head)
            case n if !ignore() => Left( CVE( js.map(_.values.toString).mkString( s"${p.s}: too many($n)[",",","]" ) ))
            case n              => Right( JString( js.map(_.values.toString).mkString( s"${p.s}: too many($n)[",",","]" ) ))
          }
        },
        Right(_)
      )

    case composite: ToComposite => composite match {

      case ToArray(path, toComposite) =>

        val sub = path(root, current)
        val (e0, l) = sub.map( s => toComposite.convert( root, s)).partitionMap(identity)

        if(ignore())
          Right( JArray( l.toList))
        else {

          val e1 = CVE20(`[::]`, s"${path.s} : ${sub.length} -> empty", l)
          val es = CVE2s(`[=>]`, e0)
          val err = e1 ++ es

          if( err.isEmpty || ignore())
            Right( JArray( l.toList))
          else
            Left( CVEo(err))
        }

      case ToObject(fields, selectedFields) =>

        val (e0, l0) = selectedFields.toList.map{ case (p, composite) =>
          val sub = p(root, current)
          val mapped = sub.length match {
            case 0 =>
              if(ignore()) Right(`{}`) else Left(CVE(s"${p.s} : ${sub.length} -> empty"))

            case 1 if !ignore() =>
              composite.convert(root, sub.head).fold(
                e => Left(e),
                j => j.asObject0().toRight( CVE(s"${p.s} : not object")) )

            case n if !ignore()=>
              Left( CVE(s"${p.s} : too many($n)"))

            case _ =>
              composite.convert(root, sub.head).fold(
                _ => Right(`{}`),
                j => j.asObject0().map(Right(_)).getOrElse(Right(`{}`)) )
          }
          mapped.map( _.obj)
        }.partitionMap(identity)

        val err0 = CVE2s(`{=>}`, e0)

        val (e1, l1) = fields.map( kv =>
          kv._2.convert(root, current)
            .map( kv._1 -> _)
            .left.map(  kv._1 -> _) ).partitionMap(identity)

        val err = err0 ++ e1

        if( err.isEmpty || ignore())
          Right(JObject( l0.flatten ++ l1))
        else
          Left(CVEo(err))

    }
  }

}

final case class ToValue(valueOr: Either[JPath, JValue]) extends MapTo

sealed trait ToComposite extends MapTo
final case class ToArray(arraySelector: JPath, toComposite: ToComposite) extends ToComposite
final case class ToObject( fields: List[(String, MapTo)], selectedFields: Option[(JPath, ToComposite)] ) extends ToComposite

///////////////////////////////////////////
object TurnJson {

  val `[$$]` = "[$$]"
  val `[$>]` = "[$>]"

  val `{$$}` = "{$$}"
  val `{$>}` = "{$>}"

  val `{::}` = "{::}"
  val `{=>}` = "{=>}"

  val `[::]` = "[::]"
  val `[=>]`  = "[=>]"

  val keyReserved   = Set(
    `[$$]`   ,
    `[$>]` ,
    `{$$}`  ,
    `{$>}`,
    `{::}`    ,
    `{=>}` ,
    `[::]`     ,
    `[=>]`  ,
  )

  private val err0 = TSE(s"Rule is not json-object")

  def apply(jv: JValue): Either[TurnSyntaxError, TurnRoot]
  = jv.asObject0().toRight(err0).flatMap( o => TurnRoot(o))

  //// Some Util
  def both[A,B]( oa: Option[A], ob: Option[B]): Option[(A, B)]
  = oa.flatMap( a => ob.map( a -> _))

  def stringAndObject(jo: JObject, pathKey: String, objKey: String)
  : Option[(String, JObject)]
  = {
    val pa = (jo \ pathKey).asString0()
    val ma = (jo \ objKey).asObject0()
    both(pa, ma)
  }

  // later: may need more generic version of sequence
  def swap[A,B]( oe: Option[Either[A,B]]): Either[A, Option[B]] = {
    oe.map( _.map(Option(_))).getOrElse(Right(None))
  }

  ///////////////////////////////////////////
}

object TurnRoot {

  private val err0 = TSE(s"(${`[$$]`}, ${`[$>]`}) or (${`{$$}`}, ${`{$>}`}) must exist")
  private val err1 = TSE(s"Only one of (${`[$$]`}, ${`[$>]`}) or (${`{$$}`}, ${`{$>}`}) is allowed.")

  def apply(jo: JObject): Either[TurnSyntaxError, TurnRoot]
  = {
    val arr = stringAndObject(jo, `[$$]`, `[$>]`)
    val obj = stringAndObject(jo, `{$$}`, `{$>}`)

    val ret = (arr, obj) match {
      case (None, None)       => Left( err0)
      case (Some(_), Some(_)) => Left( err1)
      case (Some((p, o)), None) => JPath(p).flatMap( pa => ToComposite.apply(o).map( to => RootArray(pa, to)))
      case (None, Some((p, o))) => JPath(p).flatMap( pa => ToComposite.apply(o).map( to => RootObject(pa, to)))
    }
    ret
  }
}

object ToValue {

  private def err0(st: String) = TSE(s"not allowed : $st are not allowed in ToValue")

  def apply(jv: JValue): Either[TurnSyntaxError, ToValue]
  = jv match {
    case JObject(_)   => Left(err0("JObject"))
    case JArray(_)    => Left(err0("JArray"))
    case JSet(_)      => Left(err0("JSet"))
    case o@JString(s) => Right(ToValue( JPath(s).toOption.toLeft(o)))
    case o            => Right(ToValue( Right(o)))
  }
}

object MapTo {

  private val err0 = TSE(s"not allowed : JArray are not allowed in MapTo field")

  def apply(jv: JValue): Either[TurnSyntaxError, MapTo]
  = jv match {
    case JArray(_) => Left( err0)
    case obj@JObject(_) => ToComposite.apply(obj)
    case other => ToValue.apply(other)
  }

}

object ToComposite {

  private val err0 = TSE(s"(${`[$$]`}, ${`[$>]`}) or (${`{$$}`}, ${`{$>}`}) must exist")
  private val err1 = TSE(s"Only one of (${`[$$]`}, ${`[$>]`}) or (${`{$$}`}, ${`{$>}`}) is allowed.")
  private val err2 = TSE(s"When ${`[::]`} is defined, normal-fields are not allowed.")

  private def jPathAndComposite( so: (String, JObject))
  : Either[TurnSyntaxError, (JPath, ToComposite)]
  = {
    for {
      p <- JPath(so._1)
      c <- ToComposite(so._2)
    } yield p -> c
  }

  private def makeToArray( fields: List[(String, JValue)],
                           so: (String, JObject))
  : Either[TurnSyntaxError, ToComposite]
  = fields.isEmpty match {
      case true  => jPathAndComposite(so).map( pc => ToArray(pc._1, pc._2))
      case false => Left( err2)
    }

  private def makeToObject( fields: List[(String, JValue)],
                            so: Option[(String, JObject)] )
  : Either[TurnSyntaxError, ToComposite]
  = {

    val selected = swap( so.map(t => jPathAndComposite(t)) )
    val (lefts, rights) = fields.map( t => MapTo(t._2).map( t._1 -> _)).partitionMap(identity)
    val flds = if (lefts.nonEmpty) Left(TSEs(lefts)) else Right(rights)

    for {
      s <- selected
      fs <- flds
    } yield ToObject(fs, s)
  }


  def apply( jo: JObject) : Either[TurnSyntaxError, ToComposite]
  = {
    val fields = jo.obj.filter(kv => !keyReserved.contains(kv._1) )
    val arr = stringAndObject(jo, `[::]`, `[=>]`)
    val obj = stringAndObject(jo, `{::}`, `{=>}`)

    (arr, obj) match {
      case (Some(_), Some(_)) => Left(err1)
      case (None, Some(so))   => makeToObject(fields, Some(so))
      case (None, None)       => makeToObject(fields, None)
      case (Some(so), None)   => makeToArray(fields, so)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
object SpecTurnJson extends App {

  val path = "lemon\\convert.json"
  val jstr = fromResource(path).mkString

  ////////////////////////////////////////////////////////////////////////////////
  val jv = jstr.toJValue
//  println(jv)
  val ret = TurnJson(jv)
//  println(ret)
//  ret.foreach(_.show())


  ////////////////////////////////////////////////////////////////////////////////
  val path0 = "lemon\\from.json"
  val jstr0 = fromResource(path0).mkString
  val jv0 = jstr0.toJValue
  //////////////////////////////////////////////////////////////////////////////
  val tj = ret.toOption.get
  implicit val i: IgnoreException = IgnoreException(false)
  val ret0 = tj.convert(jv0, jv0)

  println( ret0.fold( _.pretty, _.pretty))

}