package transform.jsonConverter

import org.json4s.{JArray, JNothing, JObject, JSet, JString, JValue}
import transform.jsonConverter.Fails.CVE2.{CVE20, CVE2s}
import transform.jsonConverter.Fails._
import transform.jsonConverter.TurnJson._
import transform.jsonConverter.syntaxJsonpath.rule.JsonPathAST.Query
import transform.jsonConverter.syntaxJsonpath.rule.JsonPathParser
import transform.traits.ToJson
import transform.utils.JsonUtil.{JValueWithPower, StringWithJsonPower}

import scala.io.Source.fromResource

// for implicit conf.
////////////////////////////////////////////////////////////////////////////////
final case class IgnoreException(b: Boolean) {
  def apply() = b
}

////////////////////////////////////////////////////////////////////////////////
trait Convert {
  def convert(root: JValue, current: JValue)
             (implicit ignore: IgnoreException)
  : Either[ConvertError, JValue]
}

////////////////////////////////////////////////////////////////////////////////
final case class JPath(p: Query, s: String) extends ToJson {

  override def toJson: JValue = JString(s"JPath($s)=AST(${p.pretty})")

  def apply(root: JValue, jv: JValue): Seq[JValue] = p.query(root)(jv)
}

object JPath {

  def apply(p: String): Either[RuleSyntaxError, JPath] = {
    if( p.startsWith("$") || p.startsWith("@"))
      JsonPathParser.compile(p)
        .left.map(RSE)
        .map( JPath(_, p))
    else
      Left(RSE("not json-path"))
  }
}

////////////////////////////////////////////////////////////////////////////////

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
}
final case class RootArray( path: JPath, toComposite: ToComposite) extends TurnRoot {

  override def convert(root: JValue, current: JValue)
                      (implicit ignore: IgnoreException)
  : Either[ConvertError, JValue] = {

    val sub = path.apply(root, current)
    val (e0, l) = sub.map(s => toComposite.convert(s, s)).partitionMap(identity)

    val err0 = CVE2s(`[$>]`, e0)
    val err1 = CVE20(`[$$]`, s"${path.s} : ${sub.length} -> empty", l)
    val err = err1 ++ err0

    if( err.isEmpty || ignore())
      Right( JArray(l.toList))
    else
      Left( CVEo(err))
  }

}
final case class RootObject( path: JPath, toComposite: ToComposite) extends TurnRoot {

  override def convert(root: JValue, current: JValue)
                      (implicit ignore: IgnoreException)
  : Either[ConvertError, JValue] = {

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

sealed trait MapTo extends ToJson with Convert

final case class ToValue(valueOr: Either[JPath, JValue]) extends MapTo {

  override def toJson: JValue = valueOr.fold(_.toJson, identity)

  override def convert(root: JValue, current: JValue)
                      (implicit ignore: IgnoreException)
  : Either[ConvertError, JValue] = {

    valueOr.fold(
      p => {
        val js = p.apply(root, current)
        js.length match {
          case 0 if !ignore() => Left(CVE(s"${p.s}: not found"))
          case 0 if ignore() => Right(JNothing)
          case 1 => Right(js.head)
          case n if !ignore() => Left(CVE(js.map(_.values.toString).mkString(s"${p.s}: too many($n)[", ",", "]")))
          case n => Right(JString(js.map(_.values.toString).mkString(s"${p.s}: too many($n)[", ",", "]")))
        }
      },
      Right(_)
    )
  }

}

sealed trait ToComposite extends MapTo {

  override def toJson: JValue = this match {
    case ToArray(s, to) => JObject(
      `[::]` -> s.toJson,
      `[=>]` -> to.toJson
    )
    case ToObject(fields, selectedFields) =>

      val fs = fields.map(kv => kv._1 -> kv._2.toJson)
      val ss = selectedFields.map(kv => List(
        `{::}` -> kv._1.toJson,
        `{=>}` -> kv._2.toJson
      )).getOrElse(Nil) ++ fs

      JObject(ss)
  }

}
final case class ToArray(path: JPath, toComposite: ToComposite) extends ToComposite {

  override def convert(root: JValue, current: JValue)
                      (implicit ignore: IgnoreException)
  : Either[ConvertError, JValue] = {

    val sub = path(root, current)
    val (e0, l) = sub.map(s => toComposite.convert(root, s)).partitionMap(identity)

    if (ignore())
      Right(JArray(l.toList))
    else {

      val e1 = CVE20(`[::]`, s"${path.s} : ${sub.length} -> empty", l)
      val es = CVE2s(`[=>]`, e0)
      val err = e1 ++ es

      if (err.isEmpty || ignore())
        Right(JArray(l.toList))
      else
        Left(CVEo(err))
    }
  }
}
final case class ToObject(fields: List[(String, MapTo)],
                          opt: Option[(JPath, ToComposite)] ) extends ToComposite {

  override def convert(root: JValue, current: JValue)
                      (implicit ignore: IgnoreException)
  : Either[ConvertError, JValue] = {

    val (e0, l0) = opt.toList.map{ case (p, composite) =>
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

///////////////////////////////////////////
object TurnJson {

  val `{}` = JObject(Nil)

  val `[$$]` = "[$$]"
  val `[$>]` = "[$>]"

  val `{$$}` = "{$$}"
  val `{$>}` = "{$>}"

  val `{::}` = "{::}"
  val `{=>}` = "{=>}"

  val `[::]` = "[::]"
  val `[=>]`  = "[=>]"

  val keyReserved   = Set( `[$$]`, `[$>]`, `{$$}`, `{$>}`, `{::}`, `{=>}`, `[::]`, `[=>]`)

  private val err0 = RSE(s"Rule is not json-object")

  def apply(jv: JValue): Either[RuleSyntaxError, TurnRoot]
  = jv.asObject0().toRight(err0).flatMap( o => TurnRoot(o))

  // Some Util
  ////////////////////////////////////////////////////////////////////////////////
  def both[A,B]( oa: Option[A], ob: Option[B]): Option[(A, B)]
  = oa.flatMap( a => ob.map( a -> _))

  def stringAndObject( jo: JObject, pathKey: String, objKey: String)
  : Either[RuleSyntaxError,Option[(String, JObject)]]
  = {
    val pa = (jo \ pathKey).asString0()
    val ma = (jo \ objKey).asObject0()

    if( 1 == ( pa.toList.size + ma.toList.size))
      Left(RSE( "pair-key not exist :" +
        pa.map(_ => pathKey).getOrElse("") +
        ma.map(_ => objKey).getOrElse("")))
    else
      Right(both(pa, ma))
  }

  def compositeSub( key: (String, String), sobj: (String, JObject) )
  : Either[List[(String, RuleSyntaxError)], (JPath, ToComposite)] = {

    val p = JPath(sobj._1).left.map( key._1 -> _)
    val c = ToComposite(sobj._2).left.map( key._2 -> _)

    (p, c) match {
      case (Right(path), Right(composite)) => Right( path -> composite)
      case _ => Left(p.fold( List(_), _ => Nil) ++ c.fold(List(_), _ => Nil))
    }
  }

  // later: may need more generic version of sequence
  def swap[A,B]( oe: Option[Either[A,B]]): Either[A, Option[B]] = {
    oe.map( _.map(Option(_))).getOrElse(Right(None))
  }
}

object TurnRoot {

  private val err0 = RSE(s"(${`[$$]`}, ${`[$>]`}) or (${`{$$}`}, ${`{$>}`}) must exist")
  private val err1 = RSE(s"Only one of (${`[$$]`}, ${`[$>]`}) or (${`{$$}`}, ${`{$>}`}) is allowed.")

  def apply(jo: JObject): Either[RuleSyntaxError, TurnRoot]
  = {

    val arr = stringAndObject(jo, `[$$]`, `[$>]`)
    val obj = stringAndObject(jo, `{$$}`, `{$>}`)

    def go( a: Option[(String, JObject)], o: Option[(String, JObject)] )
    : Either[RuleSyntaxError, TurnRoot] = {
      (a, o) match {
          case (None, None)       => Left( err0)
          case (Some(_), Some(_)) => Left( err1)
          case (Some((p, o)), None) =>
            compositeSub(`[$$]`-> `[$>]`, p -> o)
              .map( pc => RootArray(pc._1, pc._2))
              .left.map( RSEo )

          case (None, Some((p, o))) =>
            compositeSub(`{$$}`-> `{$>}`, p -> o)
              .map( pc => RootObject(pc._1, pc._2))
              .left.map( RSEo )
        }
    }

    val ret = for{
      a <- arr
      o <- obj
      r <- go(a, o)
    } yield r

    ret
  }
}

object ToValue {

  private def err0(st: String) = RSE(s"not allowed : $st are not allowed in ToValue")

  private def st( s: String): Either[RuleSyntaxError, ToValue] =
    s.headOption match {
      case Some('@') | Some('$')=> JPath(s).map(p => ToValue(Left(p)))
      case Some('\\') => Right( ToValue( Right(JString(s.tail))))
      case o => Right(ToValue(Right(JString(s))))
    }

  def apply(jv: JValue): Either[RuleSyntaxError, ToValue]
  = jv match {
    case JObject(_)   => Left(err0("JObject"))
    case JArray(_)    => Left(err0("JArray"))
    case JSet(_)      => Left(err0("JSet"))
    case JString(s)   => st(s)
    case o            => Right(ToValue( Right(o)))
  }
}

object MapTo {

  private val err0 = RSE(s"not allowed : JArray are not allowed in MapTo field")

  def apply(jv: JValue): Either[RuleSyntaxError, MapTo]
  = jv match {
    case JArray(_) => Left( err0)
    case obj@JObject(_) => ToComposite.apply(obj)
    case other => ToValue.apply(other)
  }
}

object ToComposite {

  private val err0 = RSE(s"(${`[$$]`}, ${`[$>]`}) or (${`{$$}`}, ${`{$>}`}) must exist")
  private val err1 = RSE(s"Only one of (${`[$$]`}, ${`[$>]`}) or (${`{$$}`}, ${`{$>}`}) is allowed.")
  private val err2 = RSE(s"When ${`[::]`} is defined, normal-fields are not allowed.")

  private def makeToArray( fields: List[(String, JValue)],
                           so: (String, JObject))
  : Either[RuleSyntaxError, ToComposite]
  = fields.isEmpty match {
      case true  =>
        compositeSub((`[::]` -> `[=>]`), so).fold(
          e => Left(RSEo(e)),
          l => Right(ToArray(l._1, l._2))
        )
      case false => Left( err2)
    }

  private def makeToObject( fields: List[(String, JValue)],
                            so: Option[(String, JObject)] )
  : Either[RuleSyntaxError, ToComposite]
  = {

    val so0 = so.map( compositeSub((`{::}` -> `{=>}`), _))
    val opt = swap( so0)

    val (lefts, rights) = fields.map( t =>
      MapTo(t._2)
        .left.map( t._1 -> _)
        .map( t._1 -> _)).partitionMap(identity)

    opt.fold(
      t =>  Left(RSEo(t ++ lefts)),
      o => if(lefts.isEmpty) Right(ToObject(rights, o)) else Left(RSEo(lefts))
    )
  }


  // construct ToComposite
  ////////////////////////////////////////////////////////////////////////////////
  def apply( jo: JObject) : Either[RuleSyntaxError, ToComposite]
  = {
    val fields = jo.obj.filter(kv => !keyReserved.contains(kv._1) )

    val arr = stringAndObject(jo, `[::]`, `[=>]`)
    val obj = stringAndObject(jo, `{::}`, `{=>}`)

    def go( a: Option[(String, JObject)], o: Option[(String, JObject)] )
    = {
      val r = (a, o) match {
        case (Some(_), Some(_)) => Left(err1)
        case (Some(so), None) => makeToArray(fields, so)
        case (None, so)       => makeToObject(fields, so)
      }
      r
    }

    for{
      a <- arr
      o <- obj
      r <- go(a, o)
    } yield r
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
object SpecTurnJson extends App {

  val jstr = fromResource("lemon\\convert.json").mkString

  ////////////////////////////////////////////////////////////////////////////////
  val jv = jstr.toJValue
//  println(jv)
  val ret = TurnJson(jv)
  println(ret.fold( _.pretty, _.pretty))
//  ret.foreach(_.show())


  ////////////////////////////////////////////////////////////////////////////////
//  val path0 = "lemon\\from.json"
//  val jstr0 = fromResource(path0).mkString
//  val jv0 = jstr0.toJValue
//  //////////////////////////////////////////////////////////////////////////////
//  val tj = ret.toOption.get
//  implicit val i: IgnoreException = IgnoreException(false)
//  val ret0 = tj.convert(jv0, jv0)
//
//  println( ret0.fold( _.pretty, _.pretty))

}