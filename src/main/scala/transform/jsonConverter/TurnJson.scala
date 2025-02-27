package transform.jsonConverter

import org.json4s.{JArray, JObject, JSet, JString, JValue}
import transform.common.ToJson
import transform.jsonConverter.Fails._
import transform.jsonConverter.TurnJson._
import transform.jsonConverter.syntaxJsonpath.rule.JsonPathAST.{Query, QueryAndFunction}
import transform.jsonConverter.syntaxJsonpath.rule.JsonPathParser
import transform.utils.JsonUtil.JValueWithPower

////////////////////////////////////////////////////////////////////////////////
trait Convert {
  def convert(root: JValue, current: JValue)
  : Either[CauseOfDenial, JValue]
}

////////////////////////////////////////////////////////////////////////////////
final case class JPath(p: Query, s: String) extends ToJson {

  override def toJson : JValue
  = JString(s"JPath($s)=AST(${p.pretty})")

  def apply(root: JValue, jv: JValue): Seq[JValue]
  = p.query(root)(jv)
}

final case class JPathAndFunction(pf: QueryAndFunction, s: String) extends ToJson {

  override def toJson : JValue = JString(s"JPath(${s})=AST(${pf.pretty})")

  def apply(root: JValue, jv: JValue): Seq[JValue]
  = pf.query(root)(jv)

  def manipluate(jv: JValue): Either[COD, JValue] = {

    // todo :: g3nie
    println( pf.f.mkString( jv.values.toString + " :: ", ".", ""))
    pf.manipulate(jv)
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
final case class RootArray(path: JPath, toObj: ToObject) extends TurnRoot {

  override def convert(root: JValue, current: JValue)
  : Either[CauseOfDenial, JArray] = {

    val sub = path.apply(root, current)
    val (e0, l) = sub.map(s => toObj.convert(s, s)).partitionMap(identity)

    val err0 = COD2s_Nel(`[$>]`, e0)
    val err1 = COD2s_ifNone(l.headOption)(`[$$]` -> s"${path.s} : ${sub.length} -> empty")
    val err = err1 ++ err0

    Either.cond (err.isEmpty, JArray(l.toList), CODo(err) )
  }
}
final case class RootObject(path: JPath, toObj: ToObject) extends TurnRoot {

  override def convert(root: JValue, current: JValue)
  : Either[CauseOfDenial, JObject] = {

    val sub = path.apply(root, current)

    sub.length match {
      case 1 => toObj.convert(sub.head, sub.head)
      case 0 => Left( COD(s"${`{$$}`} : empty ") )
      case n => Left( COD(s"${`{$$}`} : too many($n)") )
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
sealed trait MapTo extends ToJson with Convert

// todo :: g3nie
final case class ToValue(valueOr: Either[JPathAndFunction, JValue]) extends MapTo {

  override def toJson: JValue = valueOr.fold(_.toJson, identity)

  override def convert(root: JValue, current: JValue)
  : Either[CauseOfDenial, JValue] = {

    valueOr.left.map( p => {

      val j = p.apply(root, current)
      def st(n: Int) = j.map(_.values.toString).mkString(s"${p.s}: too many($n)[", ",", "]")

      j.length match {
        case 1 => p.manipluate(j.head).left.map( s => COD(s"${p.s}: manipulate fail : $s"))
        case 0 => Left( COD(s"${p.s}: not found"))
        case n => Left( COD(st(n)))
      }
    } ).joinLeft
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
final case class ToArray(path: JPath, toObj: ToObject) extends ToComposite {

  override def convert(root: JValue, current: JValue)
  : Either[CauseOfDenial, JArray] = {

    val sub = path(root, current)
    val (e0, l) = sub.map(s => toObj.convert(root, s)).partitionMap(identity)

//    val e1 = COD2s_cond(l.isEmpty)(`[::]` -> s"${path.s} : ${sub.length} -> empty")
    val es = COD2s_Nel(`[=>]`, e0)
    val err = es //e1 ++ es

    Either.cond ( err.isEmpty,
      JArray(l.toList),
      CODo(err)
    )
  }
}
final case class ToObject(fields: List[(String, MapTo)],
                          opt: Option[(JPath, ToObject)] ) extends ToComposite {

  override def convert(root: JValue, current: JValue)
  : Either[CauseOfDenial, JObject] = {

    // fields
    val (e1, l1) =
      fields
        .map{ case (k, m) => m.convert(root, current).map( k -> _).left.map(  k -> _) }
        .partitionMap(identity)

    // opt. fields
    val (e2, l2) = opt.toList.map { case (p, c) =>

      val sub = p(root, current)
      val obj = sub.length match {
        case 1 => c.convert(root, sub.head)
        case 0 => Left( COD(s"${p.s} : ${sub.length} -> empty") )
        case n => Left( COD(s"${p.s} : too many($n)") )
      }
      obj.left.map(`{=>}`-> _).map( _.obj)

    }.partitionMap(identity)

    val err2 = e2 ++ e1

    Either.cond( err2.isEmpty,
      JObject( l2.flatten ++ l1),
      CODo(err2) )
  }
}

// Companion Objects
////////////////////////////////////////////////////////////////////////////////
object JPath {

  private val rse_not_json_path = RSE("not json-path")

  def apply(p: String): Either[RuleSyntaxError, JPath] = {
    if( p.startsWith("$") || p.startsWith("@"))
      JsonPathParser.compile(p)
        .left.map(RSE)
        .map( JPath(_, p))
    else
      Left(rse_not_json_path)
  }
}

object JPathAndFunction {

  private val rse_not_json_path = RSE("not json-path")

  def apply(p: String): Either[RuleSyntaxError, JPathAndFunction] = {
    if( p.startsWith("$") || p.startsWith("@"))
      JsonPathParser.compilePathAndFunction(p)
        .fold(
          es => Left(RSE(es)),
          qf => qf.functionSyntaxError.map(RSE).toLeft( JPathAndFunction(qf, p)) // extract syntax error
        )
    else
      Left(rse_not_json_path)
  }
}

object TurnJson {

  val `[$$]` = "[$$]"
  val `[$>]` = "[$>]"
  val `{$$}` = "{$$}"
  val `{$>}` = "{$>}"
  val `{::}` = "{::}"
  val `{=>}` = "{=>}"
  val `[::]` = "[::]"
  val `[=>]`  = "[=>]"
  val keyReserved   = Set( `[$$]`, `[$>]`, `{$$}`, `{$>}`, `{::}`, `{=>}`, `[::]`, `[=>]`)

  private val rse_not_json_format = RSE("not json-format")

  def apply(jv: JValue)
  : Either[RuleSyntaxError, TurnRoot]
  = jv.asObject0().toRight( rse_not_json_format).flatMap( o => TurnRoot(o))

  // Some Util
  ////////////////////////////////////////////////////////////////////////////////
  def both[A,B]( oa: Option[A], ob: Option[B]): Option[(A, B)]
  = oa.flatMap( a => ob.map( a -> _))

  def hasPathAndObj(jo: JObject, pathKey: String, objKey: String)
  : Either[RuleSyntaxError,Option[(String, JObject)]]
  = {
    val pa = (jo \ pathKey).asString0()
    val ma = (jo \ objKey).asObject0()

    Either.cond( 1 != (pa.toList.size + ma.toList.size),
      both(pa, ma),
      RSE( "pair-key not exist :" +
        pa.map(_ => pathKey).getOrElse("") +
        ma.map(_ => objKey).getOrElse(""))
    )
  }

  // later: may need more generic version of sequence
  def swap[A,B]( oe: Option[Either[A,B]]): Either[A, Option[B]] = {
    oe.map( _.map(Option(_))).getOrElse(Right(None))
  }
}

object TurnRoot {

  private val err0 = RSE(s"(${`[$$]`}, ${`[$>]`}) or (${`{$$}`}, ${`{$>}`}) must exist")
  private val err1 = RSE(s"Only one of (${`[$$]`}, ${`[$>]`}) or (${`{$$}`}, ${`{$>}`}) is allowed.")

  def apply(jo: JObject)
  : Either[RuleSyntaxError, TurnRoot]
  = {

    val arr = hasPathAndObj(jo, `[$$]`, `[$>]`)
    val obj = hasPathAndObj(jo, `{$$}`, `{$>}`)

    def go( a: Option[(String, JObject)], o: Option[(String, JObject)] )
    : Either[RuleSyntaxError, TurnRoot] = {

      (a, o) match {
          case (None, None)         => Left( err0)
          case (Some(_), Some(_))   => Left( err1)
          case (Some((p, o)), None) => RootArray.apply( p, o)
          case (None, Some((p, o))) => RootObject.apply( p, o)
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

object RootArray {

  def apply(p : String, o: JObject)
  : Either[RuleSyntaxError, RootArray] = {

    val p0 = JPath(p).left.map( `[$$]` -> _)
    val c0 = ToObject(o).left.map( `[$>]` -> _)

    val ra = for {
      p <- p0
      c <- c0
    } yield RootArray(p, c)

    ra.left.map( _ => RSEo(RSE2s_lefts( p0, c0)) )
  }
}

object RootObject {
  def apply(p : String, o: JObject)
  : Either[RuleSyntaxError, RootObject] = {

    val p0 = JPath(p).left.map( `{$$}` -> _)
    val c0 = ToObject(o).left.map( `{$>}` -> _)

    val ro = for{
      p <- p0
      c <- c0
    } yield RootObject (p, c)

    ro.left.map( _ => RSEo(RSE2s_lefts( p0, c0)) )
  }

}

object MapTo {

  private val err0 = RSE(s"not allowed : JArray are not allowed in MapTo field")

  def apply(jv: JValue): Either[RuleSyntaxError, MapTo]
  = jv match {
    case JArray(_)      => Left( err0)
    case obj@JObject(_) => ToComposite.apply(obj)
    case other          => ToValue.apply(other)
  }
}

object ToValue {

  private def st( s: String): Either[RuleSyntaxError, ToValue] =
    s.headOption match {
      case Some('@')
           | Some('$')=> JPathAndFunction.apply(s).map(p => ToValue(Left(p)))
      case Some('\\') => Right( ToValue( Right(JString(s.tail))))
      case _          => Right( ToValue( Right(JString(s))))
    }

  private def err0(st: String) = RSE(s"not allowed : $st are not allowed in ToValue")

  def apply(jv: JValue): Either[RuleSyntaxError, ToValue]
  = jv match {
    case JObject(_)   => Left(err0("JObject"))
    case JArray(_)    => Left(err0("JArray"))
    case JSet(_)      => Left(err0("JSet"))
    case JString(s)   => st(s)                            // jpath(and function)
    case o            => Right(ToValue.apply( Right(o)))  // literal
  }

}

object ToComposite {

  private val err0 = RSE(s"Only one of (${`[$$]`}, ${`[$>]`}) or (${`{$$}`}, ${`{$>}`}) is allowed.")
  private val err1 = RSE(s"When ${`[::]`} is defined, normal-fields are not allowed.")

  ////////////////////////////////////////////////////////////////////////////////
  def apply( jo: JObject)
  : Either[RuleSyntaxError, ToComposite] = {

    val fields = jo.obj.filter(kv => !keyReserved.contains(kv._1) )

    val arr = hasPathAndObj(jo, `[::]`, `[=>]`)
    val obj = hasPathAndObj(jo, `{::}`, `{=>}`)

    def go( a: Option[(String, JObject)],
            o: Option[(String, JObject)] ): Either[RuleSyntaxError, ToComposite]
    = (a, o) match {
      case (Some(_), Some(_)) => Left(err0)
      case (Some(so), None)   => Either.cond( fields.isEmpty, ToArray.make(so), err1 ).flatten
      case (None, so)         => ToObject.make(fields, so)
    }

    for{
      a <- arr
      o <- obj
      r <- go(a, o)
    } yield r
  }

}

object ToObject {

  def toObjectSub( so: (String, JObject),
                   keys: (String, String) = (`{::}`, `{=>}`) )
  : Either[List[(String, RuleSyntaxError)], (JPath, ToObject)] = {

    val p0 = JPath(so._1).left.map(  keys._1 -> _)
    val c0 = ToObject(so._2).left.map( keys._2 -> _)

    val po = p0.flatMap(p => c0.map(c => p -> c))

    po.left.map(_ => RSE2s_lefts(p0, c0))
  }

  ////////////////////////////////////////////////////////////
  def make( fields: List[(String, JValue)],
            opt: Option[(String, JObject)] )
  : Either[RuleSyntaxError, ToObject]
  = {
    val (e0, l0) =
      fields
        .map{ case( k, j) => MapTo(j) .left.map( k -> _) .map( k-> _)}
        .partitionMap(identity)

    val opt0 = swap( opt.map( toObjectSub(_) ))

    opt0.fold(
      t => Left(RSEo(t ++ e0)),
      o => Either.cond( e0.isEmpty, ToObject(l0, o), RSEo(e0))
    )
  }

  ////////////////////////////////////////////////////////////
  def apply( jo: JObject)
  : Either[RuleSyntaxError, ToObject] = {

    val fields = jo.obj.filter(kv => !keyReserved.contains(kv._1) )
    val obj = hasPathAndObj(jo, `{::}`, `{=>}`)

    obj.flatMap ( o => make(fields, o))
  }

}

object ToArray {

  private val err0 = RSE(s"${`{$$}`}, ${`{$>}`} must exist")

  def make(so: (String, JObject))
  : Either[RuleSyntaxError, ToArray]
  = ToObject.toObjectSub(so, `[::]` -> `[=>]`)
      .left.map(RSEo)
      .map(l => ToArray(l._1, l._2))

  ////////////////////////////////////////////////////////////
  def apply( jo: JObject)
  : Either[RuleSyntaxError, ToArray] = {

    val fields = jo.obj.filter(kv => !keyReserved.contains(kv._1) )
    val arr = hasPathAndObj(jo, `[::]`, `[=>]`)

    arr.flatMap( _.toRight( err0 ))
      .flatMap ( so => Either.cond( fields.isEmpty, ToArray.make(so), RSE("") ))
      .joinRight    // todo :: check
  }
}

