package transform.jsonConverter.syntaxJsonpath.rule

import org.json4s.JsonAST.JBool
import org.json4s.{JArray, JDecimal, JDouble, JInt, JLong, JNothing, JNull, JObject, JString, JValue}
import transform.utils.JsonUtil.JValueWithPower

import scala.util.matching.Regex

object JsonPathAST {

  ////////////////////////////////////////////////////////////////////////////////
  // AST
  ////////////////////////////////////////////////////////////////////////////////
  sealed trait Token {
    def pretty: String
  }

  sealed trait JPath extends Token
  {
    def query(root: JValue)(jv: JValue): LazyList[JValue] = this match {
      case position: Position => position match {
        case Root    => LazyList(root)
        case Current => LazyList(jv)
      }

      case selector: FieldSelector => selector match {
        case Field(name)          => jv \~ name
        case Fields(names)        => jv \~ (names: _*)
        case AllFields            => jv.\~~
        case RecursiveField(name) => jv \\~ name
        case RecursiveAll         => jv.\\~~    // todo :: need some consideration.
      }

      case selector: ArraySelector =>
        selector match {
          case Slice(start, end, step)=> jv.slice(start, end, step)
          case Random(index)          => jv.random(index)
          case AllItems               => jv.\~~
        }

      case predicate: PredicateSelector =>
        jv match {
          case JArray(l)  => l.filter( predicate.test(root)).to(LazyList)
          case JObject(l) => l.filter( f => predicate.test(root)(f._2)).map(_._2).to(LazyList)
          case _          => LazyList.empty
        }

      case RecursiveFilter(filter) => jv.\\~~.filter(filter.test(root))
    }
  }

  sealed trait Position extends JPath {
    override def pretty: String = this match {
      case Root => "$"
      case Current => "@"
    }
  }
  final case object Root    extends Position
  final case object Current extends Position

  sealed trait FieldSelector extends JPath {
    override def pretty: String = this match {
      case Field(name)          => "." + name
      case Fields(names)        => names.mkString("[",",","]")
      case AllFields            => ".*"
      case RecursiveField(name) => ".." + name
      case RecursiveAll   => "..*"
    }
  }
  final case class Field(name: String)          extends FieldSelector
  final case class Fields(names: List[String])  extends FieldSelector
  final case class RecursiveField(name: String) extends FieldSelector
  final case object AllFields                   extends FieldSelector
  final case object RecursiveAll                extends FieldSelector

  object FieldSelector {
    def recursiveField(s: String) = RecursiveField(s)
    def field( s: String ) = Field(s)
    def fields( s: Seq[String] ) = if(s.length == 1) Field(s.head) else Fields(s.toList)
  }

  //////////////////////////////////////////////////
  sealed trait ArraySelector extends JPath {
    private def m[T](t: Option[T]) = t.map(_.toString).getOrElse("")

    override def pretty: String = this match {
      case Slice(s, e, t) => s"[${m(s)}:${m(e)}:${t}]"
      case Random(index)  => index.mkString("[", ",", "]")
      case AllItems            => "[*]"
    }
  }
  final case class Slice(start: Option[Int], end: Option[Int], step: Int) extends ArraySelector
  final case class Random( index: Seq[Int]) extends ArraySelector
  final case object AllItems extends ArraySelector

  object Slice {
    def apply(slice: (Option[Int], Option[Option[Int]], Option[Option[Int]])): ArraySelector = {
      slice match {
        case (None, None, None) => AllItems
        case (start, end, step) => Slice(start, end.flatten, step.flatten.getOrElse(1))
      }
    }
  }

  //////////////////////////////////////////////////
  sealed trait FilterValue extends Token {

    def get(root: JValue)(jv: JValue): LazyList[JValue] = this match {
      case q: Query => q.query(root)(jv)
      case Literal(jv) => LazyList(jv)
    }

    override def pretty: String = this match {
      case Query(p, route) => s"${p.pretty}" + route.map(_.pretty).mkString
      case Literal(jv)     => jv.values.toString
    }
  }

  final case class Query(p: Position, route: List[JPath] ) extends FilterValue {

    def extract(jv: JValue) = query(jv)(jv)

    def query(root: JValue)(jv: JValue) : LazyList[JValue] = {

      val s = p.query(root)(jv)
      val ret = route.foldLeft(s)((b, a) => b.flatMap( a.query(root) ) )
      ret
    }

  }

  final case class Literal(jv: JValue) extends FilterValue

  object Literal {
    def long(s: String) = Literal(JLong(s.toLong))
    def double(s: String) = Literal(JDouble(s.toDouble))
    def string(s: String) = Literal(JString(s))
    def boolean(b: Boolean) = if(b) True else False

    val True  = Literal(JBool(true))
    val False = Literal(JBool(true))
    val Null  = Literal(JNull)
    val empty = Literal(JNothing)
  }

  //////////////////////////////////////////////////
  sealed trait PredicateSelector extends JPath {

    def test(root: JValue)(jv: JValue): Boolean = {

      val ret = this match {

          case MatchRegex(q, _, reg) => q.query(root)(jv).exists(_ =~ reg)
          case Contains(q)           => q.query(root)(jv).nonEmpty
          case Compare(lhs, op, rhs) => op( lhs.get(root)(jv).toList, rhs.get(root)(jv).toList)
          case When(lhs, op, rhs)    => op( lhs.test(root)(jv), rhs.test(root)(jv))
          case BasePredicate(filter) => filter.test(root)(jv)
        }

      if(!ret)
        println( s"Predicate result is false : $this : ${jv.pretty}")
      ret
    }

    override def pretty: String = this match {
      case MatchRegex(q, regex, _)   => s"${q.pretty} =~ $regex"
      case Contains(q)            => q.pretty
      case Compare(lhs, op, rhs)  => s"(${lhs.pretty} ${op.pretty} ${rhs.pretty})"
      case When(lhs, op, rhs)     => s"(${lhs.pretty} ${op.pretty} ${rhs.pretty})"
      case BasePredicate(filter)  => s"[?( ${filter.pretty} )]"
    }
  }

  final case class MatchRegex(q: Query, regex: String, reg: Regex) extends PredicateSelector
  final case class Contains(q: Query) extends PredicateSelector
  final case class Compare( lhs: FilterValue, op: Comparator, rhs: FilterValue) extends PredicateSelector
  final case class When(lhs: PredicateSelector, op: BinaryBoolOp, rhs: PredicateSelector) extends PredicateSelector
  final case class BasePredicate(filter: PredicateSelector) extends PredicateSelector

  case class RecursiveFilter(filter: PredicateSelector) extends JPath {
    override def pretty: String = s"..*${filter.pretty}"
  }

  //////////////////////////////////////////////////
  sealed trait Comparator extends Token {

    def stringCompare(l: String, r: String): Boolean = this match {
      case Eq   => l == r
      case Neq  => l != r
      case Gt   => l > r
      case Lt   => l < r
      case Gte  => l >= r
      case Lte  => l <= r
    }

    def numericCompare[T: Numeric](l: T, r: T): Boolean = {

      val num0 = implicitly[Numeric[T]]

      this match {
        case Eq   => num0.equiv(l, r)
        case Neq  => !num0.equiv(l, r)
        case Gt   => num0.gt(l, r)
        case Lt   => num0.lt(l, r)
        case Gte  => num0.gteq(l, r)
        case Lte  => num0.lteq(l, r)
      }
    }

    def compare(lhs: JValue, rhs: JValue): Boolean = {

      def err() = {
        println( s"not numeric compare: $lhs $this $rhs")
        false
      }

      val ret = (lhs, rhs) match {
        case (JString(l), JString(r)) => stringCompare(l, r)
        case (JDouble(n), r)          => r.toDouble().map( numericCompare(n, _)).getOrElse(err())
        case (JDecimal(n), r)         => r.toBigDecimal().map( numericCompare(n, _)).getOrElse(err())
        case (JLong(n), r)            => r.toLong().map( numericCompare(n, _)).getOrElse(err())
        case (JInt(n), r)             => r.toBigInt().map( numericCompare(n, _)).getOrElse(err())
        case _ => err()
      }
      ret
    }

    def apply(l: List[JValue], r: List[JValue]) = (l.length, r.length) match {
      case (1, n) if n > 0 => r.exists( compare(l.head, _))
      case (n, 1) if n > 0 => l.exists( compare(_, r.head))
      case (n, m) => println( s"not allowed comparison : $n X $m")
        false
    }

    override def pretty: String = this match {
      case Eq => "=="
      case Neq => "!="
      case Gt => ">"
      case Lt => "<"
      case Gte => ">="
      case Lte => "<="
    }
  }
  final case object Eq extends Comparator
  final case object Neq extends Comparator
  final case object Gt extends Comparator
  final case object Lt extends Comparator
  final case object Gte extends Comparator
  final case object Lte extends Comparator

  //////////////////////////////////////////////////
  sealed trait BinaryBoolOp extends Token {
    def apply( l: Boolean, r: Boolean): Boolean = this match {
      case And => l && r
      case Or  => l || r
    }

    override def pretty: String = this match {
      case And => "&&"
      case Or  => "||"
    }
  }
  final case object And extends BinaryBoolOp
  final case object Or extends BinaryBoolOp

}



