package validator.jsonpath.rule

import org.json4s.{JDouble, JLong, JNothing, JNull, JString, JValue}
import org.json4s.JsonAST.JBool

object JPathAST {

  ////////////////////////////////////////////////////////////////////////////////
  // AST
  ////////////////////////////////////////////////////////////////////////////////
  sealed trait Token {
    def show: String
  }

  sealed trait JPath extends Token

  sealed trait Position extends JPath {
    override def show: String = this match {
      case Root => "$"
      case Current => "@"
    }
  }
  final case object Root    extends Position
  final case object Current extends Position

  sealed trait FieldSelector extends JPath {
    override def show: String = this match {
      case Field(name)          => "." + name
      case Fields(names)        => names.mkString("[",",","]")
      case RecursiveField(name) => ".." + name
      case AllFields            => ".*"
      case RecursiveAllFields   => "..*"
    }
  }
  final case class Field(name: String)          extends FieldSelector
  final case class Fields(names: List[String])  extends FieldSelector
  final case class RecursiveField(name: String) extends FieldSelector
  final case object AllFields                   extends FieldSelector
  final case object RecursiveAllFields          extends FieldSelector

  object FieldSelector {
    def recursiveField(s: String) = new RecursiveField(s)
    def field( s: String ) = Field(s)
    def fields( s: Seq[String] ) = if(s.length == 1) Field(s.head) else new Fields(s.toList)
    val AllField = AllFields
  }

  //////////////////////////////////////////////////
  sealed trait ArraySelector extends JPath {
    private def m[T](t: Option[T]) = t.map(_.toString).getOrElse("")

    override def show: String = this match {
      case Slice(s, e, t) => s"[${m(s)}:${m(e)}:${t}]"
      case Random(index)  => index.mkString("[", ",", "]")
      case All            => "[*]"
    }
  }
  final case class Slice(start: Option[Int], end: Option[Int], step: Int) extends ArraySelector
  final case class Random( index: Seq[Int]) extends ArraySelector
  final case object All extends ArraySelector

  object Slice {
    def apply(slice: (Option[Int], Option[Option[Int]], Option[Option[Int]])): ArraySelector = {
      slice match {
        case (None, None, None) => All
        case (start, end, step) => Slice(start, end.flatten, step.flatten.getOrElse(1))
      }
    }
  }

  //////////////////////////////////////////////////
  sealed trait FilterValue extends Token {
    override def show: String = this match {
      case Query(p, route) => s"${p.show}" + route.map(_.show).mkString
      case Literal(jv)     => jv.values.toString
    }
  }
  final case class Query(p: Position, route: List[JPath] ) extends FilterValue
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
  sealed trait FilterPredicate extends JPath {
    override def show: String = this match {
      case MatchRegex(q, regex)   => s"${q.show} =~ $regex"
      case Contains(q)            => q.show
      case Compare(lhs, op, rhs)  => s"(${lhs.show} ${op.show} ${rhs.show})"
      case When(lhs, op, rhs)     => s"(${lhs.show} ${op.show} ${rhs.show})"
      case BasePredicate(filter)  => s"[? ${filter.show} ]"
    }
  }
  final case class MatchRegex(q: Query, regex: String) extends FilterPredicate
  final case class Contains(q: Query) extends FilterPredicate
  final case class Compare( lhs: FilterValue, op: Comparator, rhs: FilterValue) extends FilterPredicate
  final case class When( lhs: FilterPredicate, op: BinaryBoolOp, rhs: FilterPredicate) extends FilterPredicate
  final case class BasePredicate(filter: FilterPredicate) extends FilterPredicate

  case class RecursiveFilter(filter: FilterPredicate) extends JPath {
    override def show: String = s"..*${filter.show}"
  }

  //////////////////////////////////////////////////
  sealed trait Comparator extends Token {
    override def show: String = this match {
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
    override def show: String = this match {
      case And => "&&"
      case Or  => "||"
    }
  }
  final case object And extends BinaryBoolOp
  final case object Or extends BinaryBoolOp

  object JPathASTFeature {
    // todo :::


  }

}
