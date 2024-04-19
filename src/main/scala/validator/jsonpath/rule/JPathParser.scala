package validator.jsonpath.rule

import fastparse.NoWhitespace._
import fastparse._
import org.json4s.JsonAST.{JBool, JObject}
import org.json4s.{JDouble, JLong, JNothing, JNull, JString, JValue}
import validator.jsonpath.rule.JPathParser.{jsonPath, literal}
import validator.utils.StringUtil.parseWith

object JPathParser {

  //////////////////////////////////////////////////
  private def sp[$: P]= P(CharsWhileIn(" \r\n\t").rep(max = 80))

  // dot, comma
  //////////////////////////////////////////////////
  private def `.`[$: P] = P(sp ~ "." ~ sp)
  private def `..`[$: P] = P(sp ~ ".." ~ sp)
  private def `,`[$: P] = P(sp ~ "," ~ sp)

  private def `!`[$: P] = P(sp ~ "!" ~ sp)
  private def `?`[$: P] = P(sp ~ "?" ~ sp)
  private def `$`[$: P] = P(sp ~ "$" ~ sp)
  private def `@`[$: P] = P(sp ~ "@" ~ sp)
  private def `|`[$: P] = P(sp ~ "|" ~ sp)
  private def `:`[$: P] = P(sp ~ ":" ~ sp)

  private def `||`[$: P] = P(sp ~ "||" ~ sp)
  private def `&&`[$: P] = P(sp ~ "&&" ~ sp)

  //////////////////////////////////////////////////
  private def `=`[$: P] = P(sp ~ "=" ~ sp)
  private def `+`[$: P] = P(sp ~ "+" ~ sp)
  private def `-`[$: P] = P(sp ~ "-" ~ sp)
  private def `*`[$: P] = P(sp ~ "*" ~ sp)
  private def `/`[$: P] = P(sp ~ "/" ~ sp)

  private def `%`[$: P] = P(sp ~ "%" ~ sp)

  private def `>`[$: P] = P(sp ~ ">" ~ sp)
  private def `<`[$: P] = P(sp ~ "<" ~ sp)
  private def `==`[$: P] = P(sp ~ "==" ~ sp)
  private def `!=`[$: P] = P(sp ~ "!=" ~ sp)
  private def `>=`[$: P] = P(sp ~ ">=" ~ sp)
  private def `<=`[$: P] = P(sp ~ "<=" ~ sp)

  // quote
  //////////////////////////////////////////////////
  private def `'`[$: P] = P("'")
  private def esc_q[$: P] = P( "\\'")
  private def other_q[$: P] = P(CharPred( _ != '\''))

  private def singleQuoted[$: P] = P( `'` ~(other_q | esc_q).rep.! ~ `'`)
  private def unescapedSingleQuoted[$: P] = singleQuoted.map( _.replace("\\'", "'"))

  // double-quote
  //////////////////////////////////////////////////
  private def `"`[$: P] = P("\"")

  private def esc_dq[$: P] = P("\\\"").map( _ => "\"")
  private def other_dq[$: P] = P(CharPred( _ != '"'))
  private def doubleQuoted[$: P] = P( `"` ~ (other_dq | esc_dq).rep.! ~ `"`)
  private def unescapedDoubleQuoted[$: P] = doubleQuoted.map( _.replace("\\\"", "\""))

  // brace
  //////////////////////////////////////////////////
  private def `[`[$: P] = P(sp ~ "[" ~ sp)
  private def `]`[$: P] = P(sp ~ "]" ~ sp)

  private def `(`[$: P] = P(sp ~ "(" ~ sp)
  private def `)`[$: P] = P(sp ~ ")" ~ sp)

  private def `{`[$: P] = P(sp ~ "{" ~ sp)
  private def `}`[$: P] = P(sp ~ "}" ~ sp)

  // numbers
  //////////////////////////////////////////////////
  private def digit[$: P] = P(CharIn("0-9"))
  private def num[$: P] = P("-".? ~ digit.rep(1)).!
  private def num0[$: P] = P("-".? ~ digit.rep(1) ~ (`.` ~ digit.rep).?).!

  private def `true`[$: P] = P( "true" ).map( _ => true)
  private def `false`[$: P] = P( "false" ).map( _ => false)

  ////////////////////////////////////////////////////////////////////////////////

  private def number[$: P] = num.map( _.toInt)        // todo :: one more think

  val char_not_allowed_in_field = """*.[]()=!<>""" + " \r\n\t"

  private def field[$: P] = P( CharsWhile( !char_not_allowed_in_field.contains(_)).rep(1).!)
  private def field_quoted[$: P] = P(unescapedSingleQuoted | unescapedDoubleQuoted)
  private def value_quoted[$: P] = P(unescapedSingleQuoted | unescapedDoubleQuoted)

  ////////////////////////////////////////////////////////////////////////////////
  // array-Selector :: select with number
  ////////////////////////////////////////////////////////////////////////////////
  private def arraySelector[$: P]
  = P(  all | random | slice )

  private def all[$: P]    = P( `[` ~ (`*` | (`'`~ `*` ~`'`) | (`"` ~ `*` ~ `"`) ) ~ `]`).map( _ => All)
  private def random[$: P] = P( `[` ~ number.rep(1, `,`) ~ `]`).map(Random)
  private def slice[$: P]  = P( `[` ~ number.? ~ `:num`.? ~ `:num`.? ~ `]`).map( Slice.apply )
  private def `:num`[$: P] = P( `:` ~ number.?)

  ////////////////////////////////////////////////////////////////////////////////
  // Position
  ////////////////////////////////////////////////////////////////////////////////
  private def current[$: P] = `@`.map( _ => Current)
  private def root[$: P]    = `$`.map( _ => Root)       // todo ::: JPath or FieldSelector

  ////////////////////////////////////////////////////////////////////////////////
  // Field Selector :: select by name
  ////////////////////////////////////////////////////////////////////////////////
  private def fieldSelector[$: P]
  = P( `.*` | `..*`  | `[*]` | `.key` | `..key` | `[keys]` | recursiveFilter) // todo

  private def `..*`[$:P]    = P(`..`~`*`).map(_ => RecursiveAllFields)    // todo :: ??
  private def `[*]`[$: P]   = P( `[` ~ (`*` | (`'`~ `*` ~`'`) | (`"` ~ `*` ~ `"`) ) ~ `]`).map( _ => AllFields)
  private def `.*`[$:P]     = P(`.` ~ `*`).map(_ => AllFields)

  private def `.key` [$: P]  = P( `.` ~ field).map( FieldSelector.field)
  private def `..key` [$: P] = P( `..` ~ field).map( FieldSelector.recursiveField)
  private def `[keys]`[$: P] = P( `[` ~ field_quoted.rep(1, `,`) ~ `]`).map( FieldSelector.fields)

  ////////////////////////////////////////////////////////////////////////////////
  // childSelector
  ////////////////////////////////////////////////////////////////////////////////
  private def selector[$: P] = P( arraySelector | fieldSelector )
  private def route[$: P]    = P( (selector | filterPredicate).rep)

  ////////////////////////////////////////////////////////////////////////////////
  // literal (used in FilterPredicate)
  private def literal[$: P]: P[Literal]
  = P ( numberValue | numberValue0 | booleanValue | nullValue | emptyValue | stringValue)

  private def numberValue[$: P]  = num.map( Literal.long)
  private def numberValue0[$: P] = num0.map( Literal.double)
  private def booleanValue[$: P] = P( `true` | `false`).map( Literal.boolean)
  private def nullValue[$: P]    = P( "null" ).map( _ => Literal.Null)
  private def emptyValue[$: P]   = P( "empty" ).map( _ => Literal.empty)    // not exist
  private def stringValue[$: P]  = value_quoted.map( Literal.string)

  ////////////////////////////////////////////////////////////////////////////////
  // Filter-predicate
  private def filterPredicate[$: P]: P[FilterPredicate] = P( `[` ~ `?` ~ `(` ~ boolExpr ~ `)` ~ `]`)
  private def recursiveFilter[$: P] = P( `..` ~ `*`.? ~ filterPredicate).map( RecursiveFilter)    // todo

  private def query[$: P] = P ( (current | root) ~ route).map{ case (p, ps) => Query(p, ps.toList) }

  ////////////////////////////////////////////////////////////////////////////////

  def jsonPath[$: P] = P (query ~ sp ~ End)

  ////////////////////////////////////////////////////////////////////////////////
  // compare-expr
  private def compExpr[$: P]: P[FilterPredicate] = P ( expr0 | expr1 | expr2)

  private def expr0[$: P] = P( query ~ `=~` ~ value_quoted).map{
    case (q, regex) => MatchRegex(q, regex)
  }

  private def expr1[$: P] = P( query ~ (comparator ~ (query | literal)).?).map{
    case (lhs: Query, None) => Contains(lhs)
    case (lhs, Some( (op, rhs) )) => Compare( lhs, op, rhs)
  }
  private def expr2[$: P] = P( literal ~ comparator ~ query ).map{
    case (lhs, op, rhs) => Compare(lhs, op, rhs)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // comparator
  ////////////////////////////////////////////////////////////////////////////////
  private def comparator[$: P]: P[Comparator] = P ( eq | neq | gte | lte | gt | lt )

  private def gt[$: P]  = P (`>`)map( _ => Gt)
  private def lt[$: P]  = P (`<`).map( _ => Lt)
  private def eq[$: P]  = P (`==`).map( _ => Eq)
  private def neq[$: P] = P (`!=`).map( _ => Neq)
  private def gte[$: P] = P (`>=`).map( _ => Gte)
  private def lte[$: P] = P (`<=`).map( _ => Lte)

  // regex rule
  private def `=~`[$: P] = P (sp ~ "=~" ~sp)

  ////////////////////////////////////////////////////////////////////////////////
  // binary-bool-operation
  ////////////////////////////////////////////////////////////////////////////////
  private def boolExpr[$: P] = or.map( BasePredicate)

  private def and[$: P] = P( factorBool ~ (`&&` ~ factorBool).rep).map {
    case (base, rs) => rs.foldLeft(base)((l, r) => When(l, And, r))
  }
  private def or[$: P]  = P( and ~ (`||` ~ and).rep).map{
    case (base, rs) => rs.foldLeft(base)((l, r) => When(l, Or, r))
  }
  private def factorBool[$: P]: P[FilterPredicate] = P( paransBool | compExpr)
  private def paransBool[$: P]  = P( `(` ~/ or ~ `)` )

  ////////////////////////////////////////////////////////////////////////////////
  // AST
  ////////////////////////////////////////////////////////////////////////////////
  sealed trait Token

  sealed trait JPath extends Token

  sealed trait Position extends JPath {
    override def toString: String = this match {
      case Root => "$"
      case Current => "@"
    }
  }
  final case object Root    extends Position
  final case object Current extends Position

  sealed trait FieldSelector extends JPath {
    override def toString: String = this match {
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

    override def toString: String = this match {
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
    override def toString: String = this match {
      case Query(p, route) => s"_${p}_" + route.mkString
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
    override def toString: String = this match {
      case MatchRegex(q, regex)   => s"$q =~ $regex"
      case Contains(q)            => q.toString
      case Compare(lhs, op, rhs)  => s"($lhs $op $rhs)"
      case When(lhs, op, rhs)     => s"($lhs $op $rhs)"
      case BasePredicate(filter)  => s"[? $filter ]"
    }
  }
  final case class MatchRegex(q: Query, regex: String) extends FilterPredicate
  final case class Contains(q: Query) extends FilterPredicate
  final case class Compare( lhs: FilterValue, op: Comparator, rhs: FilterValue) extends FilterPredicate
  final case class When( lhs: FilterPredicate, op: BinaryBoolOp, rhs: FilterPredicate) extends FilterPredicate
  final case class BasePredicate(filter: FilterPredicate) extends FilterPredicate

  case class RecursiveFilter(filter: FilterPredicate) extends JPath {
    override def toString: String = s"..*$filter"
  }

  //////////////////////////////////////////////////
  sealed trait Comparator extends Token {
    override def toString: String = this match {
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
    override def toString: String = this match {
      case And => "&&"
      case Or  => "||"
    }
  }
  final case object And extends BinaryBoolOp
  final case object Or extends BinaryBoolOp

}

object SpecJsonPathParser extends App{

  def show(s: String) = {
    val p = parseWith(jsonPath(_))(s)
    println(s)
    p.foreach(println)
    println("=============================")
  }
  val ss = List(
    "$.store.book[*].author",
    "$..author",
    "$.store.*",
    "$.store..price",
    "$..book[2]",       // todo
    "$..book[-2]",
    "$..book[1,2]",
    "$..book[:2]",
    "$.book[1:2]",      // <<<
    "$..book[1:2]",     // <<<
    "$..book[-2:]",     // <<<
    "$..book[2:]",      // <<<
    "$..book[?(@.isbn)]",
    "$.store.book[?(@['price'] < 10)]",
    "$..book[?(@.price <= $['expensive'])]",
    "$..book[?(@.author =~ '/.*REES/i')]",
    "$..*",
    "$..book",
    "$['store']['book'][0]['title']",
    "$['store']['book'][0]['title'].*[?( @.name =~ 'abc*' || @.name =~ 'year')]",
    "$.book.store[?(@.price <= $['expensive'])]",
    "$.this[? ( (@.price < 10 && @.category == 'fiction') || (@.name == 'this') )]",
    "$.store.book[-1].title"
  )

  ss.foreach(show)

}
