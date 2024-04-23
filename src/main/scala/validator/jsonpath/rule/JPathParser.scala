package validator.jsonpath.rule

import fastparse.NoWhitespace._
import fastparse._
import org.json4s.JValue
import validator.jsonpath.rule.JPathAST._
import validator.jsonpath.rule.JPathParser.{compile, jsonPath}
import validator.utils.JsonUtil.{JValueWithPower, StringWithJsonPower}
import validator.utils.StringUtil.parseWith


/*
  java-script version spec :: https://www.npmjs.com/package/jsonpath
  java version sepc        :: https://github.com/json-path/JsonPath
 */
object JPathParser {

  def compile(s: String) = parseWith(jsonPath(_))(s)

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

  val char_not_allowed_in_field = """*.[]()=!<>:"'""" + " \r\n\t"

  private def field[$: P] = P( CharsWhile( !char_not_allowed_in_field.contains(_)).rep(1).!)
  private def field_quoted[$: P] = P(unescapedSingleQuoted | unescapedDoubleQuoted)
  private def value_quoted[$: P] = P(unescapedSingleQuoted | unescapedDoubleQuoted)

  ////////////////////////////////////////////////////////////////////////////////
  // array-Selector :: select with number
  ////////////////////////////////////////////////////////////////////////////////
  private def arraySelector[$: P]
  = P(  all | random | slice )

  private def all[$: P]    = P( `[` ~ (`*` | (`'`~ `*` ~`'`) | (`"` ~ `*` ~ `"`) ) ~ `]`).map( _ => AllItems)
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

  private def `..*`[$:P]    = P(`..`~`*`).map(_ => RecursiveAll)    // todo :: ??
  private def `[*]`[$: P]   = P( `[` ~ (`*` | (`'`~ `*` ~`'`) | (`"` ~ `*` ~ `"`) ) ~ `]`).map( _ => AllFields)
  private def `.*`[$:P]     = P(`.` ~ `*`).map(_ => AllFields)

  private def `.key` [$: P]  = P( `.` ~ (field| field_quoted)).map( FieldSelector.field)
  private def `..key` [$: P] = P( `..` ~ ( field| field_quoted)).map( FieldSelector.recursiveField)
  private def `[keys]`[$: P] = P( `[` ~ field_quoted.rep(1, `,`) ~ `]`).map( FieldSelector.fields)

  ////////////////////////////////////////////////////////////////////////////////
  // childSelector, route
  ////////////////////////////////////////////////////////////////////////////////
  private def function[$: P] = P( arraySelector | fieldSelector )     // todo .function(literal1,literal2,...) :default
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
  private def filterPredicate[$: P]: P[PredicateSelector] = P( `[` ~ `?` ~ `(` ~ boolExpr ~ `)` ~ `]`)
  private def recursiveFilter[$: P] = P( `..` ~ `*`.? ~ filterPredicate).map( RecursiveFilter)    // todo

  private def query[$: P] = P ( (current | root).? ~ route).map{
    case (Some(p), ps) => Query(p, ps.toList)
    case (None, ps) => Query(Current, ps.toList)
  }

  ////////////////////////////////////////////////////////////////////////////////

  def jsonPath[$: P] = P (query ~ sp ~ End)

  ////////////////////////////////////////////////////////////////////////////////
  // compare-expr
  private def compExpr[$: P]: P[PredicateSelector] = P ( expr0 | expr1 | expr2)

  private def expr0[$: P] = P( query ~ `=~` ~ value_quoted).map{
    case (q, regex) => MatchRegex(q, regex, regex.r)
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
  private def factorBool[$: P]: P[PredicateSelector] = P( paransBool | compExpr)
  private def paransBool[$: P]  = P( `(` ~/ or ~ `)` )

}

