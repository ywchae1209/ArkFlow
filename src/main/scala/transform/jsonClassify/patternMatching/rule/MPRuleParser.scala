package transform.jsonClassify.patternMatching.rule

import transform.jsonClassify.patternMatching.rule.MPRuleAST._

object MPRuleParser {


  import fastparse._
  import NoWhitespace._
  import fastparse.{CharPred, CharsWhileIn, P}

  def apply(ruleString: String): Option[Rule] = {

    parse(ruleString, expression(_)) match {

      case Parsed.Success(rule, _) => Some(rule)
      case Parsed.Failure(_, index, extra) =>

        println("========================================================\n" +
          s"Invalid Rule: $ruleString\n\tcheck rule-syntax. failed: at $index\n\t${extra.trace().msg}\n" +
          "========================================================")
        None
      case _ =>
        None
    }
  }

  /**
   * {{{
   * note)
   *  when called from negate, this impl. is right. (current version.)
   *  but, when flatten( or mutate state), TermsOps update recursively (And --> OR, OR --> AND)
   * }}}
   */
  private def not(r: Rule): Rule = r match {
    case TermsOp(child, kind, op) => TermsOp(child, kind, !op)
    case Term(g, r, op, id, root) => Term(g, r, !op, id, root)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Rule-ADT
  ////////////////////////////////////////////////////////////////////////////////

  private def sp[$: P] = P(CharsWhileIn(" \r\n\t").rep(max = 5))

  private def `{`[$: P] = P("{" ~ sp)

  private def `}`[$: P] = P(sp ~ "}")

  private def `(`[$: P] = P("(" ~ sp)

  private def `)`[$: P] = P(sp ~ ")")

  private def `|`[$: P] = P(sp ~ "|" ~ sp)

  private def `&`[$: P] = P(sp ~ "&" ~ sp)

  private def `~`[$: P] = P(sp ~ "~" ~ sp)

  private def ch[$: P] = P(CharPred(_ != '\''))

  private def eq[$: P] = P("\\'".!) // escape quote

  private def cl[$: P] = P(CharPred(_ != '}'))

  private def el[$: P] = P("\\}".!)

  ////////////////////////////////////////////////////////////////////////////////

  /**
   * Future spec
   * note) Someday... lookup will be needed..
   * i think, lookup is not term, but single type of expression.
   * ( may not used as AST-Branch or Leaf, but as Root)
   */
  private def lookup[$: P] = P("'${" ~ (el | cl).rep(1).! ~ "}'").map(s => Term(s, Lookup))

  private def exact[$: P] = P("^'" ~ (eq | ch).rep(1).! ~ "'$").map(s => Term(s, Exact))

  private def start[$: P] = P("^'" ~ (eq | ch).rep(1).! ~ "'").map(s => Term(s, StartWith))

  private def end[$: P] = P("'" ~ (eq | ch).rep(1).! ~ "'$").map(s => Term(s, EndWith))

  private def contain[$: P] = P("'" ~ (eq | ch).rep(1).! ~ "'").map(s => Term(s, Contain))

  private def always[$: P] = P("*").map(_ => Term("", Always))

  private def term[$: P] = P(exact | start | end | contain | always)

  private def termSeq[$: P] = P((term ~ `~`).rep(1) ~ term).map(s => TermsOp(s._1 :+ s._2, TermSeq))

  private def parentheses[$: P] = P(`(` ~ or ~ `)`)

  private def braces[$: P] = P(`{` ~ or ~ `}`)

  private def negate[$: P] = P("!" ~ sp ~ (parentheses | braces | term)).map(s => not(s))

  private def token[$: P]: P[Rule] = P(termSeq | parentheses | braces | negate | term)

  private def and[$: P]: P[Rule] = P(token ~ (`&` ~/ token).rep).map(s => TermsOp(s._1, s._2, And))

  private def or[$: P]: P[Rule] = P(and ~ (`|` ~/ and).rep).map(s => TermsOp(s._1, s._2, Or))

  private def expression[$: P]: P[Rule] = P(sp ~ or ~ sp ~ End)

}
