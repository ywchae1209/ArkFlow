package transform.jsonClassify.patternMatching

import transform.jsonClassify.patternMatching.rule.MPRuleAST.{Rule, RuleRoot, Term}
import transform.utils.{Location, Trie}

case class Occurrence( keyword: String, loc: Seq[Int]) {
  override def toString: String =
    loc.mkString( s"$keyword: ", ",", "" )
}

/** {{{
 * << Multi-Pattern-Matching >>
 *
 * impl. note.
 *  ~ term matching :: Trie(current version) :: later improvement. Double-Array-Trie, KMP ( Aho-Corasick )
 *  ~ provide DSL   :: Rule(AST and Parser )
 * }}}
 */
case class MultiPattern(rules: Map[Int, RuleRoot], dictionary: Trie[Term]) {

  def search(s: String): Iterable[(Int, String, Seq[Occurrence])] =  {

    def matched(ms: Seq[(Term, Location)]) = {
      ms.groupBy(_._1.keyword).map{ case (k, v) =>
        Occurrence( k, v.map( _._2.from))
      }.toSeq
    }

    val ret =
      dictionary
        .search(s)
        .groupBy( _._1.root)
        .flatMap { case (root, ms: Seq[(Term, Location)]) =>
          rules.get(root).flatMap { r =>
            val p = (i: Int) => ms.exists { case (t, l) => (t.id == i) }  // todo :: location check
            val f = Rule.foldWith(r.rule)(p, () => matched(ms))
            f.map( (root, r.string, _))
          }
        }
    ret
  }
}

object MultiPattern {

  def apply( ss: (Int, String)*): MultiPattern = {

    val rs = ss.flatMap(s => RuleRoot( s._1, s._2))

    val rules = rs.groupBy( _.id).view.mapValues( s =>
      if(s.isDefinedAt(1)) throw new Exception( s.mkString( "error : duplicated ruleId ", "," ,"" )  )
      else s.head
    ).toMap

    val terms = rs.flatMap(_.terms ).map( t => t.keyword -> t)

    val dictionary = Trie( terms:_*)

    new MultiPattern( rules, dictionary)
  }
}
