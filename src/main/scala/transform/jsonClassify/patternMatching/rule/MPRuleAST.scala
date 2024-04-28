package transform.jsonClassify.patternMatching.rule

object MPRuleAST {

  case class RuleRoot(id: Int, rule: Rule, terms: Seq[Term], string: String)

  object RuleRoot {

    def apply( root: Int, ruleString: String): Option[RuleRoot] = {
      Rule( root, ruleString).map( rule =>
        new RuleRoot(root, rule, rule.getAllTerms, rule.toString)
      )
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  sealed trait TermType
  case object Exact extends TermType
  case object StartWith extends TermType
  case object EndWith extends TermType
  case object Contain extends TermType
  case object Lookup extends TermType
  case object Always extends TermType

  ////////////////////////////////////////////////////////////////////////////////
  sealed trait TermsOpType
  case object Or extends TermsOpType
  case object And extends TermsOpType
  case object TermSeq extends TermsOpType

  ////////////////////////////////////////////////////////////////////////////////

  sealed trait Rule {
    val op: Boolean
    override def toString: String = Rule.toString(this)
    def getAllTerms: Seq[Term] = Rule.getAllTerms(this)
  }

  case class Term ( keyword: String, kind: TermType, op: Boolean = true, id: Int = 0, root: Int = 0) extends Rule
  case class TermsOp(children: Seq[Rule], kind: TermsOpType, op: Boolean = true) extends Rule

  ////////////////////////////////////////////////////////////////////////////////
  object TermsOp {

    def apply(c0: Rule, cs: Seq[Rule], kind: TermsOpType): Rule=
      if (cs.isEmpty) c0 else TermsOp( c0 +: cs, kind)
  }

  object Rule {

    def apply( root: Int, ruleString: String): Option[Rule] = {

      MPRuleParser( ruleString)
        .map( rule =>
          // term's Id and root-id is set after parsing
          setTermId( flatten(rule), root)
        )
    }

    // todo :: logic completion
    def foldWith[T](root: Rule)(f: Int => Boolean, all: () => T): Option[T] = {

      def go(tree: Rule): Boolean = tree match {
        case TermsOp( child, kind, op) =>  kind match {
          case Or       => if (op) child.exists(go) else !child.forall(go)
          case And      => if (op) child.forall(go) else !child.exists(go)
          case TermSeq  => if (op) child.forall(go) else !child.forall(go)
        }

        case Term( _, _, op, id, _) => if (op) f(id) else !f(id)
      }

      val ok = go(root)
      if (ok) Some(all()) else None
    }

    ////////////////////////////////////////////////////////////////////////////////
    /**
     * todo:: flatten Redundant Operation
     */
    def flatten(from: Rule) : Rule = from

    ////////////////////////////////////////////////////////////////////////////////
    private def getAllTerms(sr: Rule): Seq[Term] = {
      val buf = collection.mutable.ListBuffer[Term]()

      def go(t: Rule): Unit = t match {
        case TermsOp( child, _, _) => child.foreach(go)
        case l @Term( _, _, _, _, _) => buf += l
      }

      go(sr)
      buf.toList
    }

    private def setTermId(sr: Rule, root: Int): Rule = {

      var i = 0
      def go( n: Rule): Rule = {
        n match {
          case TermsOp( child, k, o) => TermsOp( child.map(go), k, o)
          case Term(s, k, o, _, _)     =>
            val ret = Term( s, k, o, i, root)
            i = i+1
            ret
        }
      }
      go(sr)
    }

    private def toString( t: Rule): String = t match {
      case TermsOp( child, kind, op) =>
        kind match {
          case Or => child.mkString(if (op) "(" else "!(", " | ", ")")
          case And => child.mkString(if (op) "(" else "!(", " & ", ")")
          case TermSeq => child.mkString(if (op) "(" else "!(", " ~ ", ")")
        }

      case Term( g, r, op, id, root) =>
        val o = if (op) "" else "!"
        val pp =
          r match {
            case Exact     => s"^$g$$"
            case StartWith => s"^$g"
            case EndWith   => s"$g$$"
            case Contain   => s"$g"
            case Lookup   => s"$$$$$${$g}$$$$$$"
            case Always   => "*"
          }
        //        s"$id:$o$pp"
        s"$o$pp"
    }
  }
}



