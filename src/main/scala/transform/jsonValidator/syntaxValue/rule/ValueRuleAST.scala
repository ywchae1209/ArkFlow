package transform.jsonValidator.syntaxValue.rule

import org.json4s.JValue
import transform.jsonValidator._
import transform.jsonValidator.syntaxValue.rule.ValueRuleAst.FunctionOperator.eval

object ValueRuleAst {

  ////////////////////////////////////////////////////////////////////////////////
  // SyntaxValue ADT
  ////////////////////////////////////////////////////////////////////////////////
  sealed trait FunctionOperator
  case object And extends FunctionOperator { override def toString: String = "&&" }
  case object Or extends FunctionOperator { override def toString: String = "||" }

  object FunctionOperator {
    def eval( l: Boolean, r: Boolean, op: FunctionOperator): Boolean = op match {
      case And => l && r
      case Or  => l || r
    }
  }

  private type FunctionOp = (FunctionOperator, ValueCondition)

  ////////////////////////////////////////////////////////////////////////////////
  trait ValueCondition {
    def evaluateWith(functionTable: FunctionTable)(jv: JValue) : Either[EvaluationError, Boolean]
    def checkSyntaxWith(functionTable: FunctionTable): Option[SyntaxErrorValue]
  }

  case class VPredicate(name: String, args: Seq[String]) extends ValueCondition {

    override def toString: String = args.length match {
      case 0 => name
      case _ => args.mkString(s"$name(", ", ", ")")
    }

    override def evaluateWith(functionTable: FunctionTable)(jv: JValue)
    : Either[EvaluationError, Boolean] = {
      lazy val f = functionTable(name, args)
      f(jv)
    }

    override def checkSyntaxWith(functionTable: FunctionTable)
    : Option[SyntaxErrorValue] = functionTable(name, args).getSyntaxError
  }

  case class VPredicates(base: ValueCondition, ops: List[FunctionOp]) extends ValueCondition {

    override def toString: String =
      if (ops.nonEmpty) ops.map { case (op, exp) => s" $op $exp" }.mkString(s"( $base", " ", " )")
      else base.toString

    override def evaluateWith(functionTable: FunctionTable)(jv: JValue) : Either[EvaluationError, Boolean]
    = {

      val ret = {
        ops.foldLeft( base.evaluateWith(functionTable)(jv) ){
          case (err@Left(_), _) => err
          case (Right(b), (op, e)) => e.evaluateWith(functionTable)(jv).fold(
            s => EvalErrors( new EvalError(s"$e -> "), s),
            t => Right(eval(b, t, op)) )
        }
      }
//      ret.foreach( b => if(!b) wordy(s"${this.toString} == false"))

      ret
    }

    override def checkSyntaxWith(ft: FunctionTable) : Option[SyntaxErrorValue]
    = {
      val e0 = base.checkSyntaxWith(ft).toList
      val es = ops.flatMap( _._2.checkSyntaxWith(ft))
      val err = e0 ++ es

      err.length match {
        case 0 => None
        case 1 => Some(err.head)
        case _ => Some(SyntaxErrorValues(err, toString))
      }
    }
  }
}




