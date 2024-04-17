package validator.jsonValidator.syntaxValue.rule

import org.json4s.{JString, JValue}
import validator.jsonValidator._
import validator.jsonValidator.syntaxValue.rule.PredicateMaker.UserFunctionTable
import validator.jsonValidator.syntaxValue.rule.ValueRuleAst.FunctionOperator.eval
import validator.utils.StringUtil.{show, wordy}

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
      ret.foreach( b => if(!b) wordy(s"${this.toString} == false"))

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

object SpecValueRuleAST extends App {

  val s = "f(1) || (_myfuction(abc) && my() || this()) || " +
    "_oneOf('this is test', good) && gt(1) &&  " +
    "lt(2000) && _between( 1, 1000) || _isString && _longerThan(1) " +
    "|| _oneOf(a, b, b, 'this is \\'' ) && _regex(1,2,3) || _shorterThan(a)"

  // parsing to ValueFunction
  val r = ValueRuleParser.parse(s)

//  r.foreach(p => {
//    show("=== expression ===")
//    show(p.toString)
//  })

  val jv: JValue = JString("this")
  show(jv)

  r.map(_.checkSyntaxWith(UserFunctionTable)
    .foreach(e => e.show())
  )

  val zz = r.map(_.evaluateWith(UserFunctionTable)(jv))
}


