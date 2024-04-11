package validator.jsonValidator.syntax.valueRule

import org.json4s.{JString, JValue}
import validator.utils.StringUtil.{show, wordy}
import validator.jsonValidator._
import validator.jsonValidator.syntax.valueRule.ValueBoolTree.FunctionOperator.eval

object ValueBoolTree {

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

  private type FunctionOp = (FunctionOperator, FunctionExpr)

  ////////////////////////////////////////////////////////////////////////////////
  trait FunctionExpr {
    def evaluateWith(functionTable: FunctionTable)(jv: JValue) : Either[EvaluationError, Boolean]
    def checkSyntaxWith(functionTable: FunctionTable): Option[SyntaxError]
  }

  case class BoolFunction(name: String, args: Seq[String]) extends FunctionExpr {

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
    : Option[SyntaxError] = functionTable(name, args).getSyntaxError
  }

  case class FunctionOps(base: FunctionExpr, ops: List[FunctionOp]) extends FunctionExpr {

    override def toString: String =
      if (ops.nonEmpty) ops.map { case (op, exp) => s" $op $exp" }.mkString(s"( $base", " ", " )")
      else base.toString

    override def evaluateWith(functionTable: FunctionTable)(jv: JValue)
    : Either[EvaluationError, Boolean] = {

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

    override def checkSyntaxWith(ft: FunctionTable)
    : Option[SyntaxError] =
      ops.foldLeft( base.checkSyntaxWith(ft)){
        case (se, (_, e)) =>
          val err = List(se, e.checkSyntaxWith(ft)).flatten
          if( err.nonEmpty) Some( new ExprErrors(err))
          else None
      }

  }

}

//object SpecValueBoolTree extends App {
//
//  import validator.jsonValidator.syntax.valueRule.PredicateMaker.UserFunctionTable
//
//  val s = "f(1) || _myfuction(abc) || _oneOf('this is test', good) && gt(1) &&  lt(2000) && _between( 1, 1000) || _isString && _longerThan(1) || _oneOf(a, b, b, 'this is \\'' ) && _regex(1,2,3) || _shorterThan(a)"
//  val r = ValueRuleParser.parse(s)
//  show("=== expression ===")
//  r.foreach(show)
//
//  val jv: JValue = JString("this")
//
//  show(jv)
//
//  r.map(_.checkSyntaxWith(UserFunctionTable).foreach(show))
//
//  val zz = r.map(_.evaluateWith(UserFunctionTable)(jv))
//}


