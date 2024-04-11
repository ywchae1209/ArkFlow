package validator.validate.syntax.objectRule

import validator.validate.{EvalErrors, EvaluationError, ExprErrors, SyntaxError}
import validator.validate.syntax.objectRule.ObjectBoolTree.BoolOperator.eval
import validator.validate.syntax.objectRule.ObjectNumericTree._

object ObjectBoolTree {

  ////////////////////////////////////////////////////////////////////////////////
  // Bool & Compare Expression ADT
  ////////////////////////////////////////////////////////////////////////////////
  sealed trait BoolOperator
  case object And extends BoolOperator { override def toString: String = "&&" }
  case object Or extends BoolOperator { override def toString: String = "||" }

  object BoolOperator {
    def eval( l: Boolean, r: Boolean, op: BoolOperator ): Boolean = op match {
        case And => l && r
        case Or  => l || r
    }
  }

  sealed trait Comparator
  case object Lt extends Comparator { override def toString: String = "<" }
  case object Lte extends Comparator { override def toString: String = "<=" }
  case object Gt extends Comparator { override def toString: String = ">" }
  case object Gte extends Comparator { override def toString: String = ">=" }
  case object Eq extends Comparator { override def toString: String = "==" }
  case object NEq extends Comparator { override def toString: String = "!=" }

  object Comparator {
    def compare[T: Numeric]( l: T, r: T, op: Comparator): Boolean  = {
      val num0 = implicitly[Numeric[T]]

      val ret = op match {
        case Lt  => num0.lt(l, r)
        case Lte => num0.lteq(l, r)
        case Gt  => num0.gt(l, r)
        case Gte => num0.gteq(l, r)
        case Eq  => num0.equiv(l, r)
        case NEq => !num0.equiv(l, r)
      }
      println( s"$l $op $r :: $ret")

      ret
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  type BoolOp = (BoolOperator, BoolExpr)

  sealed trait BoolExpr {
    def evaluateWith[T:Numeric](f: String => Either[String, T]): Either[EvaluationError, Boolean]
    def checkSyntaxWith[T:Numeric](f: String => Either[String, T]): Option[SyntaxError]

  }

  case class NumComp(lhs: NumericExpression, rhs: NumericExpression, op: Comparator) extends BoolExpr {
    override def toString: String = s"( $lhs $op $rhs )"

    override def evaluateWith[T: Numeric](f: String => Either[String, T])
    : Either[EvaluationError, Boolean] = {

      val l = lhs.calculateWith(f)
      val r = rhs.calculateWith(f)

      (l, r) match {
        case (Right(v1), Right(v2)) => Right(Comparator.compare(v1, v2, op))
        case _ =>
          val err = List( l.left.toOption, r.left.toOption).flatten
          EvalErrors[Boolean](err:_*)
      }
    }

    override def checkSyntaxWith[T: Numeric](f: String => Either[String, T]): Option[SyntaxError] = {

      val err = List(lhs.checkSyntaxWith(f), rhs.checkSyntaxWith(f) ).flatten

      if( err.nonEmpty) Some( new ExprErrors(err)) else None
    }
  }

  case class BoolOps(base: BoolExpr, ops: List[BoolOp]) extends BoolExpr {

    val early = ops.to(LazyList)

    override def toString: String =
      if (ops.nonEmpty) ops.map { case (op, exp) => s" $op $exp" }.mkString(s"( $base", " ", " )")
      else s"$base"

    override def evaluateWith[T: Numeric](f: String => Either[String, T])
    : Either[EvaluationError, Boolean] = {

      early.foldLeft( base.evaluateWith(f) ){
        case (err@Left(_), _) => err
        case (Right(b), (op, e)) =>
          e.evaluateWith(f)
            .fold(
              s => EvalErrors(s),
              t => Right(eval(b, t, op)) )
      }
    }

    override def checkSyntaxWith[T: Numeric](f: String => Either[String, T])
    : Option[SyntaxError] = {

      val e0 = base.checkSyntaxWith(f).toList
      val es = ops.flatMap( _._2.checkSyntaxWith(f))
      val err = List(e0, es).flatten

      if( err.nonEmpty) Some( ExprErrors(err)) else None
    }
  }

}

object SpecSOBAdt extends App {

  import ObjectRuleParser._

  val s =
   // "${path\\}:11} > 0"
  "${path:11} < ${path_2} || 0 < ( ${path:1} / ${path_2} * ( ${path_3} + ${path_4} )) / ${path_5}  + ${path_6} * 12 - ${path_11} + ${path_12} * ${path_13__4:5} + ${path_14} / ${path:15} + ${path:16} "

  val m = Map(
    "path_0"	    -> 1,
    "path_2"      -> 2,
    "path_3"      -> 3,
    "path_4"      -> 4,
    "path_5"      -> 6,
    "path_6"      -> 6,
    "path_11"     -> 11,
    "path_12"     -> 12,
    "path_13"     -> 13,
    "path_14"     -> 14,
    "path_15"     -> 15,
    "path_16"     -> 16,
  )

  val f: String => Either[String, Double] = (s: String) => m.get(s).map(_.toDouble).toRight( s"not-found: $s")

  val expr: Either[String, ObjectBoolTree.BoolOps] = parse(s)
  println(expr)

  expr.foreach(println)

  expr.foreach( _.checkSyntaxWith(f).foreach(println))

  val z = expr.map( _.evaluateWith(f))
  println(z)
}
