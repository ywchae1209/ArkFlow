package transform.jsonValidator.syntaxObject.rule

import transform.jsonValidator.syntaxObject.rule.ObjectRuleAST.BoolOperator.eval
import transform.jsonValidator.syntaxObject.rule.ObjectRuleAST.NumericOperator.{cal0, fromInt}
import transform.jsonValidator._
import transform.utils.May.{mayOr, maybe}
import transform.utils.StringUtil.{show, wordy}

object ObjectRuleAST {

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

  private object Comparator {
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
//      wordy( s"$l $op $r :: $ret")

      ret
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  private type BoolOp = (BoolOperator, BoolExpression)

  trait showExpression {
    def expressionWith[T](f: String => Either[String, T]): String
  }

  sealed trait BoolExpression extends showExpression {
    def evaluateWith[T:Numeric](f: String => Either[String, T]): Either[EvaluationError, Boolean]
  }

  case class NumericCompare(lhs: NumericExpression, rhs: NumericExpression, op: Comparator) extends BoolExpression {
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

    override def expressionWith[T](f: String => Either[String, T]): String =
      s"(${lhs.expressionWith(f)} $op ${rhs.expressionWith(f)})"

  }

  case class BoolOps(base: BoolExpression, ops: List[BoolOp]) extends BoolExpression {

    override def toString: String =
      if (ops.nonEmpty) ops.map { case (op, exp) => s" $op $exp" }.mkString(s"( $base", " ", " )")
      else s"$base"

    override def evaluateWith[T: Numeric](f: String => Either[String, T])
    : Either[EvaluationError, Boolean] = {

      ops.foldLeft( base.evaluateWith(f) ){
        case (err@Left(_), _) => err
        case (Right(b), (op, e)) =>
          e.evaluateWith(f)
            .fold(
              s => EvalErrors(s),
              t => Right(eval(b, t, op)) )
      }
    }

    override def expressionWith[T](f: String => Either[String, T]): String =
    if (ops.nonEmpty)
      ops.map { case (op, exp) => s" $op ${exp.expressionWith(f)}" }
        .mkString(s"( ${base.expressionWith(f)}", " ", " )")
    else s"${base.expressionWith(f)}"
  }


  ////////////////////////////////////////////////////////////////////////////////
  // Numeric Expression ADT
  ////////////////////////////////////////////////////////////////////////////////
  sealed trait NumericOperator
  case object Plus extends NumericOperator { override def toString: String = "+" }
  case object Minus extends NumericOperator { override def toString: String = "-" }
  case object Multiply extends NumericOperator { override def toString: String = "*" }
  case object Divide extends NumericOperator { override def toString: String = "/" }

  object NumericOperator {
    def fromInt[T:Numeric](i: Int): T = implicitly[Numeric[T]].fromInt(i)

    def cal0[T: Numeric](l: T, r: T, op: NumericOperator)(e: => String): Either[String, T] = {

      val num0 = implicitly[Numeric[T]]

      def div(a: T, b: T) =
        if (b == num0.zero)
          Left(s"$e is zero. Divide by zero is not allowed.( $a / $b )") // Double has Inf.
        else
          num0 match {
            case fr: Fractional[_] => mayOr(fr.div(a, b))(s"$e\t$a / $b")
            case in: Integral[_] => mayOr(in.quot(a, b))(s"$e\t$a / $b")
            case _ => Left(s"$e\tunknown $num0 $a / $b")
          }

      val ret = op match {
        case Plus => mayOr(num0.plus(l, r))(e)
        case Minus => mayOr(num0.minus(l, r))(e)
        case Multiply => mayOr(num0.times(l, r))(e)
        case Divide => div(l, r)
      }

//      wordy( s"$l $op $r :: $ret")
      ret
    }
  }

  private type NumOp = (NumericOperator, NumericExpression)

  ////////////////////////////////////////////////////////////////////////////////
  sealed trait NumericExpression extends showExpression {
    def calculateWith[T:Numeric](f: String => Either[String, T]): Either[EvaluationError, T]
  }

  case class Number(value: Int) extends NumericExpression {

    override def calculateWith[T: Numeric](f: String => Either[String, T])
    : Either[EvaluationError, T] = Right( fromInt( value) )

    override def expressionWith[T](f: String => Either[String, T]): String = value.toString
  }

  case class Term(path: String, default: Option[Int] = None) extends NumericExpression {

    val str0: String = default.map(_.toString).getOrElse("_")

    override def toString: String = s"$${$path:$str0}"

    override def calculateWith[T: Numeric](f: String => Either[String, T])
    : Either[EvaluationError, T] = {

      val ret = f(path).fold(
        s => default
          .map{ i =>
            val neo = fromInt(i)
//            wordy(s"$path not found, use default : $neo")
            neo
          }.toRight(new EvalError(s"$path => $s")),
        t => Right(t)
      )

      ret
    }

    override def expressionWith[T](f: String => Either[String, T])
    : String = f(path).fold( _ => s"_:$str0", t => s"$t")

  }

  object Term {

    def apply(direction: String): Term = {

      val trim = direction.trim
      val (d, k) = trim.reverse.span(_ != ':')
      val d0 = d.reverse.toIntOption
      val k0 = maybe(k.tail.reverse)

      val ret = (d0, k0) match {
        case (Some(_), Some(s)) => new Term(s, d0) // strictly right case
        case (_, _) => new Term(trim, None)
      }

      wordy(s"$direction\tTerm( key= ${ret.path}, default= ${ret.str0})")
      ret
    }
  }

  case class NumericOps(base: NumericExpression, ops: List[NumOp]) extends NumericExpression {

    override def toString: String =
      if (ops.nonEmpty) ops.map { case (op, exp) => s" $op $exp" }.mkString(s"( $base", " ", " )")
      else base.toString

    override def calculateWith[T: Numeric](f: String => Either[String, T])
    : Either[EvaluationError, T] = {

      ops.to(LazyList).foldLeft( base.calculateWith(f) ){
        case (err@Left(_), _) => err
        case (Right(b0), (op, e)) =>
          e.calculateWith(f)
            .fold(
              s => Left( s),
              t => cal0(b0, t, op)(s"$e").left.map( new EvalError(_)) )
      }
    }

    override def expressionWith[T](f: String => Either[String, T]): String =
      if (ops.nonEmpty)
        ops.map { case (op, exp) => s" $op ${exp.expressionWith(f)}" }
          .mkString(s"( ${base.expressionWith(f)}", " ", " )")
      else base.expressionWith(f)
  }
}



