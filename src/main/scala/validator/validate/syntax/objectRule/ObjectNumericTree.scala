package validator.validate.syntax.objectRule

import validator.utils.May.{mayOr, maybe}
import validator.validate.syntax.objectRule.ObjectNumericTree.NumericOperator.{cal0, fromInt}
import validator.validate.{EvalError, EvaluationError, ExprErrors, SyntaxError}

object ObjectNumericTree {

  ////////////////////////////////////////////////////////////////////////////////
  // Numeric Expression ADT
  ////////////////////////////////////////////////////////////////////////////////
  sealed trait NumericOperator
  case object Plus extends NumericOperator { override def toString: String = "+" }
  case object Minus extends NumericOperator { override def toString: String = "-" }
  case object Multiply extends NumericOperator { override def toString: String = "*" }
  case object Divide extends NumericOperator { override def toString: String = "/" }

  object NumericOperator {
    def fromInt[T:Numeric](i: Int) = implicitly[Numeric[T]].fromInt(i)

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

//      println( s"$l $op $r :: $ret")
      ret
    }
  }

  type NumOp = (NumericOperator, NumericExpression)

  ////////////////////////////////////////////////////////////////////////////////
  sealed trait NumericExpression {
    def calculateWith[T:Numeric](f: String => Either[String, T]): Either[EvaluationError, T]
    def checkSyntaxWith[T:Numeric](f: String => Either[String, T]): Option[SyntaxError] = None
  }

  case class Number(value: Int) extends NumericExpression {

    override def calculateWith[T: Numeric](f: String => Either[String, T])
    : Either[EvaluationError, T] = Right( fromInt( value) )

  }

  case class Term(path: String, default: Option[Int] = None) extends NumericExpression {

    val str0 = default.map(_.toString).getOrElse("_")
    override def toString: String = s"$${$path:$str0}"

    override def calculateWith[T: Numeric](f: String => Either[String, T])
    : Either[EvaluationError, T] = {

      val ret = f(path).fold(
        s => default
          .map{ i =>
            val neo = fromInt(i)
            println(s"$path not found, use default : $neo")
            neo
          }.toRight(new EvalError(s"$path => $s")),
        t => Right(t)
      )

      ret
    }
  }

  object Term {
    def apply(direction: String) = {

      val trim = direction.trim
      val (d, k) = trim.reverse.span(_ != ':')
      val d0 = d.reverse.toIntOption
      val k0 = maybe(k.tail.reverse)

      val ret = (d0, k0) match {
        case (Some(_), Some(s)) => new Term(s, d0) // strictly right case
        case (_, _) => new Term(trim, None)
      }

//      println(s"$direction\tTerm( key= ${ret.path}, default= ${ret.str0})")
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

    override def checkSyntaxWith[T: Numeric](f: String => Either[String, T])
    : Option[SyntaxError] = {

      val e0 = base.checkSyntaxWith(f).toList
      val es = ops.flatMap( _._2.checkSyntaxWith(f))
      val err = List(e0, es).flatten

      if(err.nonEmpty) Some(ExprErrors(err)) else None
    }
  }
}

object SpecSONADT extends App {

  import ObjectRuleParser._

  val s = "${key1:11} + ${path_2} - (${key9:11} / ${path_2} * ( ${path_3} + ${path_4} )) / ${path_5}  + ${path_6} * 12 - ${path_11} + ${path_12} * ${path_13__4:5} + ${path_14} / ${path:15} + ${path:16} "

  val m= Map(
    "key1_1"	    -> 1,
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

  val expr = parseNumeric(s)

  expr.foreach(println)

  expr.foreach( _.checkSyntaxWith(f).foreach(println))

  val z = expr.map( _.calculateWith(f))

  println(z)
}