package validator.validate.syntax.valueRule

import org.json4s.{JDecimal, JDouble, JInt, JLong, JValue}
import validator.utils.JsonUtil.{JValueWithPower, getLong}
import validator.utils.StringUtil
import validator.validate.EvalError.StringWithSyntaxPower
import validator.validate.ExprError.StringWithExprErrorPower
import validator.validate.syntax.valueRule.Predicate.JPredicate
import validator.validate.{EvalError, EvalSyntaxErr, EvaluationError, ExprError, SyntaxError}

import scala.collection.immutable.{HashMap, HashSet}
import scala.math.BigDecimal.long2bigDecimal
import scala.math.Numeric.Implicits.infixNumericOps


case class Predicate (check: Either[SyntaxError, JPredicate ],
                      name: String,
                      args: List[String] ) {

  def getSyntaxError: Option[SyntaxError] = check.fold( Option(_), _ => None)
  lazy val mayError = check.fold( EvalSyntaxErr[JPredicate], Right(_) )

  def apply(jv: JValue) : Either[EvaluationError, Boolean] = {
    mayError.flatMap( f =>
    {
      val ret = f(jv)

      ret.left.foreach{ s =>
        val msg = s"$s : i'll judge as false"
        println( args.mkString( s"Predicate :: $name(", ", ", s") == $msg"))
      }

      ret.fold( _ => Right(false), r => Right(r))
    } )
  }
}

object Predicate {
  type JPredicate = JValue => Either[EvaluationError, Boolean]
}

case class PredicateMaker( name: String,
                           make: Seq[String] => Either[SyntaxError, JPredicate]) {
  override def toString: String = s"PredicateMaker($name)"

  def apply(s: Seq[String])
  : Either[SyntaxError, JPredicate] = make(s)
}

case class FunctionTable(functionTable: Map[String, PredicateMaker]) {

  def apply(name: String, args: Seq[String]) : Predicate = {
    val check =
      functionTable.get(name)
        .map(_(args))
        .getOrElse( Left(new ExprError(s"Undefined-function: $name")) )   //todo

    Predicate(check, name, args.toList)
  }

  def show = println( functionTable.keys.toList.sorted.mkString("=== Functions ===\n\t","\n\t","\n============\n"))
}

object FunctionTable {
  def apply( predicateMaker: PredicateMaker*): FunctionTable = {

    val m = predicateMaker.groupBy(_.name)
    val f = m.filter(_._2.length > 1)
    if( f.nonEmpty )
      println( f.keys.mkString("Duplicate function-name: [",", ", "]" ))

    new FunctionTable(
      HashMap( m.map( kv => kv._1 -> kv._2.head).toList:_* )
    )
  }

}

////////////////////////////////////////////////////////////////////////////////

object PredicateMaker {

  private def toStringOr(s: String) = Right(s)

  private def ofLen(l: Seq[String], n: Int)
  : Either[SyntaxError, Seq[String]] = if( l.length == n) Right(l) else s"arg-count must $n".exprError

  private def ofType0[T](name: String,
                        conv: String => Either[SyntaxError, T])
                       (toPred: Seq[T] => JPredicate)
  = PredicateMaker( name, args => {
        val (lefts, rights) = args.map(conv).partitionMap(identity)
        if (lefts.nonEmpty) lefts.mkString(",").exprError
        else Right(rights)
      }.map(toPred) )


  private def ofType[T](name: String,
                        len: Int,
                        conv: String => Either[SyntaxError, T])
                       (toPred: Seq[T] => JPredicate)
  = PredicateMaker( name, args => ofLen(args, len)
      .flatMap { l =>
        val (lefts, rights) = l.take(len).map(conv).partitionMap(identity)
        if (lefts.nonEmpty) lefts.mkString(s"$name : (", ",", ")").exprError
        else Right(rights)
      }
      .map(toPred) )

  private def toLongOr(s: String) = s.toLongOption.toRight(ExprError(s"$s is not long"))
  private def toDoubleOr(s: String) = s.toDoubleOption.toRight(ExprError(s"$s is not double"))

  private def ofLong0(n: String)( f: Seq[Long] => JPredicate) = ofType0[Long](n, toLongOr)(f)
  private def ofDouble0(n: String)( f: Seq[Double] => JPredicate) = ofType0[Double](n, toDoubleOr)(f)
  private def ofString0(n: String)( f: Seq[String] => JPredicate) = ofType0[String](n, toStringOr)(f)

  private def ofLong(n: String, l: Int)( f: Seq[Long] => JPredicate) = ofType[Long](n, l, toLongOr)(f)
  private def ofDouble(n: String, l: Int)( f: Seq[Double] => JPredicate) = ofType[Double](n, l, toDoubleOr)(f)
  private def ofString(n: String, l: Int)( f: Seq[String] => JPredicate) = ofType[String](n, l, toStringOr)(f)

  ////////////////////////////////////////////////////////////////////////////////
  val _any        = ofString0("_any")(_ => _ => Right(true))

  val gt          = ofLong("gt", 1)( args => Calc.gt(args.head)(_) )
  val lt          = ofLong("lt", 1)( args => Calc.lt(args.head)(_) )
  val gte         = ofLong("gte", 1)( args => Calc.gte(args.head)(_) )
  val lte         = ofLong("lte", 1)( args => Calc.lte(args.head)(_) )
  val between     = ofLong("between", 2)( args => Calc.between(args.head, args(1))(_) )

  val gt0         = ofDouble("gt0", 1)( args => Calc.gt(args.head)(_) )
  val lt0         = ofDouble("lt0", 1)( args => Calc.lt(args.head)(_) )
  val gte0        = ofDouble("gte0", 1)( args => Calc.gte(args.head)(_) )
  val lte0        = ofDouble("lte0", 1)( args => Calc.lte(args.head)(_) )
  val between0    = ofDouble("between0", 2)( args => Calc.between(args.head, args(1))(_) )

  ////////////////////////////////////////////////////////////////////////////////
  val _isString   = ofString0("_isString")(_ => Calc._isString)

  val _oneOf      = ofString0("_oneOf")(args => Calc._oneOf(args)(_))
  val _digit      = ofString0("_digit")(args => Calc._digitOr(Nil))
  val _digitOr    = ofString0("_digitOr")(args => Calc._digitOr(args))
  val _charsIn    = ofString0("_charsIn")(args => Calc._charsIn(args))
  val _regex      = ofString0("_regex")(args => Calc._regex(args))
  val _date       = ofString0("_date")(args => Calc._date(args))

  val _length     = ofLong0("_length")(args => Calc._length(args))
  val _longerThan = ofLong("_longerThan", 1)(args => Calc._longerThan(args.head))
  val _shorterThan= ofLong("_shorterThan", 1)(args => Calc._shorterThan(args.head))

  val _lt         = ofLong("_lt", 1)(args => Calc._lt(args.head))
  val _gt         = ofLong("_gt", 1)(args => Calc._gt(args.head))
  val _lte        = ofLong("_lte", 1)(args => Calc._lte(args.head))
  val _gte        = ofLong("_gte", 1)(args => Calc._gte(args.head))
  val _between    = ofLong("_between", 2)(args => Calc._between(args.head, args(1)))

  val _lt0        = ofDouble("_lt0", 1)(args => Calc._lt0(args.head))
  val _gt0        = ofDouble("_gt0", 1)(args => Calc._gt0(args.head))
  val _lte0       = ofDouble("_lte0", 1)(args => Calc._lte0(args.head))
  val _gte0       = ofDouble("_gte0", 1)(args => Calc._gte0(args.head))
  val _between0   = ofDouble("_between0", 2)(args => Calc._between0(args.head, args(1)))

  ////////////////////////////////////////////////////////////////////////////////

  val UserFunctionTable = FunctionTable(
    _any,
    gt, lt, gte, lte, between,
    gt0, lt0, gte0, lte0, between0,
    _lt, _gt, _lte, _gte, _between,
    _lt0, _gt0, _lte0, _gte0, _between0,
    _isString, _oneOf, _digit, _digitOr, _charsIn, _regex,
    _date, _length, _longerThan, _shorterThan,
  )

  UserFunctionTable.show

}

object Calc {

  private def toLongOr(s: String) = s.toLongOption.toRight(s"$s is not long".evalError)
  private def toDoubleOr(s: String) = s.toDoubleOption.toRight(s"$s is not double".evalError)

  val getString: JValue => Either[EvalError, String]
  = (jv: JValue) => jv.getString.fold( EvalError(_), Right(_))

  val _isString: JValue => Either[EvalError, Boolean]
  =  (jv) => getString(jv).map( _ => true)

  def _oneOf(args: Seq[String]) = (jv: JValue) => {
    getString(jv).map( s => args.contains(s))
  }

  def _digitOr(args: Seq[String]) = (jv: JValue) => {
    val set = HashSet.apply( args.mkString.toList.distinct: _*)
    getString(jv).map( s => s.forall(c => c.isDigit || set.contains(c)) )
  }

  def _charsIn(args: Seq[String]) = (jv: JValue) => {
    val set = HashSet.apply( args.mkString.toList.distinct: _*)
    getString(jv).map( s => s.forall(c => c.isDigit || set.contains(c)) )
  }

  def _regex(args: Seq[String]) = (jv: JValue) => {
    val regex = args.map( _.r)
    getString(jv).map( s => regex.exists( _.findFirstIn(s).nonEmpty) )
  }

  def _date( args: Seq[String]) = (jv: JValue) => {
    val map = Map("yyyy" -> "[12][0-9][0-9][0-9]", "mm" -> "[01][0-9]", "dd" -> "[0-3][0-9]")
    val sts = args.map( s => StringUtil.interpolate(map.get)(s) )
    val regex = sts.map( _.r)

    getString(jv).map( s => regex.exists( _.findFirstIn(s).nonEmpty))
  }

  def _length(args: Seq[Long]) = (jv: JValue) => {
    getString(jv).map( s => args.contains(s.length) )
  }

  def _longerThan(args: Long) = (jv: JValue) => { getString(jv).map( _.length > args) }
  def _shorterThan(args: Long) = (jv: JValue) => { getString(jv).map( _.length < args) }

  def _lt(n: Long)  = (jv: JValue) => { getString(jv).flatMap( s => toLongOr(s)).map( _ < n) }
  def _lte(n: Long) = (jv: JValue) => { getString(jv).flatMap( s => toLongOr(s)).map( _ <= n) }
  def _gt(n: Long)  = (jv: JValue) => { getString(jv).flatMap( s => toLongOr(s)).map( _ > n) }
  def _gte(n: Long) = (jv: JValue) => { getString(jv).flatMap( s => toLongOr(s)).map( _ >= n) }
  def _between(b: Long, t: Long) = (jv: JValue) => { getString(jv).flatMap( s => toLongOr(s)).map( n => b <= n && n <= t) }


  def _lt0(n: Double)  = (jv: JValue) => { getString(jv).flatMap( s => toDoubleOr(s)).map( _ < n) }
  def _lte0(n: Double) = (jv: JValue) => { getString(jv).flatMap( s => toDoubleOr(s)).map( _ <= n) }
  def _gt0(n: Double)  = (jv: JValue) => { getString(jv).flatMap( s => toDoubleOr(s)).map( _ > n) }
  def _gte0(n: Double) = (jv: JValue) => { getString(jv).flatMap( s => toDoubleOr(s)).map( _ >= n) }
  def _between0(b: Double, t: Double) = (jv: JValue) => { getString(jv).flatMap( s => toDoubleOr(s)).map( n => b <= n && n <= t) }

  def between[T: Numeric](from: T, to: T)(jv: JValue)
  : Either[EvalError, Boolean]
  = jv match {
    case _@JLong(n) => Right(from.toLong <= n && n <= to.toLong)
    case _@JDouble(n) => Right(from.toDouble <= n && n <= to.toDouble)
    case _@JDecimal(n) => Right(BigDecimal(from.toDouble) <= n && n <= BigDecimal(to.toDouble))
    case _@JInt(n) => Right(from.toLong.toBigInt <= n && n <= to.toLong.toBigInt)
    case _ => EvalError(s"not numeric: $jv")
  }

  def gt[T: Numeric](bottom: T)(jv: JValue)
  : Either[EvalError, Boolean]
  = jv match {
    case _@JLong(n) => Right(bottom.toLong < n)
    case _@JDouble(n) => Right(bottom.toDouble < n)
    case _@JDecimal(n) => Right(BigDecimal(bottom.toDouble) < n)
    case _@JInt(n) => Right(bottom.toLong.toBigInt < n)
    case _ => EvalError(s"not numeric: $jv")
  }

  def gte[T: Numeric](bottom: T)(jv: JValue)
  : Either[EvalError, Boolean]
  = jv match {
    case _@JLong(n) => Right(bottom.toLong <= n)
    case _@JDouble(n) => Right(bottom.toDouble <= n)
    case _@JDecimal(n) => Right(BigDecimal(bottom.toDouble) <= n)
    case _@JInt(n) => Right(bottom.toLong.toBigInt <= n)
    case _ => EvalError(s"not numeric: $jv")
  }

  def lt[T: Numeric](bottom: T)(jv: JValue) = gte(bottom)(jv).map(b => !b)

  def lte[T: Numeric](bottom: T)(jv: JValue) = gt(bottom)(jv).map(b => !b)

}

