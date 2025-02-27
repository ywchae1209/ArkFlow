package transform.jsonValidator.syntaxValue.rule

import org.json4s.{JDecimal, JDouble, JInt, JLong, JValue}
import transform.jsonValidator.EvalError.StringWithSyntaxPower
import transform.jsonValidator.syntaxValue.rule.Predicate.JPredicate
import transform.jsonValidator._
import transform.utils.JsonUtil._
import transform.utils.StringUtil
import transform.utils.StringUtil.{show, wordy}

import scala.collection.immutable.{HashMap, HashSet}
import scala.math.BigDecimal.long2bigDecimal
import scala.math.Numeric.Implicits.infixNumericOps

case class Predicate (check: Either[SyntaxErrorValue, JPredicate ],
                      name: String,
                      args: List[String] ) {

  def getSyntaxError: Option[SyntaxErrorValue] = check.fold( Option(_), _ => None)

  private lazy val mayError = check.fold( EvalSyntaxErr[JPredicate], Right(_) )

  def apply(jv: JValue) : Either[EvaluationError, Boolean] = {
    mayError.flatMap( f =>
    {
      val ret = f(jv)

//      ret.left.foreach{ s =>
//        wordy( args.mkString( s"Predicate :: $name(", ", ", s") == $s : i'll judge as false"))
//      }

      ret.fold( _ => Right(false), r => Right(r))
    } )
  }
}

object Predicate {
  type JPredicate = JValue => Either[EvaluationError, Boolean]
}

case class PredicateMaker( name: String,
                           make: Seq[String] => Either[SyntaxErrorValue, JPredicate]) {
  override def toString: String = s"PredicateMaker($name)"

  def apply(s: Seq[String])
  : Either[SyntaxErrorValue, JPredicate] = make(s)
}

case class FunctionTable(functionTable: Map[String, PredicateMaker]) {

  def apply(name: String, args: Seq[String]) : Predicate = {
    val check =
      functionTable.get(name)
        .map(_(args))
        .getOrElse( Left( UnknownFunction(name, args)) )

    Predicate(check, name, args.toList)
  }

  def wordy: String =
    StringUtil.show(
      functionTable.keys
        .toList.sorted.mkString("=== Functions ===\n\t","\n\t","\n============\n")
    )
}

object FunctionTable {
  def apply( predicateMaker: PredicateMaker*): FunctionTable = {

    val m = predicateMaker.groupBy(_.name)
    val f = m.filter(_._2.length > 1)
    if( f.nonEmpty )
      show( f.keys.mkString("Syntax Error:: Duplicate function-name: [",", ", "]" ))

    new FunctionTable(
      HashMap( m.map( kv => kv._1 -> kv._2.head).toList:_* )
    )
  }

}

////////////////////////////////////////////////////////////////////////////////

object PredicateMaker {

  private def toStringOr(s: String) = Right(s)

  private def ofLen(name: String, l: Seq[String], n: Int)
  : Either[SyntaxErrorValue, Seq[String]] =
    if( l.length == n) Right(l) else Left(ArgumentCountError(name, l, n))

  private def ofType0[T](name: String,
                         conv: String => Either[String, T])
                        (toPred: Seq[T] => JPredicate)
  = PredicateMaker( name, args => {
        val (lefts, rights) = args.map(conv).partitionMap(identity)
        if (lefts.nonEmpty) Left(ArgumentTypeError(name, args, lefts.head))
        else Right(rights)
      }.map(toPred) )


  private def ofType[T](name: String,
                        len: Int,
                        conv: String => Either[String, T])
                       (toPred: Seq[T] => JPredicate)
  = PredicateMaker( name, args =>
    ofLen(name, args, len)
      .flatMap { l =>
        val (lefts, rights) = l.take(len).map(conv).partitionMap(identity)
        if (lefts.nonEmpty) Left(ArgumentTypeError(name, args, lefts.head))
        else Right(rights)
      }
      .map(toPred) )

  private def toLongOr(s: String) = s.toLongOption.toRight((s"$s is not long"))
  private def toDoubleOr(s: String) = s.toDoubleOption.toRight((s"$s is not double"))

  private def ofLong0(n: String)( f: Seq[Long] => JPredicate) = ofType0[Long](n, toLongOr)(f)
  private def ofDouble0(n: String)( f: Seq[Double] => JPredicate) = ofType0[Double](n, toDoubleOr)(f)
  private def ofString0(n: String)( f: Seq[String] => JPredicate) = ofType0[String](n, toStringOr)(f)

  private def ofLong(n: String, l: Int)( f: Seq[Long] => JPredicate) = ofType[Long](n, l, toLongOr)(f)
  private def ofDouble(n: String, l: Int)( f: Seq[Double] => JPredicate) = ofType[Double](n, l, toDoubleOr)(f)
  private def ofString(n: String, l: Int)( f: Seq[String] => JPredicate) = ofType[String](n, l, toStringOr)(f)

  ////////////////////////////////////////////////////////////////////////////////
  private val _any        = ofString0("_any")(_ => _ => Right(true))
  private val _notNull   = ofString0("_notNull")(_ => j => Right(j.notNull) )

  private val gt          = ofLong("gt", 1)( args => Calc.gt(args.head)(_) )
  private val lt          = ofLong("lt", 1)( args => Calc.lt(args.head)(_) )
  private val gte         = ofLong("gte", 1)(args => Calc.gte(args.head)(_) )
  private val lte         = ofLong("lte", 1)(args => Calc.lte(args.head)(_) )
  private val between     = ofLong("between", 2)(args => Calc.between(args.head, args(1))(_) )

  private val gt0         = ofDouble("gt0", 1)(args => Calc.gt(args.head)(_) )
  private val lt0         = ofDouble("lt0", 1)(args => Calc.lt(args.head)(_) )
  private val gte0        = ofDouble("gte0", 1)(args => Calc.gte(args.head)(_) )
  private val lte0        = ofDouble("lte0", 1)(args => Calc.lte(args.head)(_) )
  private val between0    = ofDouble("between0", 2)(args => Calc.between(args.head, args(1))(_) )

  ////////////////////////////////////////////////////////////////////////////////

  private val _isString   = ofString0("_isString")(_ => Calc._isString)
  private val _notEmptyString = ofString0("_notEmptyString")(_ => Calc._notEmptyString)

  private val _oneOf      = ofString0("_oneOf")(args => Calc._oneOf(args)(_))
  private val _digit      = ofString0("_digit")(args => Calc._digitOr(Nil))
  private val _digitOr    = ofString0("_digitOr")(args => Calc._digitOr(args))
  private val _charsIn    = ofString0("_charsIn")(args => Calc._charsIn(args))

  private val _notOneOf    = ofString0("_notOneOf")(args => Calc._notOneOf(args))      // todo
  private val _charsNotIn  = ofString0("_charsNotIn")(args => Calc._charsNotIn(args))  // todo

  private val _regex      = ofString0("_regex")(args => Calc._regex(args))
  private val _date       = ofString0("_date")(args => Calc._date(args))

  private val _length     = ofLong0("_length")(args => Calc._length(args))
  private val _longerThan = ofLong("_longerThan", 1)(args => Calc._longerThan(args.head))
  private val _shorterThan= ofLong("_shorterThan", 1)(args => Calc._shorterThan(args.head))

  private val _lt         = ofLong("_lt", 1)(args => Calc._lt(args.head))
  private val _gt         = ofLong("_gt", 1)(args => Calc._gt(args.head))
  private val _lte        = ofLong("_lte", 1)(args => Calc._lte(args.head))
  private val _gte        = ofLong("_gte", 1)(args => Calc._gte(args.head))
  private val _between    = ofLong("_between", 2)(args => Calc._between(args.head, args(1)))

  private val _lt0        = ofDouble("_lt0", 1)(args => Calc._lt0(args.head))
  private val _gt0        = ofDouble("_gt0", 1)(args => Calc._gt0(args.head))
  private val _lte0       = ofDouble("_lte0", 1)(args => Calc._lte0(args.head))
  private val _gte0       = ofDouble("_gte0", 1)(args => Calc._gte0(args.head))
  private val _between0   = ofDouble("_between0", 2)(args => Calc._between0(args.head, args(1)))

  ////////////////////////////////////////////////////////////////////////////////

  val UserFunctionTable: FunctionTable = FunctionTable(
    _any, _notNull,
    gt, lt, gte, lte, between,
    gt0, lt0, gte0, lte0, between0,
    _lt, _gt, _lte, _gte, _between,
    _lt0, _gt0, _lte0, _gte0, _between0,
    _isString, _notEmptyString, _oneOf, _notOneOf, _digit, _digitOr, _charsIn, _charsNotIn,  _regex,
    _date, _length, _longerThan, _shorterThan,
  )

}

object Calc {

  private def toLongOr(s: String) = s.toLongOption.toRight(s"$s is not long".evalError)
  private def toDoubleOr(s: String) = s.toDoubleOption.toRight(s"$s is not double".evalError)

  val getString: JValue => Either[EvalError, String]
  = (jv: JValue) => jv.asString.fold( EvalError(_), Right(_))

  val _isString: JValue => Either[EvalError, Boolean]
  =  jv => getString(jv).map( _ => true)

  val _notEmptyString: JValue => Either[EvalError, Boolean]
  =  jv => getString(jv).map( _ != "" )

  def _oneOf(args: Seq[String]): JValue => Either[EvalError, Boolean] = (jv: JValue) => {
    getString(jv).map( s => args.contains(s))
  }

  def _notOneOf(args: Seq[String]): JValue => Either[EvalError, Boolean] = (jv: JValue) => {
    getString(jv).map( s => !args.contains(s))
  }

  def _digitOr(args: Seq[String]): JValue => Either[EvalError, Boolean] = (jv: JValue) => {
    val set = HashSet.apply( args.mkString.toList.distinct: _*)
    getString(jv).map( s => s.forall(c => c.isDigit || set.contains(c)) )
  }

  def _charsNotIn(args: Seq[String]): JValue => Either[EvalError, Boolean] = (jv: JValue) => {
    val set = HashSet.apply( args.mkString.toList.distinct: _*)
    getString(jv).map( s => s.forall(c => !set.contains(c)) )
  }

  def _charsIn(args: Seq[String]): JValue => Either[EvalError, Boolean] = (jv: JValue) => {
    val set = HashSet.apply( args.mkString.toList.distinct: _*)
    getString(jv).map( s => s.forall(c => set.contains(c)) )        // bug-fix
  }

  def _regex(args: Seq[String]): JValue => Either[EvalError, Boolean] = (jv: JValue) => {
    val regex = args.map( _.r)
    getString(jv).map( s => regex.exists( _.findFirstIn(s).nonEmpty) )
  }

  def _date( args: Seq[String]): JValue => Either[EvalError, Boolean] = (jv: JValue) => {
    val map = Map("yyyy" -> "[12][0-9][0-9][0-9]", "mm" -> "[01][0-9]", "dd" -> "[0-3][0-9]")
    val sts = args.map( s => StringUtil.interpolate(map.get)(s) )
    val regex = sts.map( _.r)

    getString(jv).map( s => regex.exists( _.findFirstIn(s).nonEmpty))
  }

  def _length(args: Seq[Long]): JValue => Either[EvalError, Boolean] = (jv: JValue) => {
    getString(jv).map( s => args.contains(s.length) )
  }

  def _longerThan(args: Long): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).map( _.length > args) }
  def _shorterThan(args: Long): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).map( _.length < args) }

  def _lt(n: Long): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).flatMap(s => toLongOr(s)).map( _ < n) }
  def _lte(n: Long): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).flatMap(s => toLongOr(s)).map( _ <= n) }
  def _gt(n: Long): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).flatMap(s => toLongOr(s)).map( _ > n) }
  def _gte(n: Long): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).flatMap(s => toLongOr(s)).map( _ >= n) }
  def _between(b: Long, t: Long): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).flatMap(s => toLongOr(s)).map(n => b <= n && n <= t) }


  def _lt0(n: Double): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).flatMap(s => toDoubleOr(s)).map( _ < n) }
  def _lte0(n: Double): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).flatMap(s => toDoubleOr(s)).map( _ <= n) }
  def _gt0(n: Double): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).flatMap(s => toDoubleOr(s)).map( _ > n) }
  def _gte0(n: Double): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).flatMap(s => toDoubleOr(s)).map( _ >= n) }
  def _between0(b: Double, t: Double): JValue => Either[EvalError, Boolean] = (jv: JValue) => { getString(jv).flatMap(s => toDoubleOr(s)).map(n => b <= n && n <= t) }

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

  def lt[T: Numeric](bottom: T)(jv: JValue): Either[EvalError, Boolean] = gte(bottom)(jv).map(b => !b)

  def lte[T: Numeric](bottom: T)(jv: JValue): Either[EvalError, Boolean] = gt(bottom)(jv).map(b => !b)
}

