package transform.jsonValidator

import org.json4s.{JString, JValue}
import transform.jsonValidator.syntaxValue.rule.PredicateMaker.UserFunctionTable
import transform.jsonValidator.syntaxValue.rule.ValueRuleParser
import transform.utils.StringUtil.show

object SpecValueRule extends App {

  val s = "f(1) || (_myfuction(abc) && my() || this()) || " +
    "_oneOf('this is test', good) && gt(1) &&  " +
    "lt(2000) && _between( 1, 1000) || _isString && _longerThan(1) " +
    "|| _oneOf(a, b, b, 'this is \\'' ) && _regex(1,2,3) || _shorterThan(a)"

  // parsing to ValueFunction
  val r = ValueRuleParser.compile(s)

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
