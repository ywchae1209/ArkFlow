package transform.jsonValidator

import transform.jsonValidator.syntaxObject.rule.ObjectRuleParser._
import transform.utils.StringUtil.show

////////////////////////////////////////////////////////////////////////////////
// unit test code
////////////////////////////////////////////////////////////////////////////////
object SpecObjectRule extends App {


  val s =
    "(${key:11} < ${key_2}) && 0 < ( ${key:1} / ${key_2} *  ${key_3} + ${key_4} ) / ${key_5}  + ${key_6} * 12 - ${key_11} + ${key_12} * ${key_13__4:5} + ${key_14} / ${key:15} + ${key:16} "

  val m = Map(
    "key_0" -> 1,
    "key_2" -> 2,
    "key_3" -> 3,
    "key_4" -> 4,
    "key_5" -> 6,
    "key_6" -> 6,
    "key_11" -> 11,
    "key_12" -> 12,
    "key_13" -> 13,
    "key_14" -> 14,
    "key_15" -> 15,
    "key_16" -> 16,
  )

  val f = (s: String) => m.get(s).toRight(s"not-found: $s")

  val g = f.andThen(_.map(_.toLong))

  val expr = compile(s)

  show(expr)

  val z = expr.flatMap(_.evaluateWith(g))

  expr.foreach(e => println(e.expressionWith(g)))

  show(z)
}
