package transform.jsonClassify

import transform.jsonClassify.patternMatching.rule.MPRuleAST.Rule

////////////////////////////////////////////////////////////////////////////////
// note :: testing code for implementer
object SpecMPRuleAST extends App {

  val s =
    """ !('this' ~ 'is' ~ 'suspicious' ~ 'case') | !(!(!'test' & !{!('testing rule')})) |
      | !^'exact'$ | !'contain' | !'end'$ | !^'start' |
      | { !('a' & 'b') & ('c' | 'd') } |
      | ! { !('a' & 'b') | ('c' | 'd') } & ( 'e' | 'f' ) |
      | ('a' & ( 'b' & 'c' & ('d' & 'e'))) |
      | (((( 'a' & 'b' ) & 'c') & 'd' ) & 'e')
      |""".stripMargin

  val s1 = "!('a' & !( 'b' | !'c')) | ((!(  !( 'a' & 'b' ) & 'c') | 'd' ) & 'e') "
  val r = Rule(1000, s)

  println(r)


}
