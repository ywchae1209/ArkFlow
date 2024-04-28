package transform.jsonClassify

import transform.jsonClassify.patternMatching.{MultiPattern, Occurrence}

object SpecMultiPattern extends App {

  val sampleMessage =
    """
      |Functional Programming in Scala is an intriguing title. After all, Scala is generally called
      |a functional programming language and there are dozens of books about Scala on
      |the market. Are all these other books missing the functional aspects of the language?
      |To answer the question it’s instructive to dig a bit deeper.
      |What is functional programming? For me, that is a programming style that puts the focus on the functions in a program
      |. What are functions?
      |Here, we find a larger spectrum of definitions. While one definition often
      |admits functions that may have side effects in addition to returning a result, pure
      |functional programming restricts functions to be as they are in mathematics: binary
      |relations that map arguments to results.
      | Scala is an impure functional programming language in that it admits impure as
      |well as pure functions, and in that it does not try to distinguish between
      |these categories by using different syntax or giving them different types. It shares this property
      |with most other functional languages. It would be nice if we could distinguish pure
      |and impure functions in Scala, but I believe we have not yet found a way to do so that
      |is lightweight and flexible enough to be added to Scala without hesitation.
      | To be sure, Scala programmers are generally encouraged to use pure functions.
      |Side effects such as mutation, I/O, or use of exceptions are not ruled out, and they
      |can indeed come in quite handy sometimes, be it for reasons of interoperability, efficiency, or convenience.
      |But overusing side effects is generally not considered good
      |style by experts. Nevertheless, since impure programs are possible and even convenient to write in Scala,
      |there is a temptation for programmers coming from a more
      |imperative background to keep their style and not make the necessary effort to adapt
      |to the functional mindset. In fact, it’s quite possible to write Scala as if it were Java
      |without the semicolons.
      |
      |case class Occurrence( keyword: String, loc: Seq[Int]) {
      |  override def toString: String =
      |    loc.mkString( s"$keyword: ", ",", "" )
      |}
      |
      |case class MultiPattern(rules: Map[Int, RuleRoot], dictionary: Trie[Term]) {
      |
      |  def search(s: String): Iterable[(Int, String, Seq[Occurrence])] =  {
      |
      |    def matched(ms: Seq[(Term, Location)]) = {
      |      ms.groupBy(_._1.keyword).map{ case (k, v) ㅋㅊ=>
      |        Occurrence( k, v.map( _._2.from))
      |      }.toSeq
      |    }
      |
      |    val ret =
      |      dictionary
      |        .search(s)
      |        .groupBy( _._1.root)
      |        .flatMap { case (root, ms: Seq[(Term, Location)]) =>
      |          rules.get(root).flatMap { r =>
      |            val p = (i: Int) => ms.exists { case (t, l) => (t.id == i) }  // todo :: location check
      |            val f = Rule.foldWith(r.rule)(p, () => matched(ms))
      |            f.map( (root, r.string, _))
      |          }
      |        }
      |    ret
      |  }
      |}
      |""".stripMargin

  val keywords1 =
    sampleMessage.split("\\s+")
      .map(s => s.filterNot(c => "_\\{}():=>[]!~\",|/'`&?.".contains(c)))
      .filter(_.length > 2)
      .distinct
      .map(s => s"'$s'")
      .grouped(2)
      .map(t =>
        if (t.isDefinedAt(1)) t(0) + "~" + t(1)
        else t(0)
      )
      .toSeq

  //  val keywords2 = Seq(
  //    " ( ('impure' ~ 'language') | 'program' ) & 'def' ",
  //    "'language' ~ 'they' ~ 'language'",
  //  )

  val keywords = keywords1 //++ keywords2

  val pat = keywords.zipWithIndex.map(i => i._2 -> i._1)

  keywords foreach println
  println(keywords.size)

  val mp = {
    MultiPattern(pat: _*)
  }

  println()
  println(s"src.size = ${sampleMessage.length}")
  println(s"rule.size = ${pat.length}")
  println()

  val result: Iterable[(Int, String, Seq[Occurrence])] = mp.search(sampleMessage)
  mp.search(sampleMessage).foreach(println)
}
