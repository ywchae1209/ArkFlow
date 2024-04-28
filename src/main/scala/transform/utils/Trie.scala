package transform.utils

case class Location( from: Int, to: Int, remain: Boolean)

case class Trie[T](label: String,
                   link: Map[Char, Trie[T]],
                   values: Seq[T] ) {
  def search(s: String): Seq[(T, Location)] = Trie.search(this)(s)
  def searchExact(s: String) = search(s).filter( t => t._2.from == 0 && !t._2.remain ).map(_._1)
}

object Trie {

  import scala.collection.mutable.ListBuffer

  def apply[T](inputs: (String, T)*): Trie[T] = {

    def go(label: String = "^", values: List[T] = Nil)(inputs: (String, T)*)
    : Trie[T] = {

      val link =
        inputs
          .groupBy(_._1(0).toLower)     // for ignoreCase
          .map { case (char, spt) =>
            val (vs, sub) =
              spt
                .map(st => (st._1.tail, st._2))
                .partition(_._1.isEmpty)
            val nl = label + char
            val nv = vs.map(t => t._2).toList

            char -> go(nl, nv)(sub: _*) // recursive make
          }

      new Trie(label, link, values)
    }

    val always = inputs.filter(_._1 == "").map(_._2).toList
    val ins = inputs.filterNot(_._1 == "")

    go("^", always)(ins: _*)
  }

  def search[T](trie: Trie[T])(obj: String)
  : Seq[(T, Location)] = {

    val target = obj.toLowerCase

    // local mutation for performance..
    var from    = 0
    var i       = 0
    var done    = false
    var current = trie

    val buf = ListBuffer[(T, Location)](trie.values.map( _ -> Location(0,0,true)):_* )
    val to      = target.length

    while (from < to) {
      i = from
      done = false
      current = trie

      while (i < to && !done) {
        current.link.get(target.charAt(i)) match {
          case None =>
            done = true

          case Some(next) =>
            val l = Location(from, i+1, (i+1) < to)
            buf ++=  next.values.map ( t => t -> l)
            current = next
            i = i + 1
        }
      }
      from = from + 1
    }
    buf.toList
  }
}
