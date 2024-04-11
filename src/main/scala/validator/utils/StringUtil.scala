package validator.utils

import fastparse.{P, Parsed, parse}

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.util.zip.GZIPInputStream
import scala.io.Codec
import scala.io.Source.{createBufferedSource, fromInputStream}
import scala.util.matching.Regex

object StringUtil {

  private val notPermittedInJsonValue = Seq( '\b','\f','\n','\r','\t')

  ////////////////////////////////////////////////////////////////////////////////

  def lift[T](o: T): Option[T] = if (o == null) None else Some(o)
  def lift(s: String): Option[String] = if (s == null || s.isEmpty) None else Some(s)
  def decodeUrl(s: String) = lift(s).map( java.net.URLDecoder.decode(_, StandardCharsets.UTF_8))

  ////////////////////////////////////////////////////////////////////////////////
  // zip      todo :: for more readability
  ////////////////////////////////////////////////////////////////////////////////
  def args2map(args: Array[String], keywords: String)
  : Map[String, Array[String]] = {

    def toArr(args: Array[String]) = args.foldLeft(Array[Array[String]]()) {
      case (b, arg) =>
        if (arg.startsWith("-")) {
          Array(arg) +: b
        } else {
          b.headOption foreach { _ => b.update(0, b.head :+ arg) }
          b
        }
    }

    val opts = keywords.split("\\|")

    toArr(args).flatMap { arg =>
      opts
        .find(opt => arg.headOption.exists(ar => ar.dropWhile(_ == '-').equals(opt)))
        .map(_ -> arg.drop(1))
    }.toMap
  }


  ////////////////////////////////////////////////////////////////////////////////
  // zip
  ////////////////////////////////////////////////////////////////////////////////
  def unzipBytes(bytes: ByteArrayInputStream): String = {

    val is = new GZIPInputStream(bytes)
    val codec: Codec = StandardCharsets.UTF_8
    val ret = //scala.io.Source.fromInputStream(is).mkString
      createBufferedSource(is,
        bufferSize = 65536,
        reset = () => fromInputStream(is)(codec),
        close = () => is.close())(codec)
        .mkString
    is.close()
    ret
  }

  ////////////////////////////////////////////////////////////////////////////////
  // unescaping
  ////////////////////////////////////////////////////////////////////////////////
  def unescapeUnicode(str: String): String =

    """\\u+([0-9a-fA-F]{4})""".r.replaceAllIn(str,
      m => Integer.parseInt(m.group(1), 16).toChar match {
        case '\\' => """\\"""
        case '$' => """\$"""
        case c => c.toString
      })

  def unescapeString(s: String): String = {
    StringContext.processEscapes(unescapeUnicode(s))
  }


  ////////////////////////////////////////////////////////////////////////////////
  // String Interpolation
  ////////////////////////////////////////////////////////////////////////////////
  private val capture_key = "\\$\\{([a-zA-Z0-9.]+)}".r

  val capture_arr = "\\$\\{([a-zA-Z0-9.]+)\\[([a-zA-Z0-9.]+)\\]}".r

  def interpolate(f: String => Option[String], keys: Regex = capture_key)
                 (str: String)
  : String = keys.replaceAllIn(str, m => f(m.group(1)).getOrElse(""))

  def interpolate2(prefix: String, surfix: String, reps : Seq[String], keys: Regex = capture_arr)
                  (str: String)
  : Seq[String] = {
    reps.map( s =>
      keys.replaceAllIn(str, m => {
        val k = m.group(1)
        val v = m.group(2)
        if(prefix == k && surfix == v) s else ""
      })
    )
  }

  def containKey(str: String, keys: Regex = capture_key)
  : Boolean = keys.findFirstIn(str).isDefined

  ////////////////////////////////////////////////////////////////////////////////
  // String Parse Util
  ////////////////////////////////////////////////////////////////////////////////

  def toFailMessage(s: String, p: Int, extra: Parsed.Extra) = {

    val msg = s"syntax error :: $s"

    val sp = List.fill(s.length)("-").mkString
    val p1 = List.fill(p)("").mkString("", " ", "^^^")
    println(s"$msg\n$sp\n$s\n$sp\n$p1")

    msg
  }

  def parseWith[T](p: P[_] => P[T])(s: String): Either[String, T] =
    parse[T](s, p) match {
      case Parsed.Success(r, _) => Right(r)
      case Parsed.Failure(_, p, extra) =>
        Left( toFailMessage( s, p, extra))
      case _ =>
        val msg = s"Invalid expression : $s"
        println(msg)
        Left(msg)
    }

  def show[T](t: T): T = {
    println("show: \t" + t)
    t
  }

}
