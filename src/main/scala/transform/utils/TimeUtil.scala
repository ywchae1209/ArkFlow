package transform.utils

import java.time.{Instant, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter

object TimeUtil {

  val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss SSS")

  def current(): String = timeString(currentTime)

  def currentTime: Long = System.currentTimeMillis()

  def dateTime(tm: Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(tm), ZoneId.systemDefault())

  def timeString(tm: Long): String = formatter.format(dateTime(tm))

  def elapsed(d1: Long, d2: Long): Long = d2 - d1

  ////////////////////////////////////////////////////////////////////////////////

  def timeLog[A](f: => A)(log: String => Unit = s => May.log(s, 0)): A = {
    val s = currentTime
    val ret = f
    val e = currentTime
    log(s"timeLog :: ${timeString(s)} ${timeString(e)} elapsed ${elapsed(s, e)}")
    ret
  }

}
