package transform.utils

/** exception handling */

object May {

  def log(str: String, level: Int = 0): Unit = {
    println(str)
  }

  def state(str: => String)
  : Unit = {
    log(s"[ State ] ========== $str ==========")
  }

  ////////////////////////////////////////////////////////////////////////////////
  def maybe[A](a: => A)
  : Option[A] = {
    try
      Some(a)
    catch { case e: Throwable =>
      None
    }
  }

  def mayOr[A](a: => A)(str: => String)
  : Either[String, A] = {
    try Right(a)
    catch { case e: Throwable =>
      val m = s"[Exception] $str ${e.toString}"
      Left(m) }
  }

  def using[A <: AutoCloseable,B](f: => A, needClose: Boolean = true)
                                 (g: A => B): Option[B] = {
    maybe( f ).flatMap { a =>
      try{
        Some(g(a))
      } catch { case e: Throwable =>
        println(e.toString)
        None
      } finally {
        if(needClose)
          a.close()
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////////

  def warn[A](a: => Option[A])(str: => String)
  : Option[A] = {
    if (a.isEmpty)
      log(s"[ Warn ] : None = $str")
    a
  }

  def maybeInfo[A](a: => A)(str: => String)
  : Option[A] = {
    try Some(a) catch {
      case e: Throwable => println("[ Exception ]\t" + str, e); None
    }
  }


  def maybeWarn2[A](a: => A)(str: String = "[ maybeWarn ]")
  : Option[A] = {
    try Some(a) catch {
      case e: Throwable => println(str, e); None
    }
  }

  class Lazy[T]( calc0: () => T){
    private lazy val force = calc0()
    def apply(): T = force
  }

  object Lazy {
    def apply[T](cal0: () => T) = new Lazy(cal0)
    def unit[T](cal0: => T) = new Lazy(() => cal0)

    def map2[T,U,V]( lt: Lazy[T], lu: Lazy[U])(f: (T, U) => V): Lazy[V]
    = Lazy( () => f(lt.apply(), lu.apply()) )

  }

}
