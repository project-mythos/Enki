package Enki
import scala.concurrent.{ExecutionContext, Future}


class Pipe[A](a: A) {
  def |>[B](f: A => B) = f(a)
}

object Pipe {
  def apply[A](v: A) = new Pipe(v)
}

object PipeOps {
  import scala.language.implicitConversions
  implicit def toPipe[A](a: A) = Pipe(a)
}


case class SyncVar[T](seed: T) {
  private var state = seed
  import PipeOps._ 

  def get = seed

  def update(f: T => T) = {
    val s = get |> f
    synchronized {state = s}
  }


  def put(s: T) = synchronized {state = s}

}


object Path {

  type T = List[String]

  def toString(path: T): String = {
    path.mkString("/").replaceAll("/", "%2F")
  }

  def ofString(x: String): T = {
    val s = x.replaceAll("%2F", "/")
    s.split("/").toList
  }


}




class FutureExtensions[T](fut: Future[T]) {

  def ensure(f: => Unit)(implicit ctx: ExecutionContext): Future[T] = {
    fut onComplete(x => f )
    fut
  }

}


object FutureExtensions {
  def apply[T](fut: Future[T]) = new FutureExtensions(fut)
}


object FutureOps {
  implicit def toExtendedF[T](f: Future[T]) = FutureExtensions(f)
}

