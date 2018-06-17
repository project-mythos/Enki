package Enki
import scala.language.implicitConversions

class Pipe[A](a: A) {
  def |>[B](f: A => B) = f(a)
}


object PipeOps {
  implicit def toPipe[A](a: A) = new Pipe(a)
}


case class SyncVar[T](seed: T) {
  private var state = seed

  def get = state

  def update(f: T => T) = {
    val s = f(get)
    synchronized {state = s}
    s
  }


  def put(s: T) = synchronized {state = s}

}


object Path {

  type T = List[String]

  def to_string(path: T): String = path.mkString("/")

  def fromString(p: String) = p.split("/")

  def pct_encode(path: T): String = {
    path.mkString("/").replaceAll("/", "%2F")
  }

  def pct_decode(x: String): T = {
    val s = x.replaceAll("%2F", "/")
    s.split("/").toList
  }


}

