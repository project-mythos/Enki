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

