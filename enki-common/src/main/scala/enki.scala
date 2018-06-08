package Enki

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try
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



class PromiseExt[T](p: Promise[T]) {

  def setValue(t: T) = {
    val a = Try {t}
    p.complete(a)
  }



}


object PromiseOps {
  implicit def toPromiseExt[T](p: Promise[T]) = {
    new PromiseExt(p)
  }
}


class FutureExtensions[T](fut: Future[T]) {

  def ensure(f: => Unit)(implicit ctx: ExecutionContext): Future[T] = {
    fut onComplete(x => f )
    fut
  }

}


object FutureOps {
  implicit def toExtendedF[T](f: Future[T]) = new FutureExtensions(f)
}

