package Enki

import java.util.concurrent.atomic.AtomicBoolean

import com.twitter.util.{Future, Return, Throw}
import com.twitter.concurrent.AsyncQueue
import scala.collection.JavaConverters._
//import java.util.concurrent.atomic.{AtomicLong} 

case class Pool[T](
  list: AsyncQueue[T],
  create: () => Future[T],
  check: T => Boolean, 
  close: T => Unit
) {
  val closed = new AtomicBoolean(false)

  def open = closed.get != true
  def setClosed = closed.set(true)

}

object Pool {

  def make[T](
    size: Int,
    make: () => Future[T],
    check: T => Boolean,
    close: T => Unit
  ): Pool[T] = {

    val list = new AsyncQueue[T](size)
    Pool(list, make, check, close)

  }

  def createItem[T](pool: Pool[T]) = {
    pool.create()
  }



  def acquire[T](pool: Pool[T]): Future[T] = {

    if (
      (pool.list.size == 0) && (pool.open)
    )  { createItem(pool) }

    else if (pool.open != true) {
      Future.exception{ new Exception("Error Pool was shut down") }

    }

    else pool.list.poll()

  }

  def release[T](pool: Pool[T], item: T) = {
    pool.list.offer(item)
  }

  def use[T, U](pool: Pool[T])(op: T => Future[U]): Future[U] = {

    acquire(pool) flatMap {item =>
      val f = op(item)

      f onFailure {e =>
        if ( pool.check(item) == false) pool.close(item)
        else release(pool, item) 
      }

      f onSuccess { x => release(pool, item) }
      f
    }

  }


  def destroy[T](pool: Pool[T]): Future[Unit] = {
    pool.setClosed

    val close = pool.list.fail(new Exception("Queue Destroyed"), false)
    val l = pool.list.drain()

    l match {
      case Return(items) =>
        items.foreach {c => pool.close(c) }
        Future.Done
      case Throw(e) => Future.exception(e)
    }
  }


}

