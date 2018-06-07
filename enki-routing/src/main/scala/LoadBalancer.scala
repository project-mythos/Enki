package Enki.LoadBalancer

import scala.util.Random
import scala.concurrent.{Future, ExecutionContext}
import Enki.CHash

import scala.math.Ordering
import scala.util.Random

import java.util.concurrent.atomic.AtomicLong
import scala.math
import Enki.FutureOps._

import Enki.KeyHasher.{FNV1A_64, MURMUR3}

trait Status
case object Alive extends Status
case object Dead extends Status



trait LB[T] {

  def pick(nodes: Vector[T]): T
  def use[U](nodes: Vector[T], f: T => Future[U])(implicit ec: ExecutionContext): Future[U]
}




trait RoutingLB[T] {
  def pick(nodes: Vector[T], key: Array[Byte]): T
  def use[U](nodes: Vector[T], key: Array[Byte], f: T => Future[U])(implicit ec: ExecutionContext): Future[U]
}


case class LeastLoaded(host: String, id: String) {

  private val serverLoad = new AtomicLong(0L)

  def load: Long = serverLoad.get
  def incr = serverLoad.incrementAndGet
  def decr = serverLoad.decrementAndGet
}



object P2C extends LB[LeastLoaded] {

  def pick(nodes: Vector[LeastLoaded]) = {

    val c1 = nodes( Random.nextInt(nodes.size) )
    val c2 = nodes( Random.nextInt(nodes.size) )
   
    if (c1.load >= c2.load) c1
    else c2
  }


  def use[U](nodes: Vector[LeastLoaded], f: LeastLoaded => Future[U])(implicit ec: ExecutionContext): Future[U] = {
    val node = pick(nodes)
    node.incr
    f(node) ensure node.decr
  }


}





class CHashLeastLoaded(factor: Int) extends RoutingLB[LeastLoaded] {

  val hasher = FNV1A_64
  val S = CHash.Sharder

  def pick(nodes: Vector[LeastLoaded], key: Array[Byte]) = {
    val key1 = hasher.hashKey(key)
    val shards = S.lookup(nodes, key1, factor)
    shards.minBy(x => x.load)
  }

  def use[U](nodes: Vector[LeastLoaded], key: Array[Byte], f: LeastLoaded => Future[U])(implicit ec: ExecutionContext): Future[U] = {
    val node = pick(nodes, key)
    node.incr
    f(node) ensure node.decr
  }
  
}





object P2CPKG extends RoutingLB[LeastLoaded] {



  val hasher = FNV1A_64
  val hasher1 = MURMUR3

  def pick(nodes: Vector[LeastLoaded], key: Array[Byte]): LeastLoaded = {
    val c1 = nodes( hasher.hashKey(key).toInt % nodes.length )
    val c2 = nodes( hasher1.hashKey(key).toInt % nodes.length )


    if (c1.load >= c2.load) c1
    else c2
  }

  def use[U](nodes: Vector[LeastLoaded], key: Array[Byte], f: LeastLoaded => Future[U])(implicit ec: ExecutionContext) = {
    val node = pick(nodes, key)
    node.incr
    f(node) ensure node.decr
  }

}
