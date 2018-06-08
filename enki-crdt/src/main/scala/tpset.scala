package Enki_DT

import Enki.PipeOps._
import scala.concurrent.duration._


case class TPSet[T](entries: Set[T], tombstones: Set[T])

object TPSet extends GCSet[TPSet, Int] {



  def add[T](s: TPSet[T], t: T) = {
    s.entries + t |> {e1 => s.copy(entries=e1)}
  }


  def delete[T](s: TPSet[T], t: T) = {
    val e = s.entries.filter(x => x != t)
    val ts = s.tombstones + t
    s.copy(entries=e, tombstones=ts)
  }


  def query[T](s: TPSet[T]) = s.entries


  def merge[T](l: TPSet[T], r: TPSet[T]) = {
    val e = l.entries ++ r.entries
    val ts = l.tombstones ++ r.tombstones
    l.copy(entries=e, tombstones=ts)
  }


  def gc[T](s: TPSet[T], max: Int) = {

    def evict(set: Set[T]) = {
      val l = set.size
      if (l > max) (l - max) |> set.toList.drop |> (x => x.toSet)
      else set 
    }


    val e1 = evict( s.entries )
    val ts = evict( s.tombstones )

    TPSet(e1, ts)

  }



}



/** Evicts Expired Tombstone after TTL has elapsed*/

case class LWWEntry[T](element: T, ts: Duration)
case class LWWSet[T](entries: Set[LWWEntry[T]], tombstones: Set[LWWEntry[T]])


object LWWSet extends GCSet[LWWSet, Duration] {

  def add[T](s: LWWSet[T], t: T) = {
    val e = s.entries + LWWEntry(t, getTime)
    s.copy(entries = e)
  }



  def getTime: Duration = {
    System.currentTimeMillis |> {x => Duration(x, MILLISECONDS)}
  }




  def gc[T](s: LWWSet[T], ttl: Duration) = {

    val cutoff = getTime - ttl

    val ts = s.tombstones filter {x => x.ts >= cutoff  }
    s.copy(tombstones = ts)
  }


  def delete[T](s: LWWSet[T], t: T) = {

    val e = s.entries.filterNot(i => i.element == t)
    val c = getTime

    val stones = s.tombstones + LWWEntry(t, c)
    s.copy(entries = e, tombstones = stones)
  }


  def query[T](s: LWWSet[T]) = {
    def p(x: LWWEntry[T]) = s.tombstones.exists(y => y.element == x.element)
    s.entries.filterNot( x => p(x) ) map {x => x.element}
  }



  def merge[T](l: LWWSet[T], r: LWWSet[T]) = {


    //val adjustment = l.ts - r1.ts

    val ts = l.tombstones ++ r.tombstones

    val e = l.entries ++ r.entries |> { es =>

      def p(x: LWWEntry[T]) = {
        ts.exists(y => y.element == x.element && x.ts < y.ts)
      }

      es.filterNot( x => p(x) )
    }

    l.copy(entries = e, tombstones = ts)
  }


}
