package Enki_DT

trait Clock {
  type T

  def increment(t: T): T
  def eq(l: T, r: T): Boolean

  def empty(id: String): T
  def compare(l: T, r: T): Long

  def dominates(l: T, r: T): Boolean = {
    compare(l, r) > 0 
  }

}


case class Dot(id: String, counter: Long)


object Dot extends Clock {
  type T = Dot

  def increment(dot: Dot) = {
    dot.copy(counter=dot.counter + 1 )
  }


  def eq(l: Dot, r: Dot) = {
    l.id == r.id & l.counter == r.counter 
  }


  def empty(id: String) = {
    Dot(id, 0L) 
  }


  def sorting(l: Dot, r: Dot): Boolean = {
    if (l.id != r.id) return l.id < r.id
    l.counter < r.counter 
  }

  def compare(l: Dot, r: Dot): Long = {
    l.counter - r.counter
  }

}


case class HybridClock(id: String, counter: Long, stamp: Long)

object HybridClock extends Clock {
  type T = HybridClock

  def increment(hc: HybridClock) = {

    val ts = System.currentTimeMillis
    val ctr = hc.counter + 1L
    hc.copy(counter = ctr, stamp = ts)

  }


  def empty(id: String) = {

    val ts = System.currentTimeMillis
    val ctr = 0L

    HybridClock(id, ctr, ts)
  }



  def eq(l: HybridClock, r: HybridClock): Boolean = {
    l.id == r.id && l.counter == r.counter && l.stamp == r.stamp
  }


  def compare(l: T, r: T) = {
    val ctr_diff = l.counter - r.counter

    if (ctr_diff != 0) ctr_diff
    else l.stamp - r.stamp

  }

}
