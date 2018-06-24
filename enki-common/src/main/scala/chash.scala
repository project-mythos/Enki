package Enki.CHash

import Enki.JCH
import java.net.SocketAddress

object Sharder {


  def lookup[T](shards: Vector[T], id: Long) = {
    val b = JCH.chash(id, shards.size)
    shards(b)
  }


  def slice[T](shards: Vector[T], i: Int, f: Int) = {
    val size = shards.size
    val last = shards.size - 1


    val max =
      if (f <= size) i + (f - 1)
      else i + (last)


    if (max <= last) {
      (i to max) map {x => shards(x)}
    }

    else {

      val diff = max - size
      

      val p1 = (i to last) map {x => shards(x)}
      val p2 = (0 to diff) map {x => shards(x) }
      p1 ++ p2
    }


  }




  def lookup[T](shards: Vector[T], id: Long, f: Int) = {
    val i = JCH.chash(id, shards.size)
    slice(shards, i, f)
  }


}
