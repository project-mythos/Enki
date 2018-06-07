package Enki
import scala.util.hashing.MurmurHash3

trait KeyHasher {
  def hashKey(key: Array[Byte]): Long 
}


object KeyHasher {
  val FNV1A_64 = new KeyHasher {

    def hashKey(key: Array[Byte]): Long = {

      val PRIME: Long = 1099511628211L
      var i = 0

      val len = key.length
      var rv: Long = 0xcbf29ce484222325L
      while (i < len) {
        rv = (rv * PRIME) ^ (key(i) & 0xff)
        i += 1
      }
      rv


    }

  }


  val MURMUR3 = new KeyHasher {
    def hashKey(key: Array[Byte]) = {
      MurmurHash3.bytesHash(key).toLong
    }
  }
}
