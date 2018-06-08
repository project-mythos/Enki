package Enki_DT

import scala.language.higherKinds

trait SetCRDT[A[_]] {

  def add[T](s: A[T], t: T): A[T]
  def delete[T](s: A[T], t: T): A[T]

  def merge[T](l: A[T], r: A[T]): A[T]
  def query[T](s: A[T]): Set[T]

}


trait GC_CRDT[A[_], M] {
  def gc[T](a: A[T], max: M): A[T]  
}


trait GCSet[A[_], M] extends GC_CRDT[A, M] with SetCRDT[A]


