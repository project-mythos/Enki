package Enki

case class Entry[T](element: T, dot: Dot)
case class ORSet[T](local: Dot, entries: Set[Entry[T]], dots: Set[Dot])


object ORSet extends SetCRDT[ORSet]{



  def empty[T](id: String) = {
    ORSet(
      Dot.empty(id),
      Set[Entry[T]](),
      Set[Dot]()
    )

  }

  def add[T](orset: ORSet[T], e: T) = {
    val local = Dot.increment(orset.local) 
    val entries = orset.entries + Entry(e, local)
    val dots = orset.dots + local

    orset.copy(local=local, entries=entries, dots=dots)
  }


  def delete[T](orset: ORSet[T], e: T) = {
    val local = Dot.increment(orset.local)
    val entries = orset.entries.filter (x => x.element != e)
    orset.copy(local=local, entries=entries) 
  }


  def query[T](orset: ORSet[T]): Set[T] = {
    orset.entries.map(x => x.element) 
  }


  def toAdd[T](l: ORSet[T], r: ORSet[T]) = {

    l.entries.filter {x =>
      val has_e = r.entries.contains(x) != true
      val has_d = r.dots.contains(x.dot) != true
      has_e && has_d 
    }

  }


  def toRemove[T](l: ORSet[T], r: ORSet[T]) = {

    l.entries.filter { x =>
      val is_dot = r.dots.contains(x.dot)
      val is_e = r.entries.contains(x) != true
      is_dot && is_e 
    }

  }

  def merge[T](l: ORSet[T], r: ORSet[T]) = {
    val e1 = toAdd(r, l) 
    val e2 = l.entries -- toRemove(l, r)

    val entries = e1 ++ e2
    val dots = l.dots ++  r.dots

    l.copy(entries=entries, dots=dots)
  }


}
