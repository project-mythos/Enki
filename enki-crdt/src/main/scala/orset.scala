package Enki_DT

//case class Entry[T](element: T, dot: Dot)
case class Entry[T](element: T, dots: Set[Dot])
case class ORSet[T](local: Dot, entries: Set[Entry[T]], dots: Set[Dot])


object ORSet extends SetCRDT[ORSet] {



  def empty[T](id: String) = {
    ORSet(
      Dot.empty(id),
      Set[Entry[T]](),
      Set[Dot]()
    )

  }





  def decompose[T](orset: ORSet[T] ) = {
    val entries1 = orset.entries.groupBy(_.element).map {
      case (e, v) =>
        val dots = v.map(x => x.dots)
        val d1 = dots.foldLeft( Set[Dot]() ) {case (acc, ds) => acc ++ ds }
        Entry(e, d1)
    }.toSet 

    orset.copy(entries=entries1)
  }

  
  def add[T](orset: ORSet[T], e: T) = {

    val local = Dot.increment(orset.local)

    val entries = orset.entries.find(x => x.element == e) match {
      case Some(x) =>

        val d1 = (x.dots + local)
        val entry = x.copy(dots=d1)
        (orset.entries - x) + entry

      case None =>
        orset.entries + Entry(e, Set(local) )
    }



    val dots = orset.dots + local

    orset.copy(local=local, entries=entries, dots=dots)
  }


 


/*
  def add[T](orset: ORSet[T], e: T) = {

    val local = Dot.increment(orset.local)
    val entries = orset.entries + Entry(e, local)
    val dots = orset.dots + local

    ORSet(local, entries, dots)
  }

 */



  def delete[T](orset: ORSet[T], e: T) = {
    val local = Dot.increment(orset.local)
    val entries = orset.entries.filter (x => x.element != e)
    orset.copy(local=local, entries=entries) 
  }


  def query[T](orset: ORSet[T]): Set[T] = {
    orset.entries.map(x => x.element) 
  }

/*
  def toAdd[T](l: ORSet[T], r: ORSet[T]) = {

    l.entries.filter {x =>
      val has_e = r.entries.contains(x) != true
      val has_d = r.dots.contains(x.dot) != true
      has_e && has_d 
    }

  }

 */


   def toAdd[T](l: ORSet[T], r: ORSet[T]) = {

    l.entries.filter {x =>
      val has_e = r.entries.contains(x) != true
      val has_d = (x.dots subsetOf r.dots) != true
      has_e && has_d 
    }

  }


/*
  def toRemove[T](l: ORSet[T], r: ORSet[T]) = {

    l.entries.filter { x =>
      val is_dot = r.dots.contains(x.dot)
      val is_e = r.entries.contains(x) != true
      is_dot && is_e 
    }

  }


 */

  def toRemove[T](l: ORSet[T], r: ORSet[T]) = {

    l.entries.filter { x =>
      val is_dot = x.dots subsetOf r.dots
      val is_e = r.entries.contains(x) != true
      is_dot && is_e 
    }
  }

 
  def merge[T](l: ORSet[T], r: ORSet[T]) = {
    val e1 = toAdd(r, l) 
    val e2 = l.entries -- toRemove(l, r)

    val entries = e1 ++ e2
    val dots = l.dots ++  r.dots


    val o = l.copy(entries=entries, dots=dots)
    decompose(o)
  }


}
