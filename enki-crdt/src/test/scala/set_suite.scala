package Enki_DT
import org.scalatest._

import Enki.PipeOps._
import scala.language.higherKinds

trait SetHelper[A[_]] {
  def empty(id: String): A[String]
  def sortedEntries(a: A[String]): Set[String]
}



class SetSuite[A[_]](SH: SetHelper[A], S: SetCRDT[A], name: String) extends FunSuite {
  override def suiteName: String = name 

  val osA: A[String]  = SH.empty("A")
  val osB: A[String] = SH.empty("B")


  val osA2 =
    S.add(osA, "nigga") |> {x => S.add(x, "Faggot") }

  val osB1 = S.add(osB,"Bitch")






  test("Merges seperate writes") {
    val merged = S.merge(osA2, osB1)

    val me = S.query(merged)
    assert( me.exists(x => x == "Bitch") )

    val (aEntries, mEntries) = (S.query(osA2), me)
    assert(aEntries subsetOf mEntries)
  }



  test("Is it Commutative") {
    val mergedA = S.merge(osA2, osB1)
    val mergedB = S.merge(osB1, osA2)

    assert(SH.sortedEntries(mergedA) == SH.sortedEntries(mergedB) )
  }


  test("Observes deletes from other replicas") {
    val osA3 = S.add(osA2, "Bish")

    val mergedA = S.merge(osA3, osB1)
    val mergedB = S.merge(osB1, osA3)

    val mergedB1 = S.delete(mergedB, "Bish")

    val mergedA1 = S.merge(mergedA, mergedB1)
    val p = S.query(mergedA1).contains("Bish") != true
    assert(p)
  }
  
}




object TPSetSuite {
  def apply() = {
    val SH = new SetHelper[TPSet] {
      def empty(id: String) = TPSet.empty[String]
      def sortedEntries(a: TPSet[String]) = TPSet.query(a).toList.sortWith(_ <= _).toSet
    }

    val S = TPSet
    new SetSuite(SH, S, "TPSet Suite")
  }
}


object ORSetSuite  {
  def apply() = {
    val SH = new SetHelper[ORSet] {
      def empty(id: String) = ORSet.empty[String](id)

      def sortedEntries(a: ORSet[String]) = {
        a.entries.toList.sortWith{(l, r) => l.element <= r.element}
          .map(x => x.element)
          .toSet
      }

    }

    val S = ORSet

    new SetSuite(SH, S, "ORSet Suite") 

  }
}





object LWWSetSuite  {
  def apply() = {
    val SH = new SetHelper[LWWSet] {
      def empty(id: String) = LWWSet.empty[String]
      def sortedEntries(a: LWWSet[String]) = LWWSet.query(a).toList.sortWith(_ <= _).toSet
    }

    val S = LWWSet

    new SetSuite(SH, S, "LWWSet Suite") 
  }
}


class CRDTSetTests extends Suites(LWWSetSuite(), TPSetSuite(), ORSetSuite() )
