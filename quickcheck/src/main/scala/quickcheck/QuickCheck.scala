package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(const(empty), for {
    value <- arbitrary[Int]
    heap <- genHeap
  } yield insert(value, heap))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin") = forAll { (a: Int, b: Int) =>
    val min = a min b
    findMin(insert(a, insert(b, empty))) == min
  }

  property("deleteMin1") = forAll { a : Int =>
    deleteMin(insert(a, empty)) == empty
  }

  property("deleteMinMultiple") = forAll { l: List[Int] =>
    val list = l.distinct
    list.size <= 2 || {
      val heap = list.foldLeft(empty)((h, i) => insert(i, h))
      findMin(deleteMin(heap)) != list.min
    }
  }

  property("deleteMinIsSorted") = forAll { h: H =>
    val list = toList(h)
    list == list.sorted
  }

  property("findMinAfterMeld") = forAll { (h1: H, h2: H) =>
    val min = List(h1, h2).filterNot(isEmpty).map(findMin)
    if (min.isEmpty) true
    else min contains findMin(meld(h1, h2))
  }

  property("size") = forAll { h: H =>
    size(h) == toList(h).size
  }

  property("meldSize") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    size(h1) + size(h2) == size(m)
  }

  property("allInsertsPresent") = forAll { (a: Int, b: Int) =>
    val l = toList(insert(b, insert(a, empty)))
    l.contains(a) && l.contains(b)
  }

  def size(heap: H): Int = {
    def go(h: H, s: Int): Int =
    {
      if (isEmpty(h)) s
      else go(deleteMin(h), s + 1)
    }
    go(heap, 0)
  }

  def toList(heap: H): List[A] = {
    def go(h: H, xs: List[A]): List[A] = {
      if (isEmpty(h))
        xs.reverse
      else
        go(deleteMin(h), findMin(h) :: xs)
    }
    go(heap, List())
  }

}
