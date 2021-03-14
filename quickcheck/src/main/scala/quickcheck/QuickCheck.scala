package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(const(empty), for {
    x <- arbitrary[Int]
    y <- genHeap
  } yield insert(x, y))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") = forAll { (a: Int, b: Int) =>
    val h1 = insert(b, insert(a, empty))
    if (a < b) {
      findMin(h1) == a
    } else {
      findMin(h1) == b
    }
  }

  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") = forAll { a: Int =>
    val h = insert(a, empty)
    val newH = deleteMin(h)
    newH == empty
  }

  property("Given any heap, you should get a sorted sequence of elements " +
    "when continually finding and deleting minima. " +
    "(Hint: recursion and helper functions are your friends.)") = {
    @tailrec
    def getSortedList(xs: List[A], he: H): List[A] = {
      if (isEmpty(he)) {
        xs
      } else {
        val a = findMin(he)
        getSortedList(a :: xs, deleteMin(he))
      }
    }

      forAll { (h: H) => {
        val list = getSortedList(Nil, h)
        list.reverse == list.sorted
      }
    }
  }

  property("Given any heap, you should get a sorted sequence of elements " +
    "when continually finding and deleting minima from individual lists as well as their melding heap. " +
    "(Hint: recursion and helper functions are your friends.)") = {
    @tailrec
    def getSortedList(xs: List[A], he: H): List[A] = {
      if (isEmpty(he)) {
        xs
      } else {
        val a = findMin(he)
        getSortedList(a :: xs, deleteMin(he))
      }
    }

    forAll { (h1: H, h2: H) => {
        val l1 = getSortedList(Nil, h1) // doesn't matter if sorted
        val l2 = getSortedList(Nil, h2) // doesn't matter if sorted
        val l3 = getSortedList(Nil, meld(h1,h2))
        (l1 ::: l2).sorted.reverse == l3
      }
    }
  }

  property("Finding a minimum of the melding of any two heaps" +
    " should return a minimum of one or the other.") =  forAll { (h1: H, h2: H) =>
    if(!isEmpty(h1) && !isEmpty(h2)) {
      List(findMin(h1), findMin(h2)).contains(findMin(meld(h1, h2)))
    } else {
      true
    }
  }
}
