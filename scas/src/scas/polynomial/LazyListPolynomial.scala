package scas.polynomial

import scas.util.LazyList
import LazyListPolynomial.Element

trait LazyListPolynomial[C, M] extends Polynomial[Element[C, M], C, M] {
  def apply(s: (M, C)*) = LazyList(s*)

  extension (x: Element[C, M]) {
    def add(y: Element[C, M]) = if (!x.isEmpty) {
      if (!y.isEmpty) {
        val (s, a) = x.head
        val (t, b) = y.head
        if (s > t) (s, a)#::(x.tail + y)
        else if (s < t) (t, b)#::(x + y.tail)
        else {
          val c = a + b
          val result = (s, c)#::(x.tail + y.tail)
          if (!c.isZero) result else result.tail
        }
      } else x
    } else y

    override def isZero = x.isEmpty

    def iterator = x.iterator

    def size = x.toSeq.size

    def head = x.head

    def last = x.toSeq.last

    def map(f: (M, C) => (M, C)) = if (!x.isEmpty) {
      val (s, a) = x.head
      val (m, c) = f(s, a)
      val result = (m, c)#::x.tail.map(f)
      if (!c.isZero) result else result.tail
    } else LazyList.Nil
  }
}

object LazyListPolynomial {
  type Element[C, M] = LazyList[(M, C)]
}
