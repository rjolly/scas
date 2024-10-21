package scas.polynomial.direct

import scas.polynomial.Polynomial
import scas.util.direct.Stream
import StreamPolynomial.Element

trait StreamPolynomial[C, M] extends Polynomial[Element[C, M], C, M] {
  def apply(s: (M, C)*) = Stream(s*)

  extension (x: Element[C, M]) {
    def add(y: Element[C, M]) = if (!x.isEmpty) {
      if (!y.isEmpty) {
        val (s, a) = x.head
        val (t, b) = y.head
        if (s > t) (s, a)#::(x.tail + y)
        else if (s < t) (t, b)#::(x + y.tail)
        else {
          val c = a + b
          val result = (s, c)#:(x.tail + y.tail)
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
    } else Stream.Nil
  }
}

object StreamPolynomial {
  type Element[C, M] = Stream[(M, C)]
}
