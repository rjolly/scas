package scas.polynomial

import scas.util.{LazyList, Stream, Future, await, given}
import StreamPolynomial.Element
import LazyList.{seq, #::}
import Stream.{par, #:}

trait StreamPolynomial[C, M] extends LazyListPolynomial[C, M] {
  extension (x: LazyListPolynomial.Element[C, M]) override def add(y: LazyListPolynomial.Element[C, M]) = x.par.add(y).seq

  extension (x: Element[C, M]) {
    def add(y: LazyListPolynomial.Element[C, M]): Element[C, M] = if (!x.isEmpty) {
      if (!y.isEmpty) {
        val (s, a) = x.head
        val (t, b) = y.head
        if (s > t) (s, a)#:x.tail.map(_.add(y))
        else if (s < t) (t, b)#:Future(x.add(y.tail))
        else {
          val c = a + b
          val result = (s, c)#:x.tail.map(_.add(y.tail))
          if (!c.isZero) result else result.tail.await
        }
      } else x
    } else y.par
  }
}

object StreamPolynomial {
  type Element[C, M] = Stream[(M, C)]
}
