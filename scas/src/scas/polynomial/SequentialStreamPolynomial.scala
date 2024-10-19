package scas.polynomial

import scas.util.Stream
import StreamPolynomial.Element

trait SequentialStreamPolynomial[C, M] extends StreamPolynomial[C, M] {
  override def apply(s: (M, C)*) = Stream.sequential(s*)

  extension (x: Element[C, M]) {
    override def add(y: Element[C, M]) = if (!x.isEmpty) {
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
  }
}
