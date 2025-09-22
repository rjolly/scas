package scas.polynomial

import scas.util.{Stream, await, given}
import StreamPolynomial.Element

trait StreamPolynomial[C, M] extends Polynomial[Element[C, M], C, M] {
  def apply(s: (M, C)*) = Stream(s*)

  extension (x: Element[C, M]) {
    def add(y: Element[C, M]) = if !x.isEmpty then {
      if !y.isEmpty then {
        val (s, a) = x.head
        val (t, b) = y.head
        if s > t then (s, a)#:x.tail.map(_ + y)
        else if s < t then (t, b)#:y.tail.map(x + _)
        else {
          val c = a + b
          val result = (s, c)#:(for sx <- x.tail; sy <- y.tail yield sx + sy)
          if !c.isZero then result else result.tail.await
        }
      } else x
    } else y

    override def isZero = x.isEmpty

    def iterator = x.iterator

    def size = x.toSeq.size

    def head = x.head

    def last = x.toSeq.last

    def map(f: (M, C) => (M, C)) = if !x.isEmpty then {
      val (s, a) = x.head
      val (m, c) = f(s, a)
      val result = (m, c)#:x.tail.map(_.map(f))
      if !c.isZero then result else result.tail.await
    } else Stream.Nil
  }
}

object StreamPolynomial {
  type Element[C, M] = Stream[(M, C)]
}
