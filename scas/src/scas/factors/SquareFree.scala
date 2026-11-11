package scas.factors

import scala.annotation.tailrec
import scala.compiletime.deferred
import scas.polynomial.ufd.PolynomialOverUFD
import scas.base.BigInteger.given
import Factors.Element
import Squarefree.Impl

class Squarefree[T, C, M](using PolynomialOverUFD[T, C, M]) extends Impl[T, C, M] with Factors.Conv[T, Int] {
  given instance: Squarefree[T, C, M] = this
}

object Squarefree {
  trait Impl[T, C, M] extends Factors[T, Int] {
    given ring: PolynomialOverUFD[T, C, M] = deferred
    def empty = Map.empty[T, Int]
    override def apply(x: T) = if x.isZero then zero else {
      val (c, p) = ring.contentAndPrimitivePart(x)
      if ring(c).isOne then this(p, false) else this(p) * fromRing(ring(c))
    }
    def apply(x: T, dummy: Boolean) = this(x, ring.squarefree(x), one, 0)
    @tailrec final def apply(x: T, y: T, map: Element[T, Int], n: Int): Element[T, Int] = {
      if x.isOne then map * fromRing(y) \ n
      else if y | x then this(x / y, y, map, n + 1)
      else {
        val c = ring.primitivePart(ring.gcd(x, y))
        this(x, c, map * fromRing(y / c) \ n, n)
      }
    }
  }
}
