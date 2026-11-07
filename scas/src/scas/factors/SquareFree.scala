package scas.factors

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
    override def apply(x: T) = {
      val (c, p) = ring.contentAndPrimitivePart(x)
      if ring(c).isOne then this(p, false) else this(p) * fromRing(ring(c))
    }
    def apply(x: T, dummy: Boolean) = this(x, ring.squarefree(x), 0)
    def apply(x: T, y: T, n: Int): Element[T, Int] = {
      if x.isOne then fromRing(y) \ n
      else if y | x then this(x / y, y, n + 1)
      else {
        val c = ring.gcd(x, y)
        fromRing(y / c) \ n * this(x, c, n)
      }
    }
  }
}
