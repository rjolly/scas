package scas.polynomial

import scala.annotation.tailrec
import scas.structure.commutative.UniqueFactorizationDomain
import scas.base.Boolean
import Boolean.given

trait PolynomialOverUFD[T, C, M] extends Polynomial[T, C, M] with UniqueFactorizationDomain[T] {
  given ring: UniqueFactorizationDomain[C]
  extension (x: T) {
    def divideAndRemainder(y: T) = {
      if (y.isZero) throw new ArithmeticException("Polynomial divide by zero")
      else if (x.isZero) (zero, zero)
      else {
        val (s, a) = x.head
        if (y.reduce(s, a, true)) {
          val (t, b) = y.head
          val c = this(s / t, a / b)
          val (q, r) = (x - c * y).divideAndRemainder(y)
          (c + q, r)
        } else (zero, x)
      }
    }
    def remainder(ys: T*): T = x.reduce(true, false, ys*)
    override def reduce(s: M, a: C, remainder: Boolean) = {
      val (t, b) = x.head
      (t | s) && (remainder >> (b | a))
    }
    override def reduce(m: M, a: C, y: T, b: C) = {
      val c = ring.gcd(a, b)
      val gcd = if (b.signum == -c.signum) -c else c
      val (a0, b0) = (a / gcd, b / gcd)
      (x%* b0).subtract(m, a0, y)
    }
  }
  def content(x: T) = {
    val c = x.iterator.foldLeft(ring.zero) { (l, r) =>
      val (_, a) = r
      ring.gcd(l, a)
    }
    val d = ring.abs(c)
    if (x.signum < 0) -d else d
  }
  def contentAndPrimitivePart(x: T) = {
    if (x.isZero) (ring.zero, zero) else {
      val c = content(x)
      (c, x%/ c)
    }
  }
  def primitivePart(x: T) = { val (c, p) = contentAndPrimitivePart(x) ; p }
  extension (x: T) def %/ (c: C) = x.map((s, a) => (s, a / c))

  extension (ring: UniqueFactorizationDomain[C]) def apply(s: T*) = {
    same(s*)
    this
  }
}
