package scas.polynomial.ufd

import scala.annotation.tailrec
import scas.polynomial.Polynomial
import scas.structure.commutative.UniqueFactorizationDomain
import scas.base.Boolean.given

trait PolynomialOverUFD[T, C, M] extends Polynomial[T, C, M] with UniqueFactorizationDomain[T] {
  given ring: UniqueFactorizationDomain[C]
  override def normalize(x: T) = primitivePart(x)
  extension (x: T) {
    def divideAndRemainder(y: T) = {
      if (y.isZero) throw new ArithmeticException("Polynomial divide by zero")
      else if (x.isZero) (zero, zero)
      else {
        val (s, a) = x.head
        if (y.factorOf(s, a, true)) {
          val (t, b) = y.head
          val c = this(s / t, a / b)
          val (q, r) = (x - c * y).divideAndRemainder(y)
          (c + q, r)
        } else (zero, x)
      }
    }
    def remainder(ys: T*): T = x.reduce(true, false, ys*)
    override def factorOf(s: M, a: C, strict: Boolean) = {
      val (t, b) = x.head
      (t | s) && (strict >> (b | a))
    }
    override def reduce(strict: Boolean, tail: Boolean, ys: T*) = super.reduce(x)(strict, tail, ys*)
    override def reduce(m: M, a: C, y: T, b: C, strict: Boolean) = {
      if (strict) x.subtract(m, a / b, y) else {
        val c = ring.gcd(a, b)
        val gcd = if (b.signum == -c.signum) -c else c
        val (a0, b0) = (a / gcd, b / gcd)
        (x%* b0).subtract(m, a0, y)
      }
    }
    def divideRight(c: C) = x.map((s, a) => (s, a / c))
    def %/ (c: C) = x.divideRight(c)
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

  extension (ring: UniqueFactorizationDomain[C]) def apply(s: T*) = {
    same(s*)
    this
  }
}
