package scas.polynomial.ufd

import scala.annotation.tailrec
import scala.compiletime.deferred
import scas.factors.Squarefree
import scas.polynomial.Polynomial
import scas.structure.commutative.UniqueFactorizationDomain
import scas.base.BigInteger.given
import scas.base.Boolean.given

trait PolynomialOverUFD[T, C, M] extends Polynomial[T, C, M] with UniqueFactorizationDomain[T] {
  given ring: UniqueFactorizationDomain[C] = deferred
  given factors: Squarefree.Impl[T, C, M] = new Squarefree(using this)
  override def normalize(x: T) = primitivePart(x)
  extension (x: T) {
    def divideAndRemainder(y: T) = {
      if y.isZero then throw new ArithmeticException("Polynomial divide by zero")
      else if x.isZero then (zero, x)
      else {
        val (s, _) = x.head
        x.divideAndRemainder(s, zero, y)
      }
    }
    @tailrec final def divideAndRemainder(m: M, q: T, y: T): (T, T) = {
      val xs = x.iterator(m)
      if xs.hasNext then {
        val (s, a) = xs.next()
        if y.factorOf(s, a, true) then {
          val (t, b) = y.head
          val c = this(s / t, a / b)
          (x - c * y).divideAndRemainder(s, c + q, y)
        } else if xs.hasNext then {
          val (s, _) = xs.next()
          x.divideAndRemainder(s, q, y)
        } else (q, x)
      } else (q, x)
    }
    override def factorOf(s: M, a: C, strict: Boolean) = {
      val (t, b) = x.head
      (t | s) && (strict >> (b | a))
    }
    override def reduce(strict: Boolean, tail: Boolean, ys: T*) = super.reduce(x)(strict, tail, ys*)
    override def reduce(m: M, a: C, y: T, b: C, strict: Boolean) = {
      if strict then x.subtract(m, a / b, y) else {
        val c = ring.gcd(a, b)
        val gcd = if b.signum == -c.signum then -c else c
        val (a0, b0) = (a / gcd, b / gcd)
        (x%* b0).subtract(m, a0, y)
      }
    }
    def divideRight(c: C) = x.map((s, a) => (s, a / c))
    def %/ (c: C) = x.divideRight(c)

    def isSquare = if x.isZero then true else {
      val (c, p) = contentAndPrimitivePart(x)
      if c.isOne then factors(p, false).isSquare else c.isSquare && p.isSquare
    }
    def coefficients(y: T): List[T] = coefficients(y, Nil)
    @tailrec final def coefficients(y: T, a: List[T]): List[T] = if x.isZero then a else {
      val (q, r) = x.divideAndRemainder(y)
      q.coefficients(y, r :: a)
    }
    def derivative(y: T) = x.coefficients(y).zipWithIndex.foldLeft(zero) { (l, r) =>
      val (p, n) = r
      l + (if n == 0 then zero else fromInt(n) * p * y \ (n - 1))
    }
    def substitute(y: T, z: T) = x.coefficients(y).zipWithIndex.foldLeft(zero) { (l, r) =>
      val (p, n) = r
      l + p * z \ n
    }
  }
  def factorize(x: T) = factors(x)
  def squarefree(x: T) = generators.foldLeft(x) { (l, r) =>
    val a = l.derivative(r)
    if a.isZero then l else l / primitivePart(gcd(l, a))
  }
  def sqrt(x: T) = if x.isZero then zero else {
    val (c, p) = contentAndPrimitivePart(x)
    if c.isOne then factors(p, false).sqrt.expand else sqrt(p) * ring.sqrt(c)
  }
  def content(x: T) = {
    val c = x.iterator.foldLeft(ring.zero) { (l, r) =>
      val (_, a) = r
      ring.gcd(l, a)
    }
    val d = ring.abs(c)
    if x.signum < 0 then -d else d
  }
  def contentAndPrimitivePart(x: T) = {
    if x.isZero then (ring.zero, zero) else {
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
