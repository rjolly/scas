package scas.polynomial

import scas.structure.UniqueFactorizationDomain
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialOverUFD[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] with UniqueFactorizationDomain[T] {
  override implicit val ring: UniqueFactorizationDomain[C]
  def divideAndRemainder(x: T, y: T) = {
    if (y.isZero) throw new ArithmeticException("Polynomial divide by zero")
    else if (x.isZero) (zero, zero)
    else {
      val (s, a) = head(x)
      val (t, b) = head(y)
      if (!(t | s) || !(b | a)) (zero, x) else {
        val c = multiply(one, s / t, a / b)
        val (q, r) = divideAndRemainder(x - c * y, y)
        (c + q, r)
      }
    }
  }
  override def reduce(x: T, y: T): T = {
    if (x.isZero) zero
    else {
      val (s, a) = head(x)
      val (t, b) = head(y)
      if (!(t | s)) x else {
        val gcd = ring.gcd(a, b)
        val (a0, b0) = (a / gcd, b / gcd)
        reduce(multiply(x, b0) - multiply(y, s / t, a0), y)
      }
    }
  }
  def content(x: T) = {
    val c = (ring.zero /: iterator(x)) { (l, r) => val (a, b) = r ; ring.gcd(l, b) }
    ring.abs(c) * ring(signum(x))
  }
  def contentAndPrimitivePart(x: T) = {
    if (x.isZero) (ring.zero, zero)
    else {
      val c = content(x)
      (c, divide(x, c))
    }
  }
  def primitivePart(x: T) = { val (c, p) = contentAndPrimitivePart(x) ; p }
  def divide(w: T, y: C) = map(w, (a, b) => (a, b / y))
}

object PolynomialOverUFD {
  trait Element[T <: Element[T, C, N], C, N] extends Polynomial.Element[T, C, N] with UniqueFactorizationDomain.Element[T] { this: T =>
    val factory: PolynomialOverUFD[T, C, N]
  }
}
