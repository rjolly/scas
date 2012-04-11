package scas.polynomial.ufd

import scas.polynomial.Polynomial
import scas.structure.UniqueFactorizationDomain
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialOverUFD[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] with UniqueFactorizationDomain[T] {
  override implicit val ring: UniqueFactorizationDomain[C]
  def divide(x: T, y: T) = {
    val (q, r) = divideAndRemainder(x, y)
    q
  }
  def remainder(x: T, y: T) = {
    val (q, r) = divideAndRemainder(x, y)
    r
  }
  def divideAndRemainder(x: T, y: T) = {
    if (y.isZero) throw new ArithmeticException("Polynomial divide by zero")
    else if (x.isZero) (zero, zero)
    else {
      val (s, a) = headTerm(x)
      val (t, b) = headTerm(y)
      if (!(t | s) || !(b | a)) (zero, x) else {
        val c = multiply(one, s / t, a / b)
        divideAndRemainder(x - c * y, y) match { case (q, r) => (c + q, r) }
      }
    }
  }
  def gcd(x: T, y: T) = {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    multiply(primitivePart(gcd1(p, q)), ring.gcd(a, b))
  }
  def gcd1(x: T, y: T): T
  def content(x: T) = (ring.zero /: iterator(x)) { (l, r) =>
    val (a, b) = r
    ring.gcd(l, b) match { case gcd => if (ring.signum(b) < 0) -gcd else gcd }
  }
  def contentAndPrimitivePart(x: T) = {
    val c = content(x)
    if (c.isZero) (ring.zero, zero) else (c, divide(x, c))
  }
  def primitivePart(x: T) = {
    val (c, p) = contentAndPrimitivePart(x)
    p
  }
  def divide(w: T, y: C) = map(w, (a, b) => (a, b / y))
}

object PolynomialOverUFD {
  trait Element[T <: Element[T, C, N], C, N] extends Polynomial.Element[T, C, N] with UniqueFactorizationDomain.Element[T] { this: T =>
    val factory: PolynomialOverUFD[T, C, N]
  }
}
