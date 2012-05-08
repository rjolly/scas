package scas.polynomial

import scas.long2bigInteger
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait MultivariatePolynomial[T[C, N] <: Element[T[C, N], C, N], C, N] extends PolynomialOverUFD[T[C, N], C, N] {
  def split: MultivariatePolynomial[T, T[C, N], N]
  def location = length - 1
  override def gcd(x: T[C, N], y: T[C, N]) = if (length > 1) {
    val s = split
    convertFrom(s, s.gcd(convertTo(s, x), convertTo(s, y)))
  } else {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    multiply(primitivePart(gcd1(p, q)), ring.gcd(a, b))
  }
  def gcd1(x: T[C, N], y: T[C, N]): T[C, N]
  def convertTo(s: MultivariatePolynomial[T, T[C, N], N], w: T[C, N]): T[T[C, N], N] = (s.zero /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val x = pp.projection(a, location)
    l + s.multiply(s.pow(s.generators(0), pp.degree(x)), s.ring.convert(multiply(one, a / x, b)))
  }
  def convertFrom(s: MultivariatePolynomial[T, T[C, N], N], w: T[T[C, N], N]): T[C, N] = (zero /: s.iterator(w)) { (l, r) =>
    val (a, b) = r
    l + convert(b) * pow(generators(location), s.pp.degree(a))
  }
}
