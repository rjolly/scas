package scas.polynomial

import scas.long2bigInteger
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait MultivariatePolynomial[T[C, N] <: Element[T[C, N], C, N], C, @specialized(Int, Long) N] extends PolynomialOverUFD[T[C, N], C, N] {
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
  def convertTo(s: MultivariatePolynomial[T, T[C, N], N], w: T[C, N]): T[T[C, N], N] = (s.zero /: iterator(w)) { case (l, (m, c)) =>
    val x = pp.projection(m, location)
    l + s.multiply(s.pow(s.generator(0), pp.degree(x)), s.ring.convert(multiply(one, m / x, c)))
  }
  def convertFrom(s: MultivariatePolynomial[T, T[C, N], N], w: T[T[C, N], N]): T[C, N] = (zero /: s.iterator(w)) { case (l, (m, c)) =>
    l + convert(c) * pow(generator(location), s.pp.degree(m))
  }
}
