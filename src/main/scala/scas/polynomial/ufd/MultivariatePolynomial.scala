package scas.polynomial.ufd

import scas.long2bigInteger
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait MultivariatePolynomial[T[C, N] <: Element[T[C, N], C, N], C, N] extends PolynomialOverUFD[T[C, N], C, N] {
  def split: MultivariatePolynomial[T, T[C, N], N]
  def location = length - 1
  override def gcd(x: T[C, N], y: T[C, N]) = if (length > 1) {
    val s = split
    convertFrom(s, s.gcd(convertTo(s, x), convertTo(s, y)))
  } else super.gcd(x, y)
  def convertTo(s: MultivariatePolynomial[T, T[C, N], N], w: T[C, N]): T[T[C, N], N] = (s.zero /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val x = pp.projection(a, location)
    l + s.multiply(s.pow(s.generator(0), pp.degree(x)), s.ring(multiply(one, a / x, b)))
  }
  def convertFrom(s: MultivariatePolynomial[T, T[C, N], N], w: T[T[C, N], N]): T[C, N] = (zero /: s.iterator(w)) { (l, r) =>
    val (a, b) = r
    l + apply(b) * pow(generator(location), s.pp.degree(a))
  }
}
