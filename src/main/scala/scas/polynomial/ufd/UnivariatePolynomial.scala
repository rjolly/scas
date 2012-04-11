package scas.polynomial.ufd

import scas.structure.EuclidianDomain
import scas.polynomial.PolynomialWithSyzygy
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import UnivariatePolynomial.Element

trait UnivariatePolynomial[T <: Element[T, C, N], C, N] extends PolynomialOverField[T, C, N] with PolynomialWithSyzygy[T, C, N] with EuclidianDomain[T] {
  assert (length == 1)
  def generator: T = generator(0)
  def ps(dimension: Int): UnivariatePolynomial[T, C, N]
  def norm(x: T) = java.math.BigInteger.valueOf(degree(x))
  def derivative(w: T) = map(w, (a, b) => (a / pp.generator(0), b * ring(pp.degree(a))))
  def modInverse(x: T, mod: T) = {
    val s = ps(1)
    val w = s.monic(s.gcd(s(x, 0), s(mod)))
    assert(apply(w).isOne)
    w.syzygy.value(0)
  }
}

object UnivariatePolynomial {
  trait Element[T <: Element[T, C, N], C, N] extends PolynomialOverUFD.Element[T, C, N] with PolynomialWithSyzygy.Element[T, C, N] { this: T =>
    val factory: UnivariatePolynomial[T, C, N]
  }
}
