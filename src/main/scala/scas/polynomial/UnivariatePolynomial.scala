package scas.polynomial

import scas.structure.EuclidianDomain
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import UnivariatePolynomial.Element

trait UnivariatePolynomial[T <: Element[T, C, N], C, N] extends PolynomialOverField[T, C, N] with PolynomialWithSyzygy[T, C, N] with EuclidianDomain[T] {
  assert (length == 1)
  val module = Syzygy("e", 2, tree.Polynomial(ring, pp))
  def norm(x: T) = java.math.BigInteger.valueOf(degree(x))
  def derivative(w: T) = map(w, (a, b) => (a / pp.generators(0), b * ring(pp.degree(a))))
  def modInverse(x: T, mod: T) = {
    val w = monic(gcd(apply(x, 0), mod))
    assert (w.isOne)
    fromSyzygy(w.syzygy.value(0))
  }
}

object UnivariatePolynomial {
  trait Element[T <: Element[T, C, N], C, N] extends PolynomialOverUFD.Element[T, C, N] with PolynomialWithSyzygy.Element[T, C, N] { this: T =>
    val factory: UnivariatePolynomial[T, C, N]
  }
}
