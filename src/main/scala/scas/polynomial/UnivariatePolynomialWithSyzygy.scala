package scas.polynomial

import UnivariatePolynomialWithSyzygy.Element

trait UnivariatePolynomialWithSyzygy[S[C, N] <: Polynomial.Element[S[C, N], C, N], T <: Element[S, T, C, N], C, @specialized(Int, Long) N] extends UnivariatePolynomial[T, C, N] with PolynomialWithSyzygy[S, T, C, N] {
  def modInverse(x: T, mod: T) = {
    val w = gcd(apply(x, 0), mod)
    assert (w.isOne)
    fromPolynomial(w.element(0))
  }
}

object UnivariatePolynomialWithSyzygy {
  trait Element[S[C, N] <: Polynomial.Element[S[C, N], C, N], T <: Element[S, T, C, N], C, @specialized(Int, Long) N] extends PolynomialOverUFD.Element[T, C, N] with PolynomialWithSyzygy.Element[S, T, C, N] { this: T =>
    val factory: UnivariatePolynomialWithSyzygy[S, T, C, N]
  }
}
