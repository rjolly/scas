package scas.polynomial.residue

import scas.structure.Field
import scas.polynomial.{PolynomialOverUFD, UnivariatePolynomial}
import Residue.Element

trait AlgebraicNumberLike[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N] extends Residue[R, C, N] with Field[Element[R, C, N]] {
  override val ring: UnivariatePolynomial[R, C, N]
  def update(x: Element[R, C, N]): Unit = {
    val self(mod) = x
    // assert mod is irreducible
    list = List(mod)
  }
  def inverse(x: Element[R, C, N]) = {
    val self(a) = x
    apply(ring.modInverse(a, list(0)))
  }
}
