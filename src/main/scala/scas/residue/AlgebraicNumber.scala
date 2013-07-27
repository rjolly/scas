package scas.residue

import scas.structure.Field
import scas.polynomial.{PolynomialOverUFD, UnivariatePolynomial}
import Residue.Element

trait AlgebraicNumber[R <: PolynomialOverUFD.Element[R, C, N], C, N] extends Residue[R, C, N] with Field[Element[R, C, N]] { self =>
  val ring: UnivariatePolynomial[R, C, N]
  def update(x: Element[R, C, N]): Unit = {
    val self(mod) = x
    // assert mod is irreducible
    list = List(mod)
  }
  def inverse(x: Element[R, C, N]) = {
    val self(a) = x
    fromRing(ring.modInverse(a, list(0)))
  }
  def coefficient(x: Element[R, C, N], m: Array[N]) = {
    val self(a) = x
    ring.coefficient(a, m)
  }
}
