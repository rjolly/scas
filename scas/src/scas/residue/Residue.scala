package scas.residue

import scas.polynomial.impl.UnivariatePolynomial

class Residue[T, C, M](using val ring: UnivariatePolynomial[T, C, M])(mod: T) extends AlgebraicNumber[T, C, M] {
  given instance: Residue[T, C, M] = this
  update(mod)
}
