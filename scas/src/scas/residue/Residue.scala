package scas.residue

import scas.polynomial.impl.PolynomialOverField

class Residue[T, C, M](using ring: PolynomialOverField[T, C, M])(mod: T) extends AlgebraicNumber[T, C, M] {
  given instance: Residue[T, C, M] = this
  update(mod)
}
