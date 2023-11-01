package scas.residue.conversion

import scas.polynomial.TreePolynomial.Element

trait AlgebraicNumber[C, M] extends scas.residue.AlgebraicNumber[C, M] with Residue[Element[C, M], C, M] {
  given instance: AlgebraicNumber[C, M]
}
