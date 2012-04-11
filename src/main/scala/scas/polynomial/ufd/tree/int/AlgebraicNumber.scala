package scas.polynomial.ufd.tree.int

import scas.structure.Field

class AlgebraicNumber[C](ring: Field[C], name: String) extends scas.polynomial.ufd.AlgebraicNumber(UnivariatePolynomial(ring, name))

object AlgebraicNumber {
  def apply[C](ring: Field[C], name: String) = new AlgebraicNumber(ring, name)
}
