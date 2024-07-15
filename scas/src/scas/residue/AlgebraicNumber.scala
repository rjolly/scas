package scas.residue

import scas.polynomial.tree.UnivariatePolynomial
import scas.structure.commutative.Field
import scas.variable.Variable
import scas.util.Conversion

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new conversion.AlgebraicNumber(using UnivariatePolynomial(using ring)(s*))
}
