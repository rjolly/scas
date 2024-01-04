package scas.residue

import scas.polynomial.tree.UnivariatePolynomial
import scas.structure.commutative.Field
import scas.power.Lexicographic
import scas.variable.Variable

object AlgebraicNumber {
  def apply[C](ring: Field[C])(s: Variable*) = new conversion.AlgebraicNumber(using new UnivariatePolynomial(using ring, Lexicographic[Int](s*)))
}
