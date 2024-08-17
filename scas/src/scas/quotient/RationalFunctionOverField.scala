package scas.quotient

import scas.structure.commutative.Field
import scas.polynomial.tree.MultivariatePolynomialOverField
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.PolynomialOverField
import scas.variable.Variable

class RationalFunctionOverField[C](using val ring: PolynomialOverField[Element[C, Array[Int]], C, Array[Int]]) extends QuotientOverField[Element[C, Array[Int]], C, Array[Int]] {
  def this(ring: Field[C])(s: Variable*) = this(using new MultivariatePolynomialOverField(using ring)(s*))
}
