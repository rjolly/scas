package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.{Element as Quotient_Element}
import scas.polynomial.tree.MultivariatePolynomial
import scas.polynomial.ufd.PolynomialOverField
import scas.polynomial.TreePolynomial.Element
import scas.util.Conversion
import scas.variable.Variable

class RationalFunctionOverField[C, N](using PolynomialOverField[Element[C, Array[N]], C, Array[N]]) extends QuotientOverField[Element[C, Array[N]], C, Array[N]]

object RationalFunctionOverField {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new Conv(MultivariatePolynomial(ring)(s*))

  class Conv[C, N](ring: PolynomialOverField[Element[C, Array[N]], C, Array[N]]) extends RationalFunctionOverField(using ring) with Field.Conv[Quotient_Element[Element[C, Array[N]]]] {
    given instance: Conv[C, N] = this
  }
}
