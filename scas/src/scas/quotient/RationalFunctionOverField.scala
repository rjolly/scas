package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.{Element => Quotient_Element}
import scas.polynomial.tree.MultivariatePolynomialOverField
import scas.polynomial.ufd.PolynomialOverField
import scas.polynomial.TreePolynomial.Element
import scas.util.{Conversion, unary_~}
import scas.variable.Variable

class RationalFunctionOverField[C](using val ring: PolynomialOverField[Element[C, Array[Int]], C, Array[Int]]) extends QuotientOverField[Element[C, Array[Int]], C, Array[Int]] {
  def this(ring: Field[C])(s: Variable*) = this(using new MultivariatePolynomialOverField(using ring)(s*))
}

object RationalFunctionOverField {
  class Conv[C, S : Conversion[Variable]](ring: Field[C])(s: S*) extends RationalFunctionOverField(ring)(s.map(~_)*) with Field.Conv[Quotient_Element[Element[C, Array[Int]]]] {
    given instance: Conv[C, S] = this
  }
}
