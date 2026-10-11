package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.{Element as Quotient_Element}
import scas.polynomial.tree.MultivariatePolynomialOverField
import scas.polynomial.ufd.PolynomialOverField
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable
import RationalFunctionOverField.Impl

class RationalFunctionOverField[C](using PolynomialOverField[Element[C, Array[Int]], C, Array[Int]]) extends Impl[C, Array[Int]] with Field.Conv[Quotient_Element[Element[C, Array[Int]]]] {
  def this(ring: Field[C])(variables: Variable*) = this(using new MultivariatePolynomialOverField(ring)(variables*))
    given instance: RationalFunctionOverField[C] = this
}

object RationalFunctionOverField {
  trait Impl[C, M](using PolynomialOverField[Element[C, M], C, M]) extends QuotientOverField[Element[C, M], C, M]
}
