package scas.quotient.conversion

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.TreePolynomial
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class RationalFunctionOverField[C, S : Conversion[Variable]](ring: Field[C])(s: S) extends scas.quotient.RationalFunctionOverField(ring)(~s) with scas.structure.commutative.conversion.Field[Element[TreePolynomial.Element[C, Array[Int]]]] {
  given instance: RationalFunctionOverField[C, S] = this
}
