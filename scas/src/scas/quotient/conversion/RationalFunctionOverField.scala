package scas.quotient.conversion

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.tree.MultivariatePolynomial
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class RationalFunctionOverField[C, S : Conversion[Variable]](ring: Field[C])(s: S*) extends scas.quotient.RationalFunctionOverField(ring)(s.map(~_)*) with scas.structure.commutative.conversion.Field[Element[MultivariatePolynomial.Element[C]]] {
  given instance: RationalFunctionOverField[C, S] = this
}
