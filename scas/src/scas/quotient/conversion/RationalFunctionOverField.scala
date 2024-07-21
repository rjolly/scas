package scas.quotient.conversion

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial
import scas.variable.Variable
import scas.util.Conversion

class RationalFunctionOverField[C, S : Conversion[Variable]](using Field[C])(s: S) extends scas.quotient.RationalFunctionOverField[TreePolynomial.Element[C, Array[Int]], C, Array[Int]] with scas.structure.commutative.conversion.Field[Element[TreePolynomial.Element[C, Array[Int]]]] {
  given ring: UnivariatePolynomial[C, S] = new UnivariatePolynomial(s)
  given instance: RationalFunctionOverField[C, S] = this
}
