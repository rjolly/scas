package scas.residue.conversion

import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.structure.commutative.Field
import scas.variable.Variable
import scas.util.Conversion

class AlgebraicNumber[C, S : Conversion[Variable]](using Field[C])(s: S*) extends scas.residue.Residue[Element[C, Array[Int]], C, Array[Int]] with scas.structure.commutative.conversion.Field[Element[C, Array[Int]]] {
  given ring: UnivariatePolynomial[C, S] = new UnivariatePolynomial(s*)
  given instance: AlgebraicNumber[C, S] = this
}
