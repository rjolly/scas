package scas.residue.conversion

import scas.polynomial.TreePolynomial.Element
import scas.structure.commutative.Field
import scas.variable.Variable
import scas.util.Conversion

class AlgebraicNumber[C, S : Conversion[Variable]](using Field[C])(s: S*) extends scas.residue.AlgebraicNumber[C, S](s*) with scas.structure.commutative.conversion.Field[Element[C, Array[Int]]] {
  given instance: AlgebraicNumber[C, S] = this
}
