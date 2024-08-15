package scas.residue.conversion

import scas.polynomial.TreePolynomial.Element
import scas.structure.commutative.Field
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class AlgebraicNumber[C, S : Conversion[Variable]](ring: Field[C])(s: S*) extends scas.residue.AlgebraicNumber(ring)(s.map(~_)*) with scas.structure.commutative.conversion.Field[Element[C, Array[Int]]] {
  given instance: AlgebraicNumber[C, S] = this
}
