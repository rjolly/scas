package scas.residue.conversion

import scas.polynomial.TreePolynomial.Element
import scas.structure.commutative.conversion.Field
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class GaloisField[S : Conversion[Variable]](str: String)(s: S*) extends scas.residue.GaloisField(str)(s.map(~_)*) with Field[Element[Int, Array[Int]]] {
  given instance: GaloisField[S] = this
}
