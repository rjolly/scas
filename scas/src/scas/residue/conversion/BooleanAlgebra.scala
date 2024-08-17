package scas.residue.conversion

import scas.polynomial.TreePolynomial.Element
import scas.structure.commutative.conversion.UniqueFactorizationDomain
import scas.structure.conversion.BooleanRing
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class BooleanAlgebra[S : Conversion[Variable]](s: S*) extends scas.residue.BooleanAlgebra(s.map(~_)*) with UniqueFactorizationDomain[Element[Boolean, Array[Int]]] with BooleanRing[Element[Boolean, Array[Int]]] {
  given instance: BooleanAlgebra[S] = this
}
