package scas.residue

import scas.structure.BooleanRing
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.tree.BooleanPolynomial
import scas.polynomial.PolynomialOverUFD
import scas.variable.Variable
import scas.util.Conversion

class BooleanAlgebra(using val ring: PolynomialOverUFD[Element[Boolean, Array[Int]], Boolean, Array[Int]]) extends Residue[Element[Boolean, Array[Int]], Boolean, Array[Int]] with BooleanRing[Element[Boolean, Array[Int]]] {
  def this(s: Variable*) = this(using new BooleanPolynomial(s*))
}

object BooleanAlgebra {
  def apply[S : Conversion[Variable]](s: S*) = new conversion.BooleanAlgebra(s*)
}
