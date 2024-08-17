package scas.residue

import scas.structure.BooleanRing
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.tree.BooleanPolynomial
import scas.polynomial.PolynomialOverUFD
import scas.variable.Variable
import scas.util.Conversion
import scas.base.BigInteger
import BigInteger.given

class BooleanAlgebra(using val ring: PolynomialOverUFD[Element[Boolean, Array[Int]], Boolean, Array[Int]]) extends Residue[Element[Boolean, Array[Int]], Boolean, Array[Int]] with BooleanRing[Element[Boolean, Array[Int]]] {
  def this(s: Variable*) = this(using new BooleanPolynomial(s*))
  for (x <- generators) {
    update(x+x\2)
  }
}

object BooleanAlgebra {
  def apply[S : Conversion[Variable]](s: S*) = new conversion.BooleanAlgebra(s*)
}
