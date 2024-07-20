package scas.residue.conversion

import scas.structure.commutative.conversion.Field
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable
import scas.util.Conversion
import scas.base.ModInteger

class GaloisField[S : Conversion[Variable]](str: String)(s: S*) extends scas.residue.Residue[Element[Int, Array[Int]], Int, Array[Int]] with Field[Element[Int, Array[Int]]] {
  given ring: UnivariatePolynomial[Int, S] = new UnivariatePolynomial(using ModInteger(str))(s*)
  given instance: GaloisField[S] = this
}
