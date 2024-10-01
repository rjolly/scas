package scas.residue

import scas.structure.commutative.Field
import scas.polynomial.TreePolynomial.Element
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.ModInteger

class GaloisField(str: String)(s: Variable*) extends AlgebraicNumber(ModInteger(str))(s*)

object GaloisField {
  def apply[S : Conversion[Variable]](str: String)(s: S*) = new Conv(str)(s*)

  class Conv[S : Conversion[Variable]](str: String)(s: S*) extends GaloisField(str)(s.map(~_)*) with Field.Conv[Element[Int, Array[Int]]] {
    given instance: Conv[S] = this
  }
}
