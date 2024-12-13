package scas.residue

import scas.structure.commutative.Field
import scas.polynomial.TreePolynomial.Element
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.ModInteger

class GaloisField(str: String)(s: Variable*) extends AlgebraicNumber(ModInteger(str))(s*)

object GaloisField {
  def apply[S : Conversion[Variable]](str: String)(s: S*) = new Conv(str)(s.map(~_)*)

  class Conv(str: String)(s: Variable*) extends GaloisField(str)(s*) with Field.Conv[Element[Int, Array[Int]]] {
    given instance: Conv = this
  }
}
