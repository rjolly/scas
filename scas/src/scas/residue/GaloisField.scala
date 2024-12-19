package scas.residue

import scas.structure.commutative.Field
import scas.polynomial.TreePolynomial.Element
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.ModInteger

class GaloisField(str: String)(variables: Variable*) extends AlgebraicNumber(ModInteger(str))(variables*)

object GaloisField {
  def apply[S : Conversion[Variable]](str: String)(s: S*) = new Conv(str)(s.map(~_)*)

  class Conv(str: String)(variables: Variable*) extends GaloisField(str)(variables*) with Field.Conv[Element[Int, Array[Int]]] {
    given instance: Conv = this
  }
}
