package scas.residue

import scas.math.Numeric
import scala.reflect.ClassTag
import scas.structure.commutative.Field
import scas.polynomial.TreePolynomial.Element
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.ModInteger

class GaloisField[N : {Numeric, ClassTag}](str: String)(degree: N)(variables: Variable*) extends AlgebraicNumber(using ModInteger(str))(degree)(variables*)

object GaloisField {
  def apply[S : Conversion[Variable]](str: String)(s: S*) = new Conv(str)(0)(s.map(~_)*)

  class Conv[N : {Numeric, ClassTag}](str: String)(degree: N)(variables: Variable*) extends GaloisField(str)(degree)(variables*) with Field.Conv[Element[Int, Array[N]]] {
    given instance: Conv[N] = this
  }
}
