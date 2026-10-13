package scas.residue

import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.ModInteger

class GaloisField(str: String)(variables: Variable*) extends AlgebraicNumber(ModInteger(str))(variables*) {
  given instance: AlgebraicNumber[Int] = this
}

object GaloisField {
  def apply[S : Conversion[Variable]](str: String)(s: S*) = new GaloisField(str)(s.map(~_)*)
}
