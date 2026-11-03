package scas.residue

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.ModInteger

class GaloisField[N : {Numeric, ClassTag}](degree: N)(str: String)(variables: Variable*) extends AlgebraicNumber.Conv(degree)(ModInteger(str))(variables*) {
  given instance: GaloisField[N] = this
}

object GaloisField {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(str: String)(s: S*) = new GaloisField(degree)(str)(s.map(~_)*)
}
