package scas.residue

import scas.variable.Variable
import scas.util.Conversion
import scas.base.ModInteger

class GaloisField(str: String)(s: Variable*) extends AlgebraicNumber(ModInteger(str))(s*)

object GaloisField {
  def apply[S : Conversion[Variable]](str: String)(s: S*) = new conversion.GaloisField(str)(s*)
}
