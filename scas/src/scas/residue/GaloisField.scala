package scas.residue

import scas.variable.Variable
import scas.util.Conversion

object GaloisField {
  def apply[S : Conversion[Variable]](str: String)(s: S*) = new conversion.GaloisField(str)(s*)
}
