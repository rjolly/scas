package scas.residue

import scas.structure.commutative.Field
import scas.variable.Variable
import scas.util.Conversion
import scas.base.ModInteger

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new conversion.AlgebraicNumber(using ring)(s*)
  def galoisField[S : Conversion[Variable]](str: String)(s: S*) = this(ModInteger(str))(s*)
}
