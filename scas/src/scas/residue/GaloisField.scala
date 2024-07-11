package scas.residue

import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.PolynomialOverField
import scas.variable.Variable
import scas.util.Conversion
import scas.base.ModInteger

object GaloisField {
  def apply[S : Conversion[Variable]](str: String)(s: S*) = new conversion.GaloisField(using UnivariatePolynomial(ModInteger(str))(s*))
}
