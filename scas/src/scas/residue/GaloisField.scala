package scas.residue

import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.PolynomialOverField
import scas.base.ModInteger

object GaloisField {
  def apply(str: String)(s: String*) = new conversion.GaloisField(using UnivariatePolynomial(ModInteger(str))(s: _*))
}
