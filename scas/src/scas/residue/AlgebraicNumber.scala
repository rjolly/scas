package scas.residue

import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.PolynomialOverField
import scas.structure.commutative.Field
import scas.variable.Variable
import scas.util.Conversion
import scas.base.ModInteger

class AlgebraicNumber[C, S : Conversion[Variable]](using Field[C])(s: S) extends Residue[Element[C, Array[Int]], C, Array[Int]] {
  given ring: PolynomialOverField[Element[C, Array[Int]], C, Array[Int]] = new UnivariatePolynomial(s)
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S) = new conversion.AlgebraicNumber(using ring)(s)
  def galoisField[S : Conversion[Variable]](str: String)(s: S) = this(ModInteger(str))(s)
}
