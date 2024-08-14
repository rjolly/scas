package scas.residue

import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.PolynomialOverField
import scas.structure.commutative.Field
import scas.variable.Variable
import scas.util.Conversion
import scas.base.ModInteger

class AlgebraicNumber[C](using PolynomialOverField[Element[C, Array[Int]], C, Array[Int]]) extends Residue[Element[C, Array[Int]], C, Array[Int]] {
  def this(ring: Field[C])(s: Variable) = this(using new UnivariatePolynomial(using ring)(s))
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S) = new conversion.AlgebraicNumber(ring)(s)
  def galoisField[S : Conversion[Variable]](str: String)(s: S) = this(ModInteger(str))(s)
}
