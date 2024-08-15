package scas.residue

import scas.power.DegreeReverseLexicographic
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.PolynomialOverField
import scas.structure.commutative.Field
import scas.variable.Variable
import scas.util.Conversion
import scas.base.ModInteger

class AlgebraicNumber[C](using PolynomialOverField[Element[C, Array[Int]], C, Array[Int]]) extends Residue[Element[C, Array[Int]], C, Array[Int]] {
  def this(ring: Field[C])(s: Variable*) = this(using new scas.polynomial.tree.PolynomialOverField(using ring, DegreeReverseLexicographic(0)(s*)))
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new conversion.AlgebraicNumber(ring)(s*)
  def galoisField[S : Conversion[Variable]](str: String)(s: S*) = this(ModInteger(str))(s*)
}
