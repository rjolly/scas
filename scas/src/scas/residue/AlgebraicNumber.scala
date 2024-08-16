package scas.residue

import scas.power.DegreeReverseLexicographic
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.PolynomialOverField
import scas.structure.commutative.Field
import AlgebraicNumber.newInstance
import scas.variable.Variable
import scas.util.Conversion
import scas.base.ModInteger

class AlgebraicNumber[C](using var ring: PolynomialOverField[Element[C, Array[Int]], C, Array[Int]]) extends Residue[Element[C, Array[Int]], C, Array[Int]] {
  def this(ring: Field[C])(s: Variable*) = this(using newInstance(ring)(s*))
  def extend(s: Variable*): Unit = {
    ring = newInstance(ring.ring)((ring.variables ++ s)*)
    mods = mods.map(_.convert)
  }
  extension (x: Element[C, Array[Int]]) def convert = ring.convert(x)
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new conversion.AlgebraicNumber(ring)(s*)
  def galoisField[S : Conversion[Variable]](str: String)(s: S*) = this(ModInteger(str))(s*)
  def newInstance[C](ring: Field[C])(s: Variable*) = new scas.polynomial.tree.PolynomialOverField(using ring, DegreeReverseLexicographic(0)(s*))
}
