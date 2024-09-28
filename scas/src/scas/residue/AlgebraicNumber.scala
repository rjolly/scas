package scas.residue

import scas.power.DegreeReverseLexicographic
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.tree.PolynomialOverField
import scas.polynomial.PolynomialOverFieldWithGB
import scas.structure.commutative.Field
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class AlgebraicNumber[C](using ring: PolynomialOverFieldWithGB[Element[C, Array[Int]], C, Int]) extends ResidueOverField[Element[C, Array[Int]], C, Int] {
  def this(ring: Field[C])(s: Variable*) = this(using new PolynomialOverField(using ring, DegreeReverseLexicographic(0)(s*)))
  def extend(s: Variable*): Unit = {
    ring = ring.newInstance((ring.variables ++ s)*)
    mods = mods.map(_.convert)
  }
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new AlgebraicNumber(ring)(s.map(~_)*)
}
