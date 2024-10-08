package scas.residue

import scas.power.DegreeReverseLexicographic
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.tree.PolynomialOverField
import scas.polynomial.ufd.PolynomialOverFieldWithGB
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}
import scas.variable.Variable

class AlgebraicNumber[C](using var ring: PolynomialOverFieldWithGB[Element[C, Array[Int]], C, Int]) extends ResidueOverField[Element[C, Array[Int]], C, Int] {
  def this(ring: Field[C])(s: Variable*) = this(using new PolynomialOverField(using ring, DegreeReverseLexicographic(0)(s*)))
  def extend(s: Variable*): Unit = {
    ring = ring.newInstance((ring.variables ++ s)*)
    mods = mods.map(_.convert)
  }
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new Conv(ring)(s*)

  class Conv[C, S : Conversion[Variable]](ring: Field[C])(s: S*) extends AlgebraicNumber(ring)(s.map(~_)*) with Field.Conv[Element[C, Array[Int]]] {
    given instance: Conv[C, S] = this
  }
}
