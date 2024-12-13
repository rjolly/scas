package scas.residue

import scas.power.DegreeReverseLexicographic
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialOverFieldWithGB
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}
import scas.variable.Variable

class AlgebraicNumber[C](using var ring: PolynomialOverFieldWithGB[Element[C, Array[Int]], C, Int]) extends ResidueOverField[Element[C, Array[Int]], C, Int] {
  def this(ring: Field[C])(s: Variable*) = this(using new scas.polynomial.tree.PolynomialOverFieldWithGB(using ring, new DegreeReverseLexicographic[Int](s*)))
  def extend(s: Variable*): Unit = {
    ring = ring.newInstance((ring.variables ++ s)*)
    mods = mods.map(_.convert)
  }
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new Conv(ring)(s.map(~_)*)

  class Conv[C](ring: Field[C])(s: Variable*) extends AlgebraicNumber(ring)(s*) with Field.Conv[Element[C, Array[Int]]] {
    given instance: Conv[C] = this
  }
}
