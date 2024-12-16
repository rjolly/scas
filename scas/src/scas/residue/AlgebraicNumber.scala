package scas.residue

import scas.power.DegreeReverseLexicographic
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialOverFieldWithGB
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}
import scas.variable.Variable

class AlgebraicNumber[C](var using_ring: PolynomialOverFieldWithGB[Element[C, Array[Int]], C, Int]) extends ResidueOverField[Element[C, Array[Int]], C, Int] {
  def this(ring: Field[C])(s: Variable*) = this(new scas.polynomial.tree.PolynomialOverFieldWithGB(using ring, new DegreeReverseLexicographic[Int](s*)))
  override given ring: () => PolynomialOverFieldWithGB[Element[C, Array[Int]], C, Int] = using_ring
  def extend(s: Variable*): Unit = {
    using_ring = ring.newInstance((ring.variables ++ s)*)
    mods = mods.map(_.convert)
  }
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new Conv(ring)(s.map(~_)*)

  class Conv[C](ring: Field[C])(s: Variable*) extends AlgebraicNumber(ring)(s*) with Field.Conv[Element[C, Array[Int]]] {
    given instance: Conv[C] = this
  }
}
