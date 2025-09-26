package scas.residue

import scas.power.growable.DegreeReverseLexicographic
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialOverFieldWithGB
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}
import scas.variable.Variable

open class AlgebraicNumber[C](var using_ring: PolynomialOverFieldWithGB[Element[C, Array[Int]], C, Int]) extends ResidueOverField[Element[C, Array[Int]], C, Int] {
  def this(ring: Field[C])(variables: Variable*) = this(new scas.polynomial.tree.PolynomialOverFieldWithGB(using ring, new DegreeReverseLexicographic[Int](variables*)))
  override given ring: () => PolynomialOverFieldWithGB[Element[C, Array[Int]], C, Int] = using_ring
  def extend(variables: Variable*): Unit = {
    using_ring = ring.newInstance((ring.pp.variables ++ variables)*)
    mods = mods.map(_.convert)
  }
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new Conv(ring)(s.map(~_)*)

  class Conv[C](ring: Field[C])(variables: Variable*) extends AlgebraicNumber(ring)(variables*) with Field.Conv[Element[C, Array[Int]]] {
    given instance: Conv[C] = this
  }
}
