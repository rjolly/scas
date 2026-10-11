package scas.residue

import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialWithModInverse
import scas.polynomial.tree.PolynomialOverFieldWithGB
import scas.power.degree.{ArrayPowerProduct, DegreeReverseLexicographic}
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}
import scas.variable.Variable

open class AlgebraicNumber[C](using PolynomialWithModInverse[Element[C, Array[Int]], C, Array[Int]]) extends ResidueOverField[Element[C, Array[Int]], C, Array[Int]] {
  def this(ring: Field[C])(pp: ArrayPowerProduct[Int]) = this(using new PolynomialOverFieldWithGB(using ring, pp))
  def this(ring: Field[C])(variables: Variable*) = this(ring)(new DegreeReverseLexicographic[Int](variables*))
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new Conv(ring)(s.map(~_)*)

  class Conv[C](ring: Field[C])(variables: Variable*) extends AlgebraicNumber(ring)(variables*) with Field.Conv[Element[C, Array[Int]]] {
    given instance: Conv[C] = this
  }
}
