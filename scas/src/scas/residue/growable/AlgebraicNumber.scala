package scas.residue.growable

import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.growable.PolynomialWithModInverse
import scas.polynomial.tree.growable.PolynomialOverFieldWithGB
import scas.power.growable.{ArrayPowerProduct, DegreeReverseLexicographic}
import scas.structure.commutative.Field
import scas.variable.Variable

abstract class AlgebraicNumber[C](using PolynomialWithModInverse[Element[C, Array[Int]], C, Array[Int]]) extends ResidueOverField[Element[C, Array[Int]], C, Array[Int]] with Field.Conv[Element[C, Array[Int]]] {
  def this(ring: Field[C])(pp: ArrayPowerProduct[Int]) = this(using new PolynomialOverFieldWithGB(using ring, pp))
  def this(ring: Field[C])(variables: Variable*) = this(ring)(new DegreeReverseLexicographic[Int](variables*))
  def instance: AlgebraicNumber[C]
}

object AlgebraicNumber {
  class Conv[C](ring: Field[C])(variables: Variable*) extends AlgebraicNumber(ring)(variables*) {
    given instance: AlgebraicNumber[C] = this
  }
}
