package scas.residue

import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialWithModInverse
import scas.polynomial.tree.PolynomialOverFieldWithGB
import scas.power.degree.{ArrayPowerProduct, DegreeReverseLexicographic}
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import AlgebraicNumber.Impl

abstract class AlgebraicNumber[C](using PolynomialWithModInverse[Element[C, Array[Int]], C, Array[Int]]) extends Impl[C, Array[Int]] with Field.Conv[Element[C, Array[Int]]] {
  def this(ring: Field[C])(pp: ArrayPowerProduct[Int]) = this(using new PolynomialOverFieldWithGB(using ring, pp))
  def this(ring: Field[C])(variables: Variable*) = this(ring)(new DegreeReverseLexicographic[Int](variables*))
  def instance: AlgebraicNumber[C]
}

object AlgebraicNumber {
  def apply[C](ring: PolynomialWithModInverse[Element[C, Array[Int]], C, Array[Int]])(s: Element[C, Array[Int]]*) = new Conv(using ring)(s*)
  def galoisField[S : Conversion[Variable]](str: String)(s: S*) = new GaloisField(str)(s.map(~_)*)
  def growable[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new scas.residue.growable.AlgebraicNumber.Conv(ring)(s.map(~_)*)

  trait Impl[C, M] extends ResidueOverField[Element[C, M], C, M]

  class Conv[C](using PolynomialWithModInverse[Element[C, Array[Int]], C, Array[Int]])(s: Element[C, Array[Int]]*) extends AlgebraicNumber[C] {
    given instance: AlgebraicNumber[C] = this
    update(s*)
  }
}
