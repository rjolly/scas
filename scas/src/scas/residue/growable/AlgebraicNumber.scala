package scas.residue.growable

import scala.compiletime.deferred
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.growable.PolynomialWithModInverse
import scas.polynomial.tree.growable.PolynomialOverFieldWithGB
import scas.power.growable.{ArrayPowerProduct, DegreeReverseLexicographic}
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import AlgebraicNumber.Impl

abstract class AlgebraicNumber[C](using PolynomialWithModInverse[Element[C, Array[Int]], C, Array[Int]]) extends Impl[Element[C, Array[Int]], C, Array[Int]] with Field.Conv[Element[C, Array[Int]]] {
  def this(ring: Field[C])(pp: ArrayPowerProduct[Int]) = this(using new PolynomialOverFieldWithGB(using ring, pp))
  def this(ring: Field[C])(variables: Variable*) = this(ring)(new DegreeReverseLexicographic[Int](variables*))
  def instance: AlgebraicNumber[C]
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new Conv(ring)(s.map(~_)*)

  trait Impl[T, C, M] extends scas.residue.AlgebraicNumber.Impl[T, C, M] with Residue[T, C, M] {
    given ring: PolynomialWithModInverse[T, C, M] = deferred

    extension (ring: PolynomialWithModInverse[T, C, M]) def apply(s: T*) = {
      same(s*)
      this
    }
  }

  class Conv[C](ring: Field[C])(variables: Variable*) extends AlgebraicNumber(ring)(variables*) {
    given instance: AlgebraicNumber[C] = this
  }
}
