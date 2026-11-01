package scas.residue.growable

import scala.compiletime.deferred
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.growable.ufd.PolynomialWithModInverse
import scas.polynomial.tree.growable.PolynomialOverFieldWithGB
import scas.power.growable.{ArrayPowerProduct, DegreeReverseLexicographic}
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import AlgebraicNumber.Impl

abstract class AlgebraicNumber[C, N : {Numeric, ClassTag}](using PolynomialWithModInverse[Element[C, Array[N]], C, Array[N]]) extends Impl[Element[C, Array[N]], C, Array[N]] with Field.Conv[Element[C, Array[N]]] {
  def this(ring: Field[C])(pp: ArrayPowerProduct[N]) = this(using summon, summon, new PolynomialOverFieldWithGB(using ring, pp))
  def this(degree: N)(ring: Field[C])(variables: Variable*) = this(ring)(new DegreeReverseLexicographic[N](variables*))
  def instance: AlgebraicNumber[C, N]
}

object AlgebraicNumber {
  def apply[C, N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(ring: Field[C])(s: S*) = new Conv(degree)(ring)(s.map(~_)*)

  trait Impl[T, C, M] extends scas.residue.AlgebraicNumber.Impl[T, C, M] with Residue[T, C, M] {
    given ring: PolynomialWithModInverse[T, C, M] = deferred

    extension (ring: PolynomialWithModInverse[T, C, M]) def apply(s: T*) = {
      same(s*)
      this
    }
  }

  class Conv[C, N : {Numeric, ClassTag}](degree: N)(ring: Field[C])(variables: Variable*) extends AlgebraicNumber(degree)(ring)(variables*) {
    given instance: AlgebraicNumber[C, N] = this
  }
}
