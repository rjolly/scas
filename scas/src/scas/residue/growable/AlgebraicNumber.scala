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
import scas.base.BigInteger.given
import scas.variable.Variable
import AlgebraicNumber.Conv

class AlgebraicNumber[C, N : {Numeric, ClassTag}](degree: N)(ring: Field[C])(variables: Variable*) extends Conv(degree)(ring)(variables*) {
  given instance: AlgebraicNumber[C, N] = this
}

object AlgebraicNumber {
  def apply[C, N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(ring: Field[C])(s: S*) = new AlgebraicNumber(degree)(ring)(s.map(~_)*)

  trait Impl[T, C, M] extends scas.residue.AlgebraicNumber.Impl[T, C, M] with Residue[T, C, M] {
    given ring: PolynomialWithModInverse[T, C, M] = deferred
    override def generator(a: Variable, c: T => T) = {
      if variables.contains(a) then generator(variables.indexOf(a))
      else {
        extend(a)
        val s = generator(variables.length - 1)
        update(c(s))
        s
      }
    }

    extension (ring: PolynomialWithModInverse[T, C, M]) def apply(s: T*) = {
      same(s*)
      this
    }
  }

  abstract class Conv[C, N : {Numeric, ClassTag}](using PolynomialWithModInverse[Element[C, Array[N]], C, Array[N]]) extends Impl[Element[C, Array[N]], C, Array[N]] with Field.Conv[Element[C, Array[N]]] {
    def this(ring: Field[C])(pp: ArrayPowerProduct[N]) = this(using summon, summon, new PolynomialOverFieldWithGB(using ring, pp))
    def this(degree: N)(ring: Field[C])(variables: Variable*) = this(ring)(new DegreeReverseLexicographic(degree)(variables*))
    def instance: Conv[C, N]
  }
}
