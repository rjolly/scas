package scas.residue.growable.convertable

import scala.compiletime.deferred
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.polynomial.tree.PolynomialWithGB.Element
import scas.polynomial.ufd.growable.convertable.PolynomialOverFieldWithGB
import scas.power.growable.splitable.{ArrayPowerProduct, Lexicographic}
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import AlgebraicNumber.Conv

class AlgebraicNumber[C, N : {Numeric, ClassTag}](degree: N)(ring: Field[C])(variables: Variable*) extends Conv(degree)(ring)(variables*) {
  given instance: AlgebraicNumber[C, N] = this
}

object AlgebraicNumber {
  def apply[C, N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(ring: Field[C])(s: S*) = new AlgebraicNumber(degree)(ring)(s.map(~_)*)

  trait Impl[T[N], C, N] extends scas.residue.growable.AlgebraicNumber.Impl[T[Array[N]], C, Array[N]] {
    given ring: PolynomialOverFieldWithGB[T, C, N] = deferred
    override def generator(a: Variable, c: T[Array[N]] => T[Array[N]]) = super.generator(a, c)

    extension (ring: PolynomialOverFieldWithGB[T, C, N]) def apply(s: T[Array[N]]*) = {
      same(s*)
      this
    }
  }

  abstract class Conv[C, N : {Numeric, ClassTag}](using PolynomialOverFieldWithGB[Element[C], C, N]) extends Impl[Element[C], C, N] with Field.Conv[Element[C][Array[N]]] {
    def this(ring: Field[C])(pp: ArrayPowerProduct[N]) = this(using summon, summon, new scas.polynomial.tree.growable.convertable.PolynomialOverFieldWithGB(ring, pp))
    def this(degree: N)(ring: Field[C])(variables: Variable*) = this(ring)(new Lexicographic(degree)(variables*))
    def instance: Conv[C, N]
  }
}
