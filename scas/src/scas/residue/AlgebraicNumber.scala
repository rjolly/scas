package scas.residue

import scala.compiletime.deferred
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialWithModInverse
import scas.polynomial.tree.PolynomialOverFieldWithGB
import scas.power.degree.{ArrayPowerProduct, DegreeReverseLexicographic}
import scas.structure.commutative.Field
import scas.variable.Variable
import AlgebraicNumber.Conv

class AlgebraicNumber[C, N : {Numeric, ClassTag}](using PolynomialWithModInverse[Element[C, Array[N]], C, Array[N]])(s: Element[C, Array[N]]*) extends Conv[C, N] {
  given instance: AlgebraicNumber[C, N] = this
  update(s*)
}

object AlgebraicNumber {
  def galoisField = GaloisField
  def growable = scas.residue.growable.AlgebraicNumber

  trait Impl[T, C, M] extends ResidueOverUFD.Impl[T, C, M] with Field[T] {
    given ring: PolynomialWithModInverse[T, C, M] = deferred
    extension (x: T) def isSquare = true
    def sqrt(x: T) = {
      val n = ring.pp.variables.indexOf(Variable.sqrt(x))
      assert (n > -1)
      generator(n)
    }
    def inverse(x: T) = x.modInverse(mods*)

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
