package scas.residue

import scala.compiletime.deferred
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialWithModInverse
import scas.polynomial.tree.PolynomialOverFieldWithGB
import scas.power.degree.{ArrayPowerProduct, DegreeReverseLexicographic}
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
  def apply[C](ring: PolynomialWithModInverse[Element[C, Array[Int]], C, Array[Int]])(s: Element[C, Array[Int]]*) = new Conv(using ring)(s*)
  def galoisField = GaloisField
  def growable = scas.residue.growable.AlgebraicNumber

  trait Impl[T, C, M] extends Residue[T, C, M] with Field[T] {
    given ring: PolynomialWithModInverse[T, C, M] = deferred
    def sqrt[U: Conversion[T]](x: U): T = sqrt(~x)
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

  class Conv[C](using PolynomialWithModInverse[Element[C, Array[Int]], C, Array[Int]])(s: Element[C, Array[Int]]*) extends AlgebraicNumber[C] {
    given instance: AlgebraicNumber[C] = this
    update(s*)
  }
}
