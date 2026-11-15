package scas.residue

import scala.compiletime.deferred
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialWithModInverse
import scas.polynomial.tree.PolynomialOverFieldWithGB
import scas.power.{ArrayPowerProduct, Lexicographic}
import scas.structure.commutative.Field
import scas.base.BigInteger.given
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
    def variables = ring.pp.variables
    def generator(a: Variable, c: T => T): T = {
      assert (variables.contains(a))
      generator(variables.indexOf(a))
    }
    extension (x: T) def nthrt(n: Int, dummy: Boolean) = {
      val (a, b) = ring.nthrt(x)(n, dummy)
      if b.isOne then (a, b) else (a * generator(if n == 2 then Variable.sqrt(b) else Variable.pow(b, inverse(n)), s => b - s \ n), one)
    }
    def solve(equ: T, x: T, n: Int) = {
      val s = ring.coefficients(equ)(x)
      generator(Variable.solve(equ, x, fromInt(n)), s => equ.substitute(x, s))
    }
    def inverse(x: T) = x.modInverse(mods*)

    extension (ring: PolynomialWithModInverse[T, C, M]) def apply(s: T*) = {
      same(s*)
      this
    }
  }

  abstract class Conv[C, N : {Numeric, ClassTag}](using PolynomialWithModInverse[Element[C, Array[N]], C, Array[N]]) extends Impl[Element[C, Array[N]], C, Array[N]] with Field.Conv[Element[C, Array[N]]] {
    def this(ring: Field[C])(pp: ArrayPowerProduct[N]) = this(using summon, summon, new PolynomialOverFieldWithGB(using ring, pp))
    def this(degree: N)(ring: Field[C])(variables: Variable*) = this(ring)(new Lexicographic(degree)(variables*))
    def instance: Conv[C, N]
  }
}
