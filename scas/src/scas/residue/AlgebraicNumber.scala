package scas.residue

import scala.compiletime.deferred
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialWithModInverse
import scas.polynomial.tree.PolynomialOverFieldWithGB
import scas.power.degree.{ArrayPowerProduct, DegreeReverseLexicographic}
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
    extension (x: T) {
      def isPower(n: Int) = true
      def nthrt(n: Int) = if ring.isPower(x)(n) then ring.nthrt(x)(n) else generator(if n == 2 then Variable.sqrt(x) else Variable.pow(x, inverse(n)), s => x - s \ n)
    }
    def solve(equ: T, x: T, n: Int) = {
      val s = ring.coefficients(equ)(x)
      s.length - 1 match {
        case 1 => linear(s(0) / s(1), n)
        case 2 => quadratic(s(1) / s(2), s(0) / s(2), n)
        case 3 => cubic(s(2) / s(3), s(1) / s(3), s(0) / s(3), n)
        case 4 => quartic(s(3) / s(4), s(2) / s(4), s(1) / s(4), s(0) / s(4), n)
        case _ => generator(Variable.solve(equ, x, fromInt(n)), s => equ.substitute(x, s))
      }
    }
    def linear(a: T, n: Int) = -a
    def quadratic(a: T, b: T, n: Int) = {
      val d = a \ 2 - fromInt(4) * b
      n % 2 match {
        case 0 => (a - sqrt(d)) / 2
        case _ => (a + sqrt(d)) / 2
      }
    }
    def cubic(a: T, b: T, c: T, n: Int) = {
      val y = for i <- 0 until 2 yield quadratic(a \ 6 - fromInt(9) * a \ 4 * b + fromInt(27) * a \2 * b \ 2 - fromInt(27) * b \ 3, fromInt(2) * a \ 3 - fromInt(9) * a * b + fromInt(27) * c, i).nthrt(3)
      val h = inverse(2)
      val j = -h * (fromInt(1) - sqrt(-3))
      val jbar = -h * (fromInt(1) + sqrt(-3))
      n % 3 match {
        case 0 => -(a - y(0) - y(1)) / 3
        case 1 => -(a - j * y(0) - jbar * y(1)) / 3
        case _ => -(a - jbar * y(0) - j * y(1)) / 3
      }
    }
    def quartic(a: T, b: T, c: T, d: T, n: Int) = {
      val y = for i <- 0 until 3 yield sqrt(cubic(-a \ 6 + fromInt(8) * a \ 4 * b - fromInt(16) * a \ 2 * b \ 2 - fromInt(16) * a \ 3 * c + fromInt(64) * a * b * c - fromInt(64) * c \ 2, fromInt(3) * a \ 4 - fromInt(16) * a \ 2 * b + fromInt(16) * b \ 2 + fromInt(16) * a * c - fromInt(64) * d, -fromInt(3) * a \ 2 + fromInt(8) * b, i))
      n % 4 match {
        case 0 => -(a + y(0) - y(1) - y(2)) / 4
        case 1 => -(a - y(0) - y(1) + y(2)) / 4
        case 2 => -(a + y(0) + y(1) + y(2)) / 4
        case _ => -(a - y(0) + y(1) - y(2)) / 4
      }
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
