package scas.base

import scas.structure.commutative.StarUFD
import scas.polynomial.TreePolynomial.Element
import scas.residue.growable.AlgebraicNumber
import BigInteger.given
import Rational.given
import Complex.Impl

type Complex = Element[Rational, Array[Int]]

object Complex extends AlgebraicNumber.Conv(0)(Rational)() with Impl {
  given instance: Complex.type = this
  trait Impl extends AlgebraicNumber.Impl[Complex, Rational, Array[Int]] with StarUFD[Complex] {
    def real(x: Complex) = ring.remainder(x)(sqrt(-1))
    def imag(x: Complex) = ring.divide(x)(sqrt(-1))
    override def conjugate(x: Complex) = real(x) - sqrt(-1) * imag(x)
    override def toString = "Complex"
    override def toMathML = "<complexes/>"
    extension (x: Complex) def isMinusOne = x >< -1
    override def sqrt(x: Complex) = {
      assert (x.isMinusOne)
      super.sqrt(x)
    }
  }
}
