package scas.base

import scas.structure.commutative.StarUFD
import scas.polynomial.TreePolynomial.Element
import scas.residue.AlgebraicNumber
import scas.variable.Variable
import BigInteger.given
import Rational.{ring as _, given}
import Complex.Impl

type Complex = Element[Rational, Array[Int]]

object Complex extends AlgebraicNumber(Rational)(Variable.sqrt(BigInteger("-1"))) with Impl {
  given instance: Complex.type = this
  trait Impl extends AlgebraicNumber.Impl[Complex, Rational, Array[Int]] with StarUFD[Complex] {
    def real(x: Complex) = x.coefficient(one)
    def imag(x: Complex) = x.coefficient(generator(0))
    override def conjugate(x: Complex) = real(x) - sqrt(-1) * imag(x)
    override def toString = "Complex"
    override def toMathML = "<complexes/>"
  }
  update(1 + sqrt(-1)\2)
}
