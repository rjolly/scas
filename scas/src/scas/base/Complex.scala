package scas.base

import scas.structure.commutative.{StarUFD, Field}
import scas.polynomial.TreePolynomial.Element
import scas.residue.AlgebraicNumber
import scas.variable.Variable
import BigInteger.given
import Rational.{ring => _, given}

type Complex = Element[Rational, Array[Int]]

object Complex extends Complex.Impl with Field.Conv[Complex] {
  given instance: Complex.type = this
  class Impl extends AlgebraicNumber(Rational)(Variable.sqrt(BigInteger("-1"))) with StarUFD[Complex] {
    def real(x: Complex) = x.coefficient(one)
    def imag(x: Complex) = x.coefficient(generator(0))
    override def conjugate(x: Complex) = real(x) - sqrt(-1) * imag(x)
    override def toString = "Complex"
    override def toMathML = "<complexes/>"
  }
  update(1 + sqrt(-1)\2)
}
