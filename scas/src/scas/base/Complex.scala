package scas.base

import scas.structure.commutative.StarUFD
import scas.polynomial.TreePolynomial.Element
import scas.residue.AlgebraicNumber
import scas.variable.Variable
import BigInteger.given
import Rational.Implicits.given

type Complex = Element[Rational, Array[Int]]

object Complex extends Complex.Impl {
  given instance: Complex.type = this
  object Implicits {
    export Complex.{instance, coef2poly}
  }
  class Impl extends AlgebraicNumber(Rational)(Variable.sqrt(BigInteger("-1"))) with StarUFD[Complex] {
    update(sqrt(-1)\2 + 1)
    def real(x: Complex) = x.coefficient(one)
    def imag(x: Complex) = x.coefficient(generator(0))
    override def conjugate(x: Complex) = real(x) - sqrt(-1) * imag(x)
    override def toString = "Complex"
    override def toMathML = "<complexes/>"
  }
}
