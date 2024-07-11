package scas.base

import scas.structure.commutative.StarUFD
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.power.Lexicographic
import scas.variable.Variable
import scas.residue.Residue
import BigInteger.given
import Rational.given

type Complex = Element[Rational, Array[Int]]

object Complex extends Complex.Impl(using new UnivariatePolynomial(using Rational)(Variable.sqrt(BigInteger("-1")))) with scas.residue.conversion.Residue[Complex, Rational, Array[Int]] {
  given instance: Complex.type = this
  class Impl(using ring: UnivariatePolynomial[Rational]) extends Residue[Complex, Rational, Array[Int]] with StarUFD[Complex] {
    import ring.pp
    def real(x: Complex) = ring(x.coefficient(pp.one))
    def imag(x: Complex) = ring(x.coefficient(pp.generator(0)))
    override def conjugate(x: Complex) = real(x) - sqrt(-1) * imag(x)
    override def toString = "Complex"
    override def toMathML = "<complexes/>"
  }
  update(1 + sqrt(-1)\2)
}
