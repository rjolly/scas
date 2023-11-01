package scas.base

import scas.structure.commutative.impl.StarUFD
import scas.residue.impl.AlgebraicNumber
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.power.Lexicographic
import scas.variable.Variable
import BigInteger.given
import Rational.given

type Complex = Element[Rational, Array[Int]]

object Complex extends Complex.Impl(using new UnivariatePolynomial(using Rational, Lexicographic[Int](Variable.sqrt(BigInteger("-1"))))) with scas.residue.AlgebraicNumber[Rational, Array[Int]] with scas.structure.commutative.StarUFD[Complex] {
  given instance: Complex.type = this
  abstract class Impl(using ring: UnivariatePolynomial[Rational, Array[Int]]) extends AlgebraicNumber[Rational, Array[Int]] with StarUFD[Complex] {
    import ring.pp
    def real(x: Complex) = ring(x.coefficient(pp.one))
    def imag(x: Complex) = ring(x.coefficient(pp.generator(0)))
    override def toString = "Complex"
    override def toMathML = "<complexes/>"
  }
  override def conjugate(x: Complex) = real(x) - sqrt(-1) * imag(x)
  update(1 + sqrt(-1)\2)
}
