package scas.base

import scas.polynomial.TreePolynomial.Element
import scas.polynomial.tree.UnivariatePolynomial
import scas.structure.commutative.impl.StarUFD
import scas.residue.impl.AlgebraicNumber
import scas.power.Lexicographic
import scas.variable.Variable
import BigInteger.given
import Rational.given

type Complex = Element[Rational, Array[Int]]

object Complex extends Complex.Impl with scas.residue.AlgebraicNumber[Complex, Rational, Array[Int]] with scas.structure.commutative.StarUFD[Complex] {
  given ring: UnivariatePolynomial[Rational, Array[Int]] = new UnivariatePolynomial(using Rational, new Lexicographic[Int](Variable.sqrt(BigInteger("-1"))))
  given instance: Complex.type = this
  abstract class Impl extends AlgebraicNumber[Complex, Rational, Array[Int]] with StarUFD[Complex] {
    def real(x: Complex) = this.ring(x.coefficient(this.ring.pp.one))
    def imag(x: Complex) = this.ring(x.coefficient(this.ring.pp.generator(0)))
    override def toString = "Complex"
    override def toMathML = "<complexes/>"
  }
  override def conjugate(x: Complex) = real(x) - sqrt(-1) * imag(x)
  update(1 + sqrt(-1)\2)
}
