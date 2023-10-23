package scas.base

import scas.polynomial.TreePolynomial.Element
import scas.structure.impl.StarRing
import scas.residue.impl.TreeAlgebraicNumber
import scas.residue.AlgebraicNumber
import scas.power.Lexicographic
import scas.variable.Variable
import BigInteger.given
import Rational.given

type Complex = Element[Rational, Array[Int]]

object Complex extends Complex.Impl with AlgebraicNumber[Complex, Rational, Array[Int]] with scas.structure.StarRing[Complex] {
  given instance: Complex.type = this
  abstract class Impl extends TreeAlgebraicNumber(using Rational, new Lexicographic[Int](Variable.sqrt(BigInteger("-1")))) with StarUFD[Complex] {
    import ring.pp
    def real(x: Complex) = ring(x.coefficient(pp.one))
    def imag(x: Complex) = ring(x.coefficient(pp.generator(0)))
    def conjugate(x: Complex) = ???
    override def toString = "Complex"
    override def toMathML = "<complexes/>"
  }
  override def conjugate(x: Complex) = real(x) - sqrt(-1) * imag(x)
  update(1 + sqrt(-1)\2)
}
