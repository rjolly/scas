package scas.base

import scas.polynomial.TreePolynomial.Element
import scas.residue.impl.TreeAlgebraicNumber
import scas.residue.AlgebraicNumber
import scas.power.Lexicographic
import scas.variable.Variable
import BigInteger.given
import Rational.given

type Complex = Element[Rational, Array[Int]]

object Complex extends Complex.Impl with AlgebraicNumber[Complex, Rational, Array[Int]] {
  given instance: Complex.type = this
  class Impl extends TreeAlgebraicNumber(using Rational, new Lexicographic[Int](Variable.sqrt(BigInteger("-1")))) {
    override def toString = "Complex"
    override def toMathML = "<complexes/>"
  }
  update(1 + sqrt(-1)\2)
}
