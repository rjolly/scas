package scas.quotient

import scas.structure.commutative.Field
import scas.polynomial.tree.PolynomialWithSubresGCD
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.PolynomialOverUFD
import scas.base.BigInteger
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class RationalFunction(using val ring: PolynomialOverUFD[Element[BigInteger, Array[Int]], BigInteger, Array[Int]]) extends QuotientOverInteger[Element[BigInteger, Array[Int]], Array[Int]] {
  def this(s: Variable*) = this(using new PolynomialWithSubresGCD(using BigInteger)(s*))
}

object RationalFunction {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new RationalFunctionOverField(ring)(s.map(~_)*)
  def integral[S : Conversion[Variable]](s: S*) = new RationalFunction(s.map(~_)*)
}
