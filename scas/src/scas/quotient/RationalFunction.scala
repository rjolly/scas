package scas.quotient

import scas.structure.commutative.Field
import scas.polynomial.tree.{MultivariatePolynomial, PolynomialWithSubresGCD}
import scas.polynomial.PolynomialOverUFD
import scas.base.BigInteger
import scas.variable.Variable
import scas.util.Conversion

class RationalFunction(using val ring: PolynomialOverUFD[MultivariatePolynomial.Element[BigInteger], BigInteger, Array[Int]]) extends QuotientOverInteger[MultivariatePolynomial.Element[BigInteger], Array[Int]] {
  def this(s: Variable*) = this(using new PolynomialWithSubresGCD(using BigInteger)(s*))
}

object RationalFunction {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new conversion.RationalFunctionOverField(ring)(s*)
  def integral[S : Conversion[Variable]](s: S*) = new conversion.RationalFunction(s*)
}
