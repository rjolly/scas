package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.{Element as Quotient_Element}
import scas.polynomial.tree.MultivariatePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialOverUFD
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.BigInteger

class RationalFunction[N](using PolynomialOverUFD[Element[BigInteger, Array[N]], BigInteger, Array[N]]) extends QuotientOverInteger[Element[BigInteger, Array[N]], Array[N]]

object RationalFunction {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = RationalFunctionOverField(ring)(s*)
  def integral[S : Conversion[Variable]](s: S*) = new Conv(MultivariatePolynomial.withSubresGCD(BigInteger)(s*))

  class Conv[N](ring: PolynomialOverUFD[Element[BigInteger, Array[N]], BigInteger, Array[N]]) extends RationalFunction(using ring) with Field.Conv[Quotient_Element[Element[BigInteger, Array[N]]]] {
    given instance: Conv[N] = this
    extension[U: Conversion[BigInteger]] (a: U) {
      def %%[V: Conversion[BigInteger]](b: V) = this(ring(~a), ring(~b))
    }
  }
}
