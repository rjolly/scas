package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.{Element as Quotient_Element}
import scas.polynomial.tree.PolynomialWithSubresGCD
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialOverUFD
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.BigInteger

class RationalFunction(using PolynomialOverUFD[Element[BigInteger, Array[Int]], BigInteger, Array[Int]]) extends QuotientOverInteger[Element[BigInteger, Array[Int]], Array[Int]] {
  def this(variables: Variable*) = this(using new PolynomialWithSubresGCD(using BigInteger)(variables*))
}

object RationalFunction {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new RationalFunctionOverField.Conv(ring)(s.map(~_)*)
  def integral[S : Conversion[Variable]](s: S*) = new Conv(s.map(~_)*)

  class Conv(variables: Variable*) extends RationalFunction(variables*) with Field.Conv[Quotient_Element[Element[BigInteger, Array[Int]]]] {
    given instance: Conv = this
    extension[U: Conversion[BigInteger]] (a: U) {
      def %%[V: Conversion[BigInteger]](b: V) = this(ring(~a), ring(~b))
    }
  }
}
