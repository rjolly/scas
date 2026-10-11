package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.{Element as Quotient_Element}
import scas.polynomial.tree.PolynomialWithSubresGCD
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialOverUFD
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.BigInteger
import RationalFunction.Impl

class RationalFunction(using PolynomialOverUFD[Element[BigInteger, Array[Int]], BigInteger, Array[Int]]) extends Impl[Array[Int]] with Field.Conv[Quotient_Element[Element[BigInteger, Array[Int]]]] {
  def this(variables: Variable*) = this(using new PolynomialWithSubresGCD(BigInteger)(variables*))
  given instance: RationalFunction = this
}

object RationalFunction {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new RationalFunctionOverField(ring)(s.map(~_)*)
  def integral[S : Conversion[Variable]](s: S*) = new RationalFunction(s.map(~_)*)

  trait Impl[M] extends QuotientOverInteger[Element[BigInteger, M], M] {
    extension[U: Conversion[BigInteger]] (a: U) {
      def %%[V: Conversion[BigInteger]](b: V) = this(ring(~a), ring(~b))
    }
  }
}
