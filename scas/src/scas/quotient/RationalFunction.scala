package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.{Element as Quotient_Element}
import scas.polynomial.tree.PolynomialWithSubresGCD
import scas.polynomial.ufd.PolynomialOverUFD
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.BigInteger
import RationalFunction.{Impl, Element}

class RationalFunction(using PolynomialOverUFD[Element[Array[Int]], BigInteger, Array[Int]]) extends Impl[Array[Int]] with Field.Conv[Quotient_Element[Element[Array[Int]]]] {
  def this(variables: Variable*) = this(using new PolynomialWithSubresGCD(BigInteger)(variables*))
  given instance: RationalFunction = this
}

object RationalFunction {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new RationalFunctionOverField(ring)(s.map(~_)*)
  def integral[S : Conversion[Variable]](s: S*) = new RationalFunction(s.map(~_)*)

  trait Impl[M] extends QuotientOverInteger[Element[M], M] {
    extension[U: Conversion[BigInteger]] (a: U) {
      def %%[V: Conversion[BigInteger]](b: V) = this(ring(~a), ring(~b))
    }
  }

  type Element[M] = scas.polynomial.TreePolynomial.Element[BigInteger, M]
}
