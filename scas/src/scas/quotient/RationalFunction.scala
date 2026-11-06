package scas.quotient

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.structure.commutative.Field
import scas.structure.commutative.Quotient
import scas.polynomial.tree.PolynomialWithSubresGCD
import scas.polynomial.ufd.PolynomialOverUFD
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.BigInteger
import RationalFunction.{Impl, Element}

class RationalFunction[N : {Numeric, ClassTag}](using PolynomialOverUFD[Element[Array[N]], BigInteger, Array[N]]) extends Impl[Array[N]] with Field.Conv[Quotient.Element[Element[Array[N]]]] {
  def this(degree: N)(variables: Variable*) = this(using summon, summon, new PolynomialWithSubresGCD(degree)(BigInteger)(variables*))
  given instance: RationalFunction[N] = this
}

object RationalFunction {
  def apply[C, N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(ring: Field[C])(s: S*) = new RationalFunctionOverField(degree)(ring)(s.map(~_)*)
  def integral[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(s: S*) = new RationalFunction(degree)(s.map(~_)*)

  trait Impl[M] extends QuotientOverInteger[Element[M], M] {
    extension[U: Conversion[BigInteger]] (a: U) {
      def %%[V: Conversion[BigInteger]](b: V) = this(ring(~a), ring(~b))
    }
  }

  type Element[M] = scas.polynomial.TreePolynomial.Element[BigInteger, M]
}
