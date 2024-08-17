package scas.quotient.conversion

import scas.structure.commutative.conversion.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.TreePolynomial
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.BigInteger

class RationalFunction[S : Conversion[Variable]](s: S*) extends scas.quotient.RationalFunction(s.map(~_)*) with Field[Element[TreePolynomial.Element[BigInteger, Array[Int]]]] {
  given instance: RationalFunction[S] = this
  extension[U: Conversion[BigInteger]] (a: U) {
    def %%[V: Conversion[BigInteger]](b: V) = this(ring(~a), ring(~b))
  }
}
