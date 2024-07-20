package scas.quotient.conversion

import scas.polynomial.tree.PolynomialWithSimpleGCD
import scas.structure.commutative.conversion.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.TreePolynomial
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.BigInteger

class RationalFunction[S : Conversion[Variable]](s: S*) extends scas.quotient.RationalFunction[TreePolynomial.Element[BigInteger, Array[Int]], Array[Int]] with Field[Element[TreePolynomial.Element[BigInteger, Array[Int]]]] {
  given ring: PolynomialWithSimpleGCD[BigInteger, S] = new PolynomialWithSimpleGCD(using BigInteger)(s*)
  given instance: RationalFunction[S] = this
  extension[U: Conversion[BigInteger]] (a: U) {
    def %%[V: Conversion[BigInteger]](b: V) = this(ring(~a), ring(~b))
  }
}
