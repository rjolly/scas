package scas.quotient.growable

import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.growable.PolynomialOverUFD
import scas.quotient.QuotientOverInteger
import scas.base.BigInteger

class RationalFunction(using PolynomialOverUFD[Element[BigInteger, Array[Int]], BigInteger, Array[Int]]) extends QuotientOverInteger[Element[BigInteger, Array[Int]], Array[Int]] {
  override given ring: PolynomialOverUFD[Element[BigInteger, Array[Int]], BigInteger, Array[Int]] = summon
}
