package scas.quotient.growable

import scas.quotient.RationalFunction.Element
import scas.polynomial.growable.ufd.PolynomialOverUFD
import scas.base.BigInteger

class RationalFunction(using PolynomialOverUFD[Element[Array[Int]], BigInteger, Array[Int]]) extends scas.quotient.RationalFunction.Impl[Array[Int]] {
  override given ring: PolynomialOverUFD[Element[Array[Int]], BigInteger, Array[Int]] = summon
}
