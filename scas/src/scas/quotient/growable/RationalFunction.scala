package scas.quotient.growable

import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.growable.PolynomialOverUFD
import scas.quotient.QuotientOverInteger
import scas.base.BigInteger

class RationalFunction[N](using PolynomialOverUFD[Element[BigInteger, Array[N]], BigInteger, Array[N]]) extends QuotientOverInteger[Element[BigInteger, Array[N]], Array[N]] {
  override given ring: PolynomialOverUFD[Element[BigInteger, Array[N]], BigInteger, Array[N]] = summon
}
