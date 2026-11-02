package scas.quotient.growable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.quotient.RationalFunction.Element
import scas.polynomial.growable.ufd.PolynomialOverUFD
import scas.base.BigInteger

class RationalFunction[N : {Numeric, ClassTag}](using PolynomialOverUFD[Element[Array[N]], BigInteger, Array[N]]) extends scas.quotient.RationalFunction.Impl[Array[N]] {
  override given ring: PolynomialOverUFD[Element[Array[N]], BigInteger, Array[N]] = summon
}
