package scas.residue.growable

import scas.base.{BigInteger, Rational, Complex}
import BigInteger.given
import Rational.given
import Complex.Impl

class ComplexNumber extends AlgebraicNumber.Conv(0)(Rational)() with Impl {
  given instance: ComplexNumber = this
}
