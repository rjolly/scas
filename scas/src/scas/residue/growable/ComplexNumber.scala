package scas.residue.growable

import scas.variable.Variable
import scas.base.{BigInteger, Rational, Complex}
import BigInteger.given
import Rational.given
import Complex.Impl

class ComplexNumber extends AlgebraicNumber.Conv(0)(Rational)(Variable.sqrt(BigInteger("-1"))) with Impl {
  given instance: ComplexNumber = this
  update(1 + sqrt(-1)\2)
}
