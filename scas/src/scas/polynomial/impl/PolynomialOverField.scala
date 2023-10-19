package scas.polynomial.impl

import scala.annotation.tailrec
import scas.structure.commutative.impl.Field

trait PolynomialOverField[T, C : Field, M] extends PolynomialOverUFD[T, C, M] {
  extension (x: T) override def %/ (c: C) = x%* Field[C].inverse(c)
  def monic(x: T) = if (x.isZero) zero else x%/ headCoefficient(x)
  @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) monic(x) else gcd1(y, monic(x.reduce(y)))
  extension (x: T) {
    override def reduce(m: M, a: C, y: T, b: C) = x.subtract(m, a / b, y)
    override def reduce(y: T) = super.reduce(x)(y)
  }
}
